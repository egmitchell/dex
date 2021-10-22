{-# LANGUAGE ViewPatterns, TupleSections #-}

import Text.HTML.TagSoup
import Control.Monad
import Control.DeepSeq
import Control.Exception.Extra
import Data.Maybe
import Data.Either.Extra
import Data.List.Extra
import System.Environment
import System.FilePath
import qualified Data.Map as Map
import Graphics.Svg
import Linear.V2


main :: IO ()
main = do
    files <- getArgs
    when (null files) $ fail "Run with a list of SVG files to process"
    forM_ files $ \file -> do
        tags <- parseTags <$> readFile file
        let lbls = labels tags
        let descs = titleDesc tags
        let enrich i = i{infoTitle = maybe "" fst $ lookup (infoId i) descs
                        ,infoDesc  = maybe "" snd $ lookup (infoId i) descs
                        ,infoLabel = fromMaybe "" $ lookup (infoId i) lbls}

        Just doc <- loadSvgFile file
        let Just (_, _, _, height) = _viewBox doc
        let res = concatMap (root [TransformMatrix 1 0 0 (-1) 0 height]) $ _elements doc
        let ignored = ["Unknown element " ++ infoId x | (x,_) <- res, isNothing $ infoPart x]
        let ans = map unroll $ groupSort [(infoSurface i, (enrich i,s)) | (i,s) <- res, isJust $ infoPart i]
        (bad, good) <- fmap partitionEithers $ forM ans $ try_ . evaluate . force
        writeFile (dropExtension file ++ "_dex_ignored.txt") $ unlines $ ignored ++ map show bad
        writeFile (dropExtension file ++ "_dex.csv") $ unlines $ "Id,Label,Desc,DiscX,DiscY,DiscCx,DiscCy,DiscA,StemA,StemL,StemW,FrondA,FrondL,FrondW,Length1,Length2,Width1,Width2,Disc2Cx,Disc2Cy" : good

unroll :: (String, [(Info, Shape)]) -> String
unroll (surface, parts) = intercalate "," $
        surface :infoLabel i : infoDesc i :
        map show [discX,discY,discRx,discRy,discA,angle StemL,f StemL,f StemW,angle FrondL,f FrondL,f FrondW,f Length1,f Length2,f Width1,f Width2,fst g,snd g] ++
        splitOn "-" (infoTitle i)
    where
        err = errorWithoutStackTrace
        (i,discX,discY,discRx,discRy,discA) = case filter ((/= Just Disc2) . infoPart . fst) $ filter (isEllipse . snd) parts of
            [(i@Info{infoPart=Just Pt},SEllipse (x,y) _ _ _)] -> (i,x,y,0,0,0)
            [(i@Info{infoPart=Just Disc},SEllipse (x,y) rx ry (xa,ya))] -> (i,x,y,rx*2,ry*2,reangle $ atan ((xa-x) / (ya-y)))
            bad -> err $ "Wrong number of discs for " ++ surface ++ ", got " ++ show bad

        reangle radians = if v < 0 then v + 180 else v
            where v = radians / pi * 180

        f x = case [pathLength ps | (i,SPath ps) <- parts, infoPart i == Just x] of
            [] -> 0
            [x] -> x
            xs -> err $ "Wrong number of " ++ show x ++ " for " ++ surface ++ ", got " ++ show (length xs)

        g = head $ [(rx*2, ry*2) | (i, SEllipse _ rx ry _) <- parts, infoPart i == Just Disc2] ++ [(0,0)]

        -- find either StemL if it exists, or FrondL if not
        angle typ = if null paths then 0 else angleXY (pathNorm !! 0) (pathNorm !! 1)
            where
                paths = [ps | (i, SPath ps) <- parts, infoPart i == Just typ]
                pathNorm = if distanceXY (last stemPath) (discX,discY) < distanceXY (head stemPath) (discX,discY) then reverse stemPath else stemPath
                    where stemPath = head paths



    -- writeFile "test.tsv" . unlines . reorder . map (intercalate "\t") . columns [] . map (render . parse) . process . dropDefs . parseTags =<< readFile "test.svg"

type X = Double
type Y = Double
type XY = (X, Y)

data Part = Disc | Pt | Disc2 | StemW | StemL | FrondW | FrondL | Length1 | Length2 | Width1 | Width2
    deriving (Enum,Bounded,Show,Eq)

toPart :: String -> Maybe Part
toPart = \x -> lookup (lower x) xs
    where xs = ("ives",Disc) : [(lower $ show x, x) | x <- [minBound..maxBound]]

data Info = Info
    {infoId :: String
    ,infoSurface :: String
    ,infoPart :: Maybe Part
    ,infoLabel :: String
    ,infoTitle :: String
    ,infoDesc :: String
    }
    deriving Show

data Shape
    = SPath [XY]
    | SEllipse XY X Y XY
    | SUnknown String
      deriving Show

isEllipse SEllipse{} = True; isEllipse _ = False

distanceXY :: XY -> XY -> Double
distanceXY (x1,y1) (x2,y2) = sqrt $ sqr (x2-x1) + sqr (y2-y1)
    where sqr x = x * x

angleXY :: XY -> XY -> Double
angleXY (x1,y1) (x2,y2) = if r < 0 then r + 360 else r
    where r = atan2 (x2-x1) (y2-y1) * 180 / pi

pathLength :: [XY] -> Double
pathLength xs = sum $ zipWith distanceXY (init xs) (tail xs)

transformation :: Transformation -> XY -> XY
transformation (TransformMatrix a b c d e f) (x, y) = (a*x + c*y + e, b*x + d*y + f)
transformation (Translate e f) (x, y) = (x + e, y + f)
transformation (Rotate a (fromMaybe (0,0) -> (ox,oy))) (x, y) =
    (cos angle * (x - ox) - sin angle * (y - oy) + ox
    ,sin angle * (x - ox) + cos angle * (y - oy) + oy)
    where angle = a * (pi / 180) -- Convert to radians
transformation t _ = error $ "Unhandled transformation, " ++ show t

applyXY :: (XY -> XY) -> Shape -> Shape
applyXY f (SPath xs) = SPath $ map f xs
applyXY f (SEllipse xy x y a) = SEllipse (f xy) x y (f a) -- leave the radius untouched
applyXY f (SUnknown x) = SUnknown x

transformations :: Shape -> [Transformation] -> Shape
transformations shp ts = applyXY (foldl (.) id $ map transformation $ reverse ts) shp

asLine :: Path -> Shape
asLine Path{_pathDefinition = xs} = SPath $ f (0,0) xs
    where
        f (x,y) (p:ps) = case p of
            MoveTo r (V2 x y:xys) -> go r x y $ LineTo r xys : ps
            LineTo r (V2 x y:xys) -> go r x y $ LineTo r xys : ps
            CurveTo r ((_,_,V2 x y):xys) -> go r x y $ CurveTo r xys : ps
            LineTo r [] -> f (x,y) ps
            CurveTo r [] -> f (x,y) ps
            EndPath -> f (x,y) ps
            where go OriginAbsolute x y rest = (x,y) : f (x,y) rest
                  go OriginRelative ((+x) -> x) ((+y) -> y) rest = (x,y) : f (x,y) rest
        f _ [] = []

asRound :: Ellipse -> Shape
asRound Ellipse{_ellipseXRadius=Num rx, _ellipseYRadius=Num ry, _ellipseCenter=(Num x, Num y)} =
    SEllipse (x,y) rx ry $
        if rx > ry then (x+rx, y) else (x, y+ry)

info :: DrawAttributes -> Info
info x = Info ident (intercalate "_" $ take 2 parts) (toPart $ concat $ take 1 $ drop 2 parts) "" "" ""
    where ident = fromMaybe "" $ _attrId x
          parts = split (`elem` "-_") $ dropPrefix "sp" $ lower ident

root :: [Transformation] -> Tree -> [(Info, Shape)]
root ts x = case x of
    None -> []
    GroupTree x -> concatMap (root $ fromMaybe [] (_transform $ _groupDrawAttributes x) ++ ts) $ _groupChildren x
    CircleTree (Circle a b c) -> root ts $ EllipseTree $ Ellipse a b c c
    EllipseTree x -> f (_ellipseDrawAttributes x) $ asRound x
    PathTree x -> f (_pathDrawAttributes x) $ asLine x
    _ -> error $ "Unknown element: " ++ take 100 (show x)
    where f at shp = [(info at, transformations shp $ fromMaybe [] (_transform at) ++ ts)]



labels :: [Tag String] -> [(String, String)]
labels xs = [(id, dropPrefix "#" $ lbl) | TagOpen _ at <- xs, Just id <- [lookup "id" at], Just lbl <- [lookup "inkscape:label" at]]

--
titleDesc :: [Tag String] -> [(String, (String, String))]
titleDesc (TagOpen _ at:rest)
    | Just id <- lookup "id" at
    = f ("","") id rest
    where
        f (a,b) id (TagText _:TagOpen s1 _:TagText txt:TagClose s2:rest) | s1 == s2, s1 `elem` ["desc","title"] =
            f ((if s1 == "desc" then (a,) else (,b)) $ trim txt) id rest
        f ab id xs = (id,ab) : titleDesc xs
titleDesc (x:xs) = titleDesc xs
titleDesc [] = []


{-

reorder (unsnoc -> Just (xs, x)) = x:xs


dropDefs (TagOpen "defs" _:xs) | _:xs <- dropWhile (/= TagClose "defs") xs = dropDefs xs
dropDefs (x:xs) = x : dropDefs xs
dropDefs [] = []


columns :: [String] -> [[(String, String)]] -> [[String]]
columns ord [] = [ord]
columns ord (x:xs) = dropWhileEnd null [fromMaybe "" $ lookup o x | o <- ord2] : columns ord2 xs
    where ord2 = nubOrd $ ord ++ map fst x


process :: [Tag String] -> [[(String, String)]]
process (TagOpen t at:xs) | t `elem` ["ellipse","path","circle"] = (("type",t):at++desc xs) : process xs
    where
        desc (TagText _:xs) = desc xs
        desc (TagOpen "desc" _:TagText s:_) = [("description",s)]
        desc _ = []
process (x:xs) = process xs
process [] = []

data Obj = Ellipse {id, species, desc :: String, x, y, rx, ry :: Double}
         | Path {id :: String, x1, y1, x2, y2 :: Double}
         | Ignored String
         | Broken String
    deriving Show

parse :: [(String, String)] -> Obj
parse xs = case str "type" of
    "ellipse"
        | def "none" "clip-path" == "none"
        -> Ellipse (str "id") (drop 1 $ def "" "inkscape:label") (def "" "description") x2 y2 rx ry
            where
                rx = dbl "rx"; ry = dbl "ry"
                x = dbl "cx" {-  + rx/2 -}; y = dbl "cy" {- + ry/2 -}
                t = parseTransform $ def "" "transform"
                (x2, y2) = applyTransform t (x, y)
    "path"
        | def "none" "clip-path" == "none"
        , not $ "path" `isPrefixOf` str "id"
        -> if x1 == 0 && y1 == 0 && x2 == 0 && y2 == 0 then Broken (str "id") else Path (str "id") x1 y1 x2 y2
            where
                pair s = case map read $ splitOn "," s of [x,y] -> (x,y)
                trans = applyTransform $ parseTransform $ def "" "transform"
                plus (x1,y1) (x2,y2) = (x1+x2, y1+y2)

                ((x1,y1),(x2,y2)) = case words $ str "d" of
                    ["M",pair -> c1, pair -> c2] -> (trans c1, trans c2)
                    ["m",pair -> c1, pair -> c2] -> (trans c1, trans $ c1 `plus` c2)
                    ["m",pair -> c1, "c", _, _, pair -> c2] -> (trans c1, trans $ c1 `plus` c2)
                    ["M",pair -> c1, "C", _, _, pair -> c2] -> (trans c1, trans c2)
                    _ -> ((0,0),(0,0)) -- FIXME!!!

    "path" | let i = str "id", "path" `isPrefixOf` i -> Ignored i

    "circle" -> parse $ [("type","ellipse"),("rx",str "r"),("ry",str "r")] ++ xs

    _ -> error $ show xs
    where
        str x = fromJust $ lookup x xs
        def a x = fromMaybe a $ lookup x xs
        dbl = read . str


-- see https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/transform
applyTransform :: [Double] -> (Double, Double) -> (Double, Double)
applyTransform [a,b,c,d,e,f] (x,y) = (a*x + c*y + e, b*x + d*y + f)

parseTransform :: String -> [Double]
parseTransform "" = [1,0,0,1,0,0]
parseTransform x | Just x <- stripPrefix "matrix(" x, Just x <- stripSuffix ")" x = map read $ splitOn "," x
parseTransform x | Just x <- stripPrefix "scale(" x, Just x <- stripSuffix ")" x, [x,y] <- map read $ splitOn "," x = [x,0,0,y,0,0]
parseTransform x = error $ show ("transform", x)

render :: Obj -> [(String, String)]
render x = [("Obj", show x)]
-}
