{-# LANGUAGE ViewPatterns, TupleSections #-}

module Main(main) where

import Text.HTML.TagSoup
import Control.Monad
import Control.DeepSeq
import Control.Exception.Extra
import Data.Maybe
import Data.Either.Extra
import Data.List.Extra
import Data.Tuple.Extra
import System.Environment
import System.FilePath
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
        let res = map (first enrich) $ concatMap (root [TransformMatrix 1 0 0 (-1) 0 height]) $ _elements doc
        let extraParts = nubOrd [x | Right x <- map (infoPart . fst) res, x `notElem` ["frondleft", "frondright"]]
        let ans = map (unroll extraParts) $ groupSort [(infoSurface i, (i,s)) | (i,s) <- res]
        (bad, good) <- fmap partitionEithers $ forM ans $ try_ . evaluate . force
        writeFile (dropExtension file ++ "_dex_ignored.txt") $ unlines $ map show bad
        let title =
                "Id,Label,Desc,DiscX,DiscY,DiscCx,DiscCy,DiscA,StemA,StemL,StemW,FrondA,FrondL,FrondW,Length1,Length2,Width1,Width2,Disc2Cx,Disc2Cy" ++
                concat ["," ++ x ++ "," ++ x ++ "A" | x <- extraParts]
        writeFile (dropExtension file ++ "_dex.csv") $ unlines $ title : good

unroll :: [String] -> (String, [(Info, Shape)]) -> String
unroll extraParts (surface, parts) = intercalate "," $
        show surface : show (infoLabel i) : show (infoDesc i) :
        map show [discX,discY,discRx,discRy,discA,angle StemL,f StemL,f StemW,angle FrondL,f FrondL,f FrondW,f Length1,f Length2,f Width1,f Width2,fst g,snd g] ++
        concat [[show $ fAny $ Right x, show $ angleAny $ Right x] | x <- extraParts] ++
        splitOn "-" (infoTitle i)
    where
        err = errorWithoutStackTrace
        (i,discX,discY,discRx,discRy,discA) = case filter ((/= Left Disc2) . infoPart . fst) $ filter (isEllipse . snd) parts of
            [(i@Info{infoPart=Left Pt},SEllipse (x,y) _ _ _)] -> (i,x,y,0,0,0)
            [(i@Info{infoPart=Left Disc},SEllipse (x,y) rx ry (xa,ya))] -> (i,x,y,max rx ry * 2,min rx ry * 2,reangle $ atan ((xa-x) / (ya-y)))
            bad -> err $ "Wrong number of discs for " ++ surface ++ ", got " ++ show bad

        reangle radians = if v < 0 then v + 180 else v
            where v = radians / pi * 180

        f = fAny . Left
        fAny x = case [pathLength ps | (i,SPath ps) <- parts, infoPart i == x] of
            [] -> 0
            [x] -> x
            xs -> err $ "Wrong number of " ++ show x ++ " for " ++ surface ++ ", got " ++ show (length xs)

        g = head $ [(rx*2, ry*2) | (i, SEllipse _ rx ry _) <- parts, infoPart i == Left Disc2] ++ [(0,0)]

        -- find either StemL if it exists, or FrondL if not
        angle = angleAny . Left
        angleAny typ = if null paths then 0 else angleXY (pathNorm !! 0) (pathNorm !! 1)
            where
                paths = [ps | (i, SPath ps) <- parts, infoPart i == typ]
                pathNorm = if distanceXY (last stemPath) (discX,discY) < distanceXY (head stemPath) (discX,discY) then reverse stemPath else stemPath
                    where stemPath = head paths



    -- writeFile "test.tsv" . unlines . reorder . map (intercalate "\t") . columns [] . map (render . parse) . process . dropDefs . parseTags =<< readFile "test.svg"

type X = Double
type Y = Double
type XY = (X, Y)


data Part = FrondW | FrondL |  Disc | Pt | Disc2 | StemW | StemL | Length1 | Length2 | Width1 | Width2
    deriving (Enum,Bounded,Show,Eq)

toPart :: String -> Either Part String
toPart = \x -> maybe (Right x) Left $ lookup (lower x) xs
    where xs = ("ives",Disc) : [(lower $ show x, x) | x <- [minBound..maxBound]]

data Info = Info
    {infoId :: String
    ,infoSurface :: String
    ,infoPart :: Either Part String
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
            VerticalTo OriginRelative [y] -> f (x,y) $ LineTo OriginRelative [V2 0 y] : ps
            EndPath -> f (x,y) ps
            _ -> error $ "Unknown line segment: " ++ show p
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
