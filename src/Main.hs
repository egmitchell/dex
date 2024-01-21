module Main (main) where

import Control.DeepSeq
import Control.Exception.Extra
import Control.Monad
import Data.Either.Extra
import Data.List.Extra
import Data.Tuple.Extra
import Labels
import Svg
import System.Environment
import System.FilePath

main :: IO ()
main = do
    files <- getArgs
    when (null files) $ fail "Run with a list of SVG files to process"
    forM_ files $ \file -> do
        labels <- readFileLabels file
        let enrich i = i{infoLabel = labels $ infoId i}

        shapes <- readFileShapes file
        let res = map (first $ enrich . info) shapes
        let extraParts = nubOrd [x | Right x <- map (infoPart . fst) res, x `notElem` ["frondleft", "frondright"]]
        let ans = map (unroll extraParts) $ groupSort [(infoSurface i, (i, s)) | (i, s) <- res]
        (bad, good) <- fmap partitionEithers $ forM ans $ try_ . evaluate . force
        writeFile (dropExtension file ++ "_dex_ignored.txt") $ unlines $ map show bad
        let title =
                "Id,Label,Desc,DiscX,DiscY,DiscCx,DiscCy,DiscA,StemA,StemL,StemW,FrondA,FrondL,FrondW,Length1,Length2,Width1,Width2,Disc2Cx,Disc2Cy"
                    ++ concat ["," ++ x ++ "," ++ x ++ "A" | x <- extraParts]
        writeFile (dropExtension file ++ "_dex.csv") $ unlines $ title : good

unroll :: [String] -> (String, [(Info, Shape)]) -> String
unroll extraParts (surface, parts) =
    intercalate "," $
        show surface
            : show (lblLabel $ infoLabel i)
            : show (lblDescription $ infoLabel i)
            : map show [discX, discY, discRx, discRy, discA, angle StemL, f StemL, f StemW, angle FrondL, f FrondL, f FrondW, f Length1, f Length2, f Width1, f Width2, fst g, snd g]
            ++ concat [[show $ fAny $ Right x, show $ angleAny $ Right x] | x <- extraParts]
            ++ splitOn "-" (lblTitle $ infoLabel i)
  where
    err = errorWithoutStackTrace
    (i, discX, discY, discRx, discRy, discA) = case filter ((/= Left Disc2) . infoPart . fst) $ filter (isEllipse . snd) parts of
        [(i@Info{infoPart = Left Pt}, SEllipse (XY x y) _ _ _)] -> (i, x, y, 0, 0, 0)
        [(i@Info{infoPart = Left Disc}, SEllipse (XY x y) rx ry (XY xa ya))] -> (i, x, y, max rx ry * 2, min rx ry * 2, reangle $ atan ((xa - x) / (ya - y)))
        bad -> err $ "Wrong number of discs for " ++ surface ++ ", got " ++ show bad

    reangle radians = if v < 0 then v + 180 else v
      where
        v = radians / pi * 180

    f = fAny . Left
    fAny x = case [pathLength ps | (i, SPath ps) <- parts, infoPart i == x] of
        [] -> 0
        [x] -> x
        xs -> err $ "Wrong number of " ++ show x ++ " for " ++ surface ++ ", got " ++ show (length xs)

    g = head $ [(rx * 2, ry * 2) | (i, SEllipse _ rx ry _) <- parts, infoPart i == Left Disc2] ++ [(0, 0)]

    -- find either StemL if it exists, or FrondL if not
    angle = angleAny . Left
    angleAny typ = if null paths then 0 else angleXY (pathNorm !! 0) (pathNorm !! 1)
      where
        paths = [ps | (i, SPath ps) <- parts, infoPart i == typ]
        pathNorm = if distanceXY (last stemPath) (XY discX discY) < distanceXY (head stemPath) (XY discX discY) then reverse stemPath else stemPath
          where
            stemPath = head paths

-- writeFile "test.tsv" . unlines . reorder . map (intercalate "\t") . columns [] . map (render . parse) . process . dropDefs . parseTags =<< readFile "test.svg"

data Part = FrondW | FrondL | Disc | Pt | Disc2 | StemW | StemL | Length1 | Length2 | Width1 | Width2
    deriving (Enum, Bounded, Show, Eq)

toPart :: String -> Either Part String
toPart = \x -> maybe (Right x) Left $ lookup (lower x) xs
  where
    xs = ("ives", Disc) : [(lower $ show x, x) | x <- [minBound .. maxBound]]

data Info = Info
    { infoId :: Ident
    , infoSurface :: String
    , infoPart :: Either Part String
    , infoLabel :: Label
    }
    deriving (Show)

info :: Ident -> Info
info i@(Ident ident) = Info i (intercalate "_" $ take 2 parts) (toPart $ concat $ take 1 $ drop 2 parts) defaultLabel
  where
    parts = split (`elem` "-_") $ dropPrefix "sp" $ lower ident
