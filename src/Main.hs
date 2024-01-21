module Main (main) where

import Control.DeepSeq
import Control.Exception.Extra
import Control.Monad
import Csv
import Data.Either.Extra
import Data.List.Extra
import Data.Tuple.Extra
import Fossil
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

        shapes <- readFileShapes file
        let res = map (first $ \id -> info id $ labels id) shapes
        let extraParts = nubOrd [x | Other x <- map (snd . fst) res, x `notElem` ["frondleft", "frondright"]]
        let ans = map (unroll extraParts) $ groupByFossil res
        (bad, good) <- fmap partitionEithers $ forM ans $ try_ . evaluate . force
        writeFile (dropExtension file ++ "_dex_ignored.txt") $ unlines $ map show bad
        let title =
                map csv $
                    splitOn "," $
                        "Id,Label,Desc,Title,DiscX,DiscY,DiscCx,DiscCy,DiscA,StemA,StemL,StemW,FrondA,FrondL,FrondW,Length1,Length2,Width1,Width2,Disc2Cx,Disc2Cy"
                            ++ concat ["," ++ x ++ "," ++ x ++ "A" | x <- extraParts]
        writeCsvFile (dropExtension file ++ "_dex.csv") $ title : good

unroll :: [String] -> (Fossil, [((Info, Part), Shape)]) -> [CsvCell]
unroll extraParts (fossil, parts) =
    csv fossil
        : csv (lblLabel $ infoLabel $ fst i)
        : csv (lblDescription $ infoLabel $ fst i)
        : csv (lblTitle $ infoLabel $ fst i)
        : map csv [discX, discY, discRx, discRy, discA, angle StemL, f StemL, f StemW, angle FrondL, f FrondL, f FrondW, f Length1, f Length2, f Width1, f Width2, fst g, snd g]
        ++ concat [[csv $ f $ Other x, csv $ angle $ Other x] | x <- extraParts]
  where
    err = errorWithoutStackTrace
    (i, discX, discY, discRx, discRy, discA) = case filter ((/= Disc2) . snd . fst) $ filter (isEllipse . snd) parts of
        [(i@(_, Pt), SEllipse (AEllipse (XY x y) _ _ _))] -> (i, x, y, 0, 0, 0)
        [(i@(_, Disc), SEllipse (AEllipse (XY x y) rx ry (XY xa ya)))] -> (i, x, y, max rx ry * 2, min rx ry * 2, reangle $ atan ((xa - x) / (ya - y)))
        bad -> err $ "Wrong number of discs for " ++ unFossil fossil ++ ", got " ++ show bad

    reangle radians = if v < 0 then v + 180 else v
      where
        v = radians / pi * 180

    f x = case [pathLength ps | (i, SPath ps) <- parts, snd i == x] of
        [] -> 0
        [x] -> x
        xs -> err $ "Wrong number of " ++ show x ++ " for " ++ unFossil fossil ++ ", got " ++ show (length xs)

    g = head $ [(rx * 2, ry * 2) | (i, SEllipse (AEllipse _ rx ry _)) <- parts, snd i == Disc2] ++ [(0, 0)]

    -- take the angle of the path relative to north, using the end which is closest to the centre as the start
    angle typ = if null paths then 0 else angleXY (pathNorm !! 0) (pathNorm !! 1)
      where
        paths = [ps | (i, SPath ps) <- parts, snd i == typ]
        pathNorm = if distanceXY (last stemPath) (XY discX discY) < distanceXY (head stemPath) (XY discX discY) then reverse stemPath else stemPath
          where
            APath stemPath = head paths
