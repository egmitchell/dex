{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.DeepSeq
import Control.Exception.Extra
import Control.Monad
import Csv
import Data.Either.Extra
import Data.List.Extra
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
        getLabel <- readFileLabels file

        shapes <- readFileShapes file
        let res = groupFossils getLabel shapes
        let extraParts = nubOrd [x | Other x <- map fst $ concatMap fosParts res, x `notElem` ["frondleft", "frondright"]]
        let ans = map (unroll extraParts) res
        (bad, good) <- fmap partitionEithers $ forM ans $ try_ . evaluate . force
        writeFile (dropExtension file ++ "_dex_ignored.txt") $ unlines $ map show bad
        let title =
                map csv $
                    splitOn "," $
                        "Id,Label,Desc,Title,DiscX,DiscY,DiscCx,DiscCy,DiscA,StemA,StemL,StemW,FrondA,FrondL,FrondW,Length1,Length2,Width1,Width2,Disc2Cx,Disc2Cy"
                            ++ concat ["," ++ x ++ "," ++ x ++ "A" | x <- extraParts]
        writeCsvFile (dropExtension file ++ "_dex.csv") $ title : good

unroll :: [String] -> Fossil -> [CsvCell]
unroll extraParts fossil@Fossil{fosLabel = Label{..}, ..} =
    csv fosName
        : csv lblLabel
        : csv lblDescription
        : csv lblTitle
        : map csv [discX, discY, discRx, discRy, discA, angle StemL, f StemL, f StemW, angle FrondL, f FrondL, f FrondW, f Length1, f Length2, f Width1, f Width2, fst g, snd g]
        ++ concat [[csv $ f $ Other x, csv $ angle $ Other x] | x <- extraParts]
  where
    (discX, discY, discRx, discRy, discA) = case fossilAnchor fossil of
        (Pt, AEllipse (XY x y) _ _ _) -> (x, y, 0, 0, 0)
        (Disc, AEllipse (XY x y) rx ry (XY xa ya)) -> (x, y, max rx ry * 2, min rx ry * 2, reangle $ atan ((xa - x) / (ya - y)))

    reangle radians = if v < 0 then v + 180 else v
      where
        v = radians / pi * 180

    f = maybe 0 pathLength . fossilPath fossil

    g = maybe (0, 0) discSize $ fossilEllipse fossil Disc2
      where
        discSize (AEllipse _ rx ry _) = (rx * 2, ry * 2)

    -- take the angle of the path relative to north, using the end which is closest to the centre as the start
    angle = maybe 0 pathAngle . fossilPath fossil
      where
        pathAngle (APath stemPath) = angleXY a b
          where
            (a : b : _) = if distanceXY (last stemPath) (XY discX discY) < distanceXY (head stemPath) (XY discX discY) then reverse stemPath else stemPath
