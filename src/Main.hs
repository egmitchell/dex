{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.DeepSeq
import Control.Exception.Extra
import Control.Monad.Extra
import Csv
import Data.Either.Extra
import Data.List.Extra
import Data.Tuple.Extra
import Data.Maybe
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

        let (edge, res) = groupFossils getLabel shapes
        let ans = map unroll res
        (bad, good) <- fmap partitionEithers $ forM ans $ try_ . evaluate . force
        writeFile (dropExtension file ++ "_dex_ignored.txt") $ unlines $ map show bad
        writeCsvFile (dropExtension file ++ "_dex.csv") good
        writeFile (dropExtension file ++ "_raw.txt") $ unlines [a ++ "\t" ++ unCsv b | (a,b) <- concat good]
        whenJust edge $ \edge ->
            writeFile (dropExtension file ++ "_edge.csv") $ showEdgeCsv edge
        putStrLn $ file ++ " resulted in " ++ show (length bad) ++ " error(s)"

unroll :: Fossil -> [(String, CsvCell)]
unroll fossil@Fossil{fosLabel = Label{..}, ..} =
    f "Id" fosName
        ++ f "Label" lblLabel
        ++ f "Desc" lblDescription
        ++ f "Title" lblTitle
        ++ f "DiscX" discX
        ++ f "DiscY" discY
        ++ f "DiscCx" discCx
        ++ f "DiscCy" discCy
        ++ f "DistA" discA
        ++ f "StemL" (len StemL)
        ++ angles "StemA" StemL
        ++ f "StemW" (len StemW)
        ++ f "FrondL" (len FrondL)
        ++ angles "FrondA" FrondL
        ++ f "FrontW" (len FrondW)
        ++ f "Length1" length1
        ++ f "Length2" (len Length2)
        ++ f "Width1" (len Width1)
        ++ f "Width2" (len Width2)
        ++ f "Disc2Cx" disc2Cx
        ++ f "Disc2Cy" disc2Cy
        ++ f "EndX" (filament $ \(XY x _) -> x)
        ++ f "EndY" (filament $ \(XY _ y) -> y)
        ++ concat [f (show o) (len o) ++ distance (show o ++ "D") o ++ angles (show o ++ "A") o | (o@Branch{}, _) <- fosParts]
        ++ concat [f x (len o) ++ angles (x ++ "A") o | (o@(Other x), _) <- fosParts]
  where

    f name x = [(name, csv x)]

    (XY discX discY, (discCx, discCy), discA) = case fossilAnchor fossil of
        (Pt, e) -> (ellipseCentre e, (0, 0), zeroAngle)
        (Fil, e) -> (ellipseCentre e, (0, 0), zeroAngle)
        (Disc, e) -> (ellipseCentre e, ellipseSize e, ellipseAngle e)
        _ -> error "fossilAnchor of wrong type"

    -- take the total length of the part
    len = maybe 0 pathLength . fossilPath fossil

    filament xy = fmap (xy . pathEnd) $ fossilPath fossil Fil

    length1
        | Just x <- fossilPath fossil Length1 = pathLength x
        | Just x <- fossilPath fossil Fil = pathLength x
        | otherwise = 0

    (disc2Cx, disc2Cy) = maybe (0, 0) ellipseSize $ fossilEllipse fossil Disc2

    -- take the angle of the path relative to north, using the end which is closest to the centre as the start
    angles lbl typ = case fossilPath fossil typ of
        Nothing -> []
        Just path -> concat [f (lbl ++ show i) a | (i, a) <- zipFrom 0 $ anglesBetween $
            thd3 (shapeNearestTo (pathStart path) parent) : pathAngles path]
          where
            parent = fromJust $ lookup (fromJust $ partParent fossil typ) fosParts

    -- how far does this element start down its parent
    distance lbl typ = case fossilPath fossil typ of
        Nothing -> []
        Just path -> f lbl $ snd3 $ shapeNearestTo (pathStart path) parent
          where
            parent = fromJust $ lookup (fromJust $ partParent fossil typ) fosParts
