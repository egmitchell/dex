{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.DeepSeq
import Control.Exception.Extra
import Control.Monad
import Csv
import Data.Either.Extra
import Data.List.Extra (zipFrom)
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

        let res = groupFossils getLabel shapes
        let ans = map unroll res
        (bad, good) <- fmap partitionEithers $ forM ans $ try_ . evaluate . force
        writeFile (dropExtension file ++ "_dex_ignored.txt") $ unlines $ map show bad
        writeCsvFile (dropExtension file ++ "_dex.csv") good

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
        ++ angles "StemA" root StemL
        ++ f "StemW" (len StemW)
        ++ f "FrondL" (len FrondL)
        ++ angles "FrondA" stemOrRoot FrondL
        ++ f "FrontW" (len FrondW)
        ++ f "Length1" (len Length1)
        ++ f "Length2" (len Length2)
        ++ f "Width1" (len Width1)
        ++ f "Width2" (len Width2)
        ++ f "Disc2Cx" disc2Cx
        ++ f "Disc2Cy" disc2Cy
        ++ concat [f (show o) (len o) ++ angles (show o ++ "A") (parent o) o | (o@Branch{}, _) <- fosParts]
        ++ concat [f x (len o) ++ angles (x ++ "A") root o | (o@(Other x), _) <- fosParts]
  where
    root = pathFromPoint $ ellipseCentre $ snd $ fossilAnchor fossil
    stemOrRoot = fromMaybe root $ fossilPath fossil StemL
    parent (Branch lr n off)
        | null off = fromJust $ fossilPath fossil FrondL
        | otherwise = fromJust $ fossilPath fossil $ Branch lr n ""
    parent _ = error "parent on not a branch"

    f name x = [(name, csv x)]

    (XY discX discY, (discCx, discCy), discA) = case fossilAnchor fossil of
        (Pt, e) -> (ellipseCentre e, (0, 0), zeroAngle)
        (Disc, e) -> (ellipseCentre e, ellipseSize e, ellipseAngle e)
        _ -> error "fossilAnchor of wrong type"

    -- take the total length of the part
    len = maybe 0 pathLength . fossilPath fossil

    (disc2Cx, disc2Cy) = maybe (0, 0) ellipseSize $ fossilEllipse fossil Disc2

    -- take the angle of the path relative to north, using the end which is closest to the centre as the start
    angles lbl relative typ = case fossilPath fossil typ of
        Nothing -> []
        Just path -> concat [f (lbl ++ show i) a | (i, a) <- zipFrom 0 $ pathAngles $ snd $ pathsJoin relative path]
