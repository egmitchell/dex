{-# LANGUAGE ViewPatterns #-}

module Svg (
    Shape (..),
    isEllipse,
    readFileShapes,
    pathLength,
    angleXY,
    distanceXY,
) where

import Data.Maybe
import Graphics.Svg
import Linear.V2

type X = Double
type Y = Double
type XY = (X, Y)

data Shape
    = SPath [XY]
    | SEllipse XY X Y XY
    deriving (Show)

isEllipse :: Shape -> Bool
isEllipse SEllipse{} = True
isEllipse _ = False

readFileShapes :: FilePath -> IO [(String, Shape)]
readFileShapes file = do
    Just doc <- loadSvgFile file
    let Just (_, _, _, height) = _viewBox doc
    return $ concatMap (root [TransformMatrix 1 0 0 (-1) 0 height]) $ _elements doc

root :: [Transformation] -> Tree -> [(String, Shape)]
root ts x = case x of
    None -> []
    GroupTree x -> concatMap (root $ fromMaybe [] (_transform $ _groupDrawAttributes x) ++ ts) $ _groupChildren x
    CircleTree (Circle a b c) -> root ts $ EllipseTree $ Ellipse a b c c
    EllipseTree x -> f (_ellipseDrawAttributes x) $ asRound x
    PathTree x -> f (_pathDrawAttributes x) $ asLine x
    _ -> error $ "Unknown element: " ++ take 100 (show x)
  where
    f at shp = [(fromMaybe "" $ _attrId at, transformations shp $ fromMaybe [] (_transform at) ++ ts)]

distanceXY :: XY -> XY -> Double
distanceXY (x1, y1) (x2, y2) = sqrt $ sqr (x2 - x1) + sqr (y2 - y1)
  where
    sqr x = x * x

angleXY :: XY -> XY -> Double
angleXY (x1, y1) (x2, y2) = if r < 0 then r + 360 else r
  where
    r = atan2 (x2 - x1) (y2 - y1) * 180 / pi

pathLength :: [XY] -> Double
pathLength xs = sum $ zipWith distanceXY (init xs) (tail xs)

transformation :: Transformation -> XY -> XY
transformation (TransformMatrix a b c d e f) (x, y) = (a * x + c * y + e, b * x + d * y + f)
transformation (Translate e f) (x, y) = (x + e, y + f)
transformation (Rotate a (fromMaybe (0, 0) -> (ox, oy))) (x, y) =
    ( cos angle * (x - ox) - sin angle * (y - oy) + ox
    , sin angle * (x - ox) + cos angle * (y - oy) + oy
    )
  where
    angle = a * (pi / 180) -- Convert to radians
transformation t _ = error $ "Unhandled transformation, " ++ show t

applyXY :: (XY -> XY) -> Shape -> Shape
applyXY f (SPath xs) = SPath $ map f xs
applyXY f (SEllipse xy x y a) = SEllipse (f xy) x y (f a) -- leave the radius untouched

transformations :: Shape -> [Transformation] -> Shape
transformations shp ts = applyXY (foldl (.) id $ map transformation $ reverse ts) shp

asLine :: Path -> Shape
asLine Path{_pathDefinition = xs} = SPath $ f (0, 0) xs
  where
    f (x, y) (p : ps) = case p of
        MoveTo r (V2 x y : xys) -> go r x y $ LineTo r xys : ps
        LineTo r (V2 x y : xys) -> go r x y $ LineTo r xys : ps
        CurveTo r ((_, _, V2 x y) : xys) -> go r x y $ CurveTo r xys : ps
        LineTo r [] -> f (x, y) ps
        CurveTo r [] -> f (x, y) ps
        VerticalTo OriginRelative [y] -> f (x, y) $ LineTo OriginRelative [V2 0 y] : ps
        EndPath -> f (x, y) ps
        _ -> error $ "Unknown line segment: " ++ show p
      where
        go OriginAbsolute x y rest = (x, y) : f (x, y) rest
        go OriginRelative ((+ x) -> x) ((+ y) -> y) rest = (x, y) : f (x, y) rest
    f _ [] = []

asRound :: Ellipse -> Shape
asRound Ellipse{_ellipseXRadius = Num rx, _ellipseYRadius = Num ry, _ellipseCenter = (Num x, Num y)} =
    SEllipse (x, y) rx ry $
        if rx > ry then (x + rx, y) else (x, y + ry)
