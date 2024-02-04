{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Svg (
    Ident (..),
    Shape (..),
    APath,
    AEllipse,
    XY (..),
    zeroAngle,
    readFileShapes,
    pathFromPoint,
    pathLength,
    pathsJoin,
    pathFinalAngle,
    pathAngles,
    anglesBetween,
    ellipseCentre,
    ellipseSize,
    ellipseAngle,
    shapeEnd,
    pathAlign,
) where

import Csv
import Data.List.Extra
import Data.Maybe
import Graphics.Svg
import Linear.V2

-- | An identifier (id) in an Svg file
newtype Ident = Ident String deriving (Show)

newtype X = X Double deriving (Show, Csv)
newtype Y = Y Double deriving (Show, Csv)
data XY = XY X Y deriving (Show)

{-# COMPLETE XY_ #-}
pattern XY_ :: Double -> Double -> XY
pattern XY_ x y = XY (X x) (Y y)

newtype Angle = Angle Double deriving (Show, Csv)

zeroAngle :: Angle
zeroAngle = Angle 0

data Shape
    = SPath APath
    | SEllipse AEllipse
    deriving (Show)

newtype APath = APath [XY] deriving (Show)

data AEllipse = AEllipse XY X Y XY deriving (Show)

pathFromPoint :: XY -> APath
pathFromPoint xy = APath [xy]

-- | The end point of a shape
shapeEnd :: Shape -> XY
shapeEnd (SPath (APath xs)) = last xs
shapeEnd (SEllipse x) = ellipseCentre x

readFileShapes :: FilePath -> IO [(Ident, Shape)]
readFileShapes file = do
    Just doc <- loadSvgFile file
    let Just (_, _, _, height) = _viewBox doc
    return $ concatMap (root [TransformMatrix 1 0 0 (-1) 0 height]) $ _elements doc

root :: [Transformation] -> Tree -> [(Ident, Shape)]
root ts x = case x of
    None -> []
    GroupTree x -> concatMap (root $ fromMaybe [] (_transform $ _groupDrawAttributes x) ++ ts) $ _groupChildren x
    CircleTree (Circle a b c) -> root ts $ EllipseTree $ Ellipse a b c c
    EllipseTree x -> f (_ellipseDrawAttributes x) $ SEllipse $ aEllipse x
    PathTree x -> f (_pathDrawAttributes x) $ SPath $ aPath x
    _ -> error $ "Unknown element: " ++ take 100 (show x)
  where
    f at shp = [(Ident $ fromMaybe "" $ _attrId at, transformations shp $ fromMaybe [] (_transform at) ++ ts)]

distanceXY :: XY -> XY -> Double
distanceXY (XY_ x1 y1) (XY_ x2 y2) = sqrt $ sqr (x2 - x1) + sqr (y2 - y1)
  where
    sqr x = x * x

angleXY :: XY -> XY -> Angle
angleXY (XY_ x1 y1) (XY_ x2 y2) = Angle $ if r < 0 then r + 360 else r
  where
    r = atan2 (x2 - x1) (y2 - y1) * 180 / pi

-- | The angle of the ellipse, in degrees from north
ellipseAngle :: AEllipse -> Angle
ellipseAngle (AEllipse a _ _ b) = angleXY a b

anglesBetween :: [Angle] -> [Angle]
anglesBetween xs = zipWith angleDiff xs (tail xs)

angleDiff :: Angle -> Angle -> Angle
angleDiff (Angle a) (Angle b) = Angle $ if r < 0 then r + 360 else r
  where
    r = b - a

-- | The length of a path by summing up all the individual lengths on the path
pathLength :: APath -> Double
pathLength (APath xs) = sum $ zipWith distanceXY (init xs) (tail xs)

-- | If necessary, reverse the paths so they join up with each other (as close as you can get)
pathsJoin :: APath -> APath -> (APath, APath)
pathsJoin as bs = minimumOn f [(as, bs), (as, r bs), (r as, bs), (r as, r bs)]
  where
    r (APath xs) = APath $ reverse xs
    f (APath as, APath bs) = distanceXY (last as) (head bs)

-- | Make sure the path starts as close to the point as we can
pathAlign :: XY -> APath -> APath
pathAlign start (APath xs)
    | distanceXY start (head xs) <= distanceXY start (last xs) = APath xs
    | otherwise = APath $ reverse xs

pathFinalAngle :: APath -> Angle
pathFinalAngle (APath xs) = case reverse xs of
    a : b : _ -> angleXY b a
    _ -> Angle 0

-- | Find each successive angle in a path
pathAngles :: APath -> [Angle]
pathAngles (APath xs) = zipWith angleXY xs (tail xs)

-- | The centre-point of an ellipse
ellipseCentre :: AEllipse -> XY
ellipseCentre (AEllipse a _ _ _) = a

-- | The size of an ellipse. The larger of the two will always be returned first
ellipseSize :: AEllipse -> (Double, Double)
ellipseSize (AEllipse _ (X rx) (Y ry) _) = (max rx ry * 2, min rx ry * 2)

transformation :: Transformation -> XY -> XY
transformation (TransformMatrix a b c d e f) (XY_ x y) = XY_ (a * x + c * y + e) (b * x + d * y + f)
transformation (Translate e f) (XY_ x y) = XY_ (x + e) (y + f)
transformation (Rotate a (fromMaybe (0, 0) -> (ox, oy))) (XY_ x y) =
    XY_
        (cos angle * (x - ox) - sin angle * (y - oy) + ox)
        (sin angle * (x - ox) + cos angle * (y - oy) + oy)
  where
    angle = a * (pi / 180) -- Convert to radians
transformation t _ = error $ "Unhandled transformation, " ++ show t

applyXY :: (XY -> XY) -> Shape -> Shape
applyXY f (SPath (APath xs)) = SPath $ APath $ map f xs
applyXY f (SEllipse (AEllipse xy x y a)) = SEllipse $ AEllipse (f xy) x y (f a) -- leave the radius untouched

transformations :: Shape -> [Transformation] -> Shape
transformations shp ts = applyXY (foldl (.) id $ map transformation $ reverse ts) shp

aPath :: Path -> APath
aPath Path{_pathDefinition = xs} = APath $ f (XY_ 0 0) xs
  where
    f (XY_ x y) (p : ps) = case p of
        MoveTo r (V2 x y : xys) -> go r x y $ LineTo r xys : ps
        LineTo r (V2 x y : xys) -> go r x y $ LineTo r xys : ps
        CurveTo r ((_, _, V2 x y) : xys) -> go r x y $ CurveTo r xys : ps
        LineTo _ [] -> f (XY_ x y) ps
        CurveTo _ [] -> f (XY_ x y) ps
        VerticalTo OriginRelative [y] -> f (XY_ x y) $ LineTo OriginRelative [V2 0 y] : ps
        EndPath -> f (XY_ x y) ps
        _ -> error $ "Unknown line segment: " ++ show p
      where
        go OriginAbsolute x y rest = XY_ x y : f (XY_ x y) rest
        go OriginRelative ((+ x) -> x) ((+ y) -> y) rest = XY_ x y : f (XY_ x y) rest
    f _ [] = []

aEllipse :: Ellipse -> AEllipse
aEllipse Ellipse{_ellipseXRadius = Num rx, _ellipseYRadius = Num ry, _ellipseCenter = (Num x, Num y)} =
    AEllipse (XY_ x y) (X rx) (Y ry) $
        if rx > ry then XY_ (x + rx) y else XY_ x (y + ry)
aEllipse x = error $ "Ellipse of the type not normally produced by Inkscape, " ++ take 100 (show x)
