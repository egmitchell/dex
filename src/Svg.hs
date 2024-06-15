{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Svg (
    Ident (..),
    Shape (..),
    APath,
    AEllipse,
    XY (..),
    zeroAngle,
    readFileShapes,
    pathLength,
    pathAngles,
    pathPoints,
    pathStart,
    anglesBetween,
    ellipseCentre,
    ellipseSize,
    ellipseAngle,
    pathAlign,
    shapeNearestTo,
) where

import Csv
import Data.List.Extra
import Data.Ord
import Data.Tuple.Extra
import Numeric.Extra
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

newtype Length = Length Double deriving (Show, Csv)

zeroAngle :: Angle
zeroAngle = Angle 0

data Shape
    = SPath APath
    | SEllipse AEllipse
    deriving (Show)

newtype APath = APath [Segment] deriving (Show)

data Segment
    = Straight {segStart :: XY, segEnd :: XY}
    | Curve {segStart :: XY, controlBegin :: XY, controlEnd :: XY, segEnd :: XY}
    deriving (Show)

data AEllipse = AEllipse {
    ellipseCentre :: XY,
    ellipseRadiusX :: X,
    ellipseRaidusY :: Y,
    ellipseUnknown :: XY 
    } deriving (Show)

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
    PathTree x@Path{_pathDefinition=[MoveTo{}, EllipticalArc{}]} ->
        f (_pathDrawAttributes x) $ SEllipse $ aEllipseArc x
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

pointAt :: Double -> Segment -> XY
pointAt t (Straight (XY_ ax ay) (XY_ bx by)) = XY_ (ax + (bx - ax) * t) (ay + (by - ay) * t)
pointAt t (Curve p0 p1 p2 p3) = XY_ (f (\(XY_ x _) -> x)) (f (\(XY_ _ y) -> y))
    where
        -- from https://blog.maximeheckel.com/posts/cubic-bezier-from-math-to-motion/
        f p = (1-t)**3 * (p p0) + t*(p p1)*(3*(1-t)**2) + (p p2)*(3*(1-t)*t**2) + (p p3)*t**3

-- | We want at least 1/8th of the line to be involved, but don't want to make up points.
angleAt :: Double -> Segment -> Angle
angleAt _ (Straight a b) = angleXY a b
-- I expected this to be t2-bump `angleXY` t2+bump, but that experimentally is wrong?
angleAt t seg = angleXY (pointAt (t2 + bump) seg) (pointAt (t2 - bump) seg)
    where
        bump = 1/16
        t2 = clamp (bump, 1 - bump) t

-- | Produce fractions from 0..1, must include 0 and 1
fractions :: Int -> [Double]
fractions n = map (\i -> intToDouble i / intToDouble (n - 1)) [0..n-1]

-- | Given a point and a shape, find the 
shapeNearestTo :: XY -> Shape -> (Double, Length, Angle)
shapeNearestTo xy (SEllipse e) = (distanceXY xy $ ellipseCentre e, Length 0, Angle 180)
shapeNearestTo xy (SPath (APath xs)) = minimumOn fst3 $ map (first3 $ distanceXY xy) $ fragment 0 xs
    where
        fragment :: Double -> [Segment] -> [(XY, Length, Angle)]
        fragment len (seg:xs) = steps ++ fragment (len + dist) xs
            where
                steps = [(pointAt i seg, Length $ len + (dist * i), angleAt i seg) | i <- fractions 100]
                dist = distanceSegment seg
        fragment _ _ = []

-- | The angle of the ellipse, in degrees from north
ellipseAngle :: AEllipse -> Angle
ellipseAngle (AEllipse a _ _ b) = angleXY a b

anglesBetween :: [Angle] -> [Angle]
anglesBetween xs = zipWith angleDiff xs (tail xs)

angleDiff :: Angle -> Angle -> Angle
angleDiff (Angle a) (Angle b) = Angle $ if r < 0 then r + 360 else r
  where
    r = 180 - a + b

distanceSegment :: Segment -> Double
distanceSegment (Straight a b) = distanceXY a b
distanceSegment curve = sum $ zipWith distanceXY pts (tail pts)
    where pts = map (`pointAt` curve) $ fractions 20

-- | The length of a path by summing up all the individual lengths on the path
pathLength :: APath -> Double
pathLength (APath xs) = sum $ map distanceSegment xs

-- | Make sure the path starts as close to the shape
pathAlign :: Shape -> APath -> APath
pathAlign parent p
    | f (pathStart p) <= f (pathEnd p) = p
    | otherwise = pathReverse p
    where f x = fst3 $ shapeNearestTo x parent

pathStart :: APath -> XY
pathStart (APath xs) = segStart $ head xs

pathEnd :: APath -> XY
pathEnd (APath xs) = segEnd $ last xs

pathReverse :: APath -> APath
pathReverse (APath xs) = APath $ reverse $ map reverseSegment xs

reverseSegment :: Segment -> Segment
reverseSegment Straight{..} = Straight segEnd segStart
reverseSegment Curve{..} = Curve segEnd controlEnd controlBegin segStart

angleSegment :: Segment -> Angle
angleSegment (Straight a b) = angleXY a b
angleSegment c@(Curve a _ _ _) = angleXY a b
    where b = pointAt (1/8) c

-- | Find each successive angle in a path
pathAngles :: APath -> [Angle]
pathAngles (APath xs) = map angleSegment xs

pathPoints :: APath -> [XY]
pathPoints (APath xs) = segStart (head xs) : map segEnd xs

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
applyXY f (SPath (APath xs)) = SPath $ APath $ map g xs
    where
        g (Straight a b) = Straight (f a) (f b)
        g (Curve a b c d) = Curve (f a) (f b) (f c) (f d)
applyXY f (SEllipse (AEllipse xy x y a)) = SEllipse $ AEllipse (f xy) x y (f a) -- leave the radius untouched

transformations :: Shape -> [Transformation] -> Shape
transformations shp ts = applyXY (foldl (.) id $ map transformation $ reverse ts) shp

aPath :: Path -> APath
aPath Path{_pathDefinition = xs} = APath $ f Nothing xs
  where
    resolve OriginRelative (XY_ px py) (V2 x y) = XY_ (px+x) (py+y)
    resolve OriginAbsolute _ (V2 x y) = XY_ x y

    f prev (p : ps) = case p of
        LineTo r [xy@(V2 x y)] -> case prev of
            Nothing -> f (Just $ XY_ x y) ps
            Just prev -> let new = resolve r prev xy in Straight prev new : f (Just new) ps
        CurveTo r [(pb, pe, xy)] -> case prev of
            Nothing -> error "CurveTo without drawing anything"
            Just prev -> let new = resolve r prev xy in Curve prev (resolve r prev pb) (resolve r prev pe) new : f (Just new) ps

        LineTo r (x:xs) -> f prev $ LineTo r [x] : LineTo r xs : ps
        LineTo _ [] -> f prev ps
        CurveTo r (x:xs) -> f prev $ CurveTo r [x] : CurveTo r xs : ps
        CurveTo _ [] -> f prev ps
        MoveTo r xys -> f prev $ LineTo r xys : ps
        VerticalTo OriginRelative [y] -> f prev $ LineTo OriginRelative [V2 0 y] : ps
        VerticalTo OriginAbsolute [y] -> f prev $ LineTo OriginAbsolute [V2 (maybe 0 (\(XY_ x _) -> x) prev) y] : ps
        HorizontalTo OriginRelative [x] -> f prev $ LineTo OriginRelative [V2 x 0] : ps
        HorizontalTo OriginAbsolute [x] -> f prev $ LineTo OriginAbsolute [V2 x (maybe 0 (\(XY_ _ y) -> y) prev)] : ps
        EndPath -> f prev ps

        -- Throw away EllipticalArc information
        EllipticalArc r xs -> f prev (MoveTo r [x | (_, _, _, _, _, x) <- xs] : ps)

        _ -> error $ "Unknown line segment: " ++ show p
    f _ [] = []

aEllipseHelper :: XY -> X -> Y -> AEllipse
aEllipseHelper (XY_ x y) (X rx) (Y ry) =
    AEllipse (XY_ x y) (X rx) (Y ry) $
        if rx > ry then XY_ (x + rx) y else XY_ x (y + ry)

aEllipse :: Ellipse -> AEllipse
aEllipse Ellipse{_ellipseXRadius = Num rx, _ellipseYRadius = Num ry, _ellipseCenter = (Num x, Num y)} =
    aEllipseHelper (XY_ x y) (X rx) (Y ry)
aEllipse x = error $ "Ellipse of the type not normally produced by Inkscape, " ++ take 100 (show x)

aEllipseArc :: Path -> AEllipse
aEllipseArc Path{_pathDefinition=[MoveTo _ [V2 x0 y0],
    EllipticalArc r steps]}
    | steps <- if r == OriginRelative then absolute x0 y0 steps else steps
    , length steps == 4
    , [(rx, ry)] <- nubOrd [(rx,ry) | (rx,ry,_,_,_,_) <- steps]
    , let (xs,ys) = unzip [(x, y) | (_,_,_,_,_,V2 x y) <- steps]
    = aEllipseHelper (XY_ (mean xs) (mean ys)) (X rx) (Y ry)
    where
        mean xs = sum xs / genericLength xs
        absolute _ _ [] = []
        absolute x y ((a,b,c,d,e,V2 x2 y2):rest) =
            (a,b,c,d,e,V2 (x+x2) (y+y2)) :
            absolute (x+x2) (y+y2) rest
aEllipseArc x = error $ "Ellipse of the type not normally produced by Inkscape, " ++ take 100 (show x)
