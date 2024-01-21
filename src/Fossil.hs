{- | A fossil is a set of shapes, all of which have an identifier, e.g. @photo1_sp75_part@.
  Each part for a given prefix is unique.
  Every fossil must have exactly one of a disc or a pt (point) from which label information is taken.
-}
module Fossil (Part (..), Fossil (..), groupFossils, fossilPath, fossilEllipse, fossilAnchor) where

import Data.List.Extra
import Data.Maybe
import Labels
import Svg

data Part = FrondW | FrondL | Disc | Pt | Disc2 | StemW | StemL | Length1 | Length2 | Width1 | Width2 | Other String
    deriving (Show, Eq, Ord)

toPart :: String -> Part
toPart x = fromMaybe (Other x) $ lookup (lower x) builtin
  where
    builtin = ("ives", Disc) : [(lower $ show x, x) | x <- [FrondW, FrondL, Disc, Pt, Disc2, StemW, StemL, Length1, Length2, Width1, Width2]]

-- | Information derived from the Svg identifier, associated with a 'Shape'.
data Fossil = Fossil
    { fosName :: String
    -- ^ The fossil this represents.
    , fosLabel :: Label
    -- ^ The label information associated with it.
    , fosParts :: [(Part, Shape)]
    -- ^ The parts associated with the fossil, each part is unique
    }
    deriving (Show)

-- | Given the identifier and its label, create the info.
info :: Ident -> (String, Part)
info i@(Ident ident) = case split (`elem` "-_") $ dropPrefix "sp" $ lower ident of
    [surface, specimen, part] -> (surface ++ "_" ++ specimen, toPart part)
    _ -> errorWithoutStackTrace $ "Identifier must have exactly 3 _ separated components, got " ++ ident

groupFossils :: (Ident -> Label) -> [(Ident, Shape)] -> [Fossil]
groupFossils getLabel shapes = map f $ groupSort [(fossil, (part, (ident, shape))) | (ident, shape) <- shapes, let (fossil, part) = info ident]
  where
    f :: (String, [(Part, (Ident, Shape))]) -> Fossil
    f (fossil, parts)
        | dupes@(_ : _) <- duplicates $ map fst parts = errorFossil fossil $ "has duplicate parts for " ++ show dupes
        | otherwise = Fossil fossil (getLabel $ fst $ snd $ lookupAnchor fossil parts) $ map (\(p, (_, s)) -> (p, s)) parts

    duplicates :: (Ord a) => [a] -> [a]
    duplicates xs = [x | (x : _ : _) <- group $ sort xs]

lookupAnchor :: String -> [(Part, a)] -> (Part, a)
lookupAnchor fosName parts = case (lookup Pt parts, lookup Disc parts) of
    (Just x, Nothing) -> (Pt, x)
    (Nothing, Just x) -> (Disc, x)
    (a, b) -> errorFossil fosName $ "must have pt or disc, but has " ++ show (length $ catMaybes [a, b]) ++ " of them"

-- | Find the anchor for this fossil, must be either a Pt or Disc.
fossilAnchor :: Fossil -> (Part, AEllipse)
fossilAnchor fos = (part, fromJust $ fossilEllipse fos part)
  where
    (part, _) = lookupAnchor (fosName fos) (fosParts fos)

fossilPath :: Fossil -> Part -> Maybe APath
fossilPath fos part = fmap f $ lookup part $ fosParts fos
  where
    f (SPath x) = x
    f x = errorFossil (fosName fos) $ "part " ++ show part ++ " expected to be a line, but got " ++ show x

fossilEllipse :: Fossil -> Part -> Maybe AEllipse
fossilEllipse fos part = fmap f $ lookup part $ fosParts fos
  where
    f (SEllipse x) = x
    f x = errorFossil (fosName fos) $ "part " ++ show part ++ " expected to be an ellipse, but got " ++ show x

errorFossil :: String -> String -> a
errorFossil fosName msg = errorWithoutStackTrace $ "Fossil " ++ fosName ++ ": " ++ msg
