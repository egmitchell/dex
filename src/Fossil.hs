{- | A fossil is a set of shapes, all of which have an identifier, e.g. @photo1_sp75_part@.
  Each part for a given prefix is unique.
  Every fossil must have exactly one of a disc or a pt (point) from which label information is taken.
-}
module Fossil (Part (..), Fossil (..), groupFossils, fossilPath, fossilEllipse) where

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
    _ -> error $ "Identifier must have exactly 3 _ separated components, got " ++ ident

groupFossils :: (Ident -> Label) -> [(Ident, Shape)] -> [Fossil]
groupFossils getLabel shapes = map f $ groupSort [(fossil, (part, (ident, shape))) | (ident, shape) <- shapes, let (fossil, part) = info ident]
  where
    f :: (String, [(Part, (Ident, Shape))]) -> Fossil
    f (fossil, parts)
        | dupes@(_ : _) <- duplicates $ map fst parts = error $ "Fossil " ++ fossil ++ " has duplicate parts for " ++ show dupes
        | otherwise = case catMaybes [lookup Pt parts, lookup Disc parts] of
            [(ident, _)] -> Fossil fossil (getLabel ident) $ map (\(p, (_, s)) -> (p, s)) parts
            xs -> error $ "Fossil " ++ fossil ++ " must have pt or disc, but has " ++ show (length xs) ++ " of them"

    duplicates :: (Ord a) => [a] -> [a]
    duplicates xs = [x | (x : _ : _) <- group $ sort xs]

fossilPath :: Fossil -> Part -> Maybe APath
fossilPath fos part = fmap f $ lookup part $ fosParts fos
  where
    f (SPath x) = x
    f x = error $ "Fossil " ++ fosName fos ++ " part " ++ show part ++ " expected to be a line, but got " ++ show x

fossilEllipse :: Fossil -> Part -> Maybe AEllipse
fossilEllipse fos part = fmap f $ lookup part $ fosParts fos
  where
    f (SEllipse x) = x
    f x = error $ "Fossil " ++ fosName fos ++ " part " ++ show part ++ " expected to be an ellipse, but got " ++ show x
