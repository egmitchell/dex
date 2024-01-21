{- | A fossil is a set of shapes, all of which have an identifier, e.g. @photo1_sp75_part@.
  Each part for a given prefix is unique.
  Every fossil must have exactly one of a disc or a pt (point) from which label information is taken.
-}
module Fossil (Part (..), Info (..), groupFossils) where

import Data.List.Extra
import Data.Maybe
import Labels
import Svg

data Part = FrondW | FrondL | Disc | Pt | Disc2 | StemW | StemL | Length1 | Length2 | Width1 | Width2 | Other String
    deriving (Show, Eq)

toPart :: String -> Part
toPart x = fromMaybe (Other x) $ lookup (lower x) builtin
  where
    builtin = ("ives", Disc) : [(lower $ show x, x) | x <- [FrondW, FrondL, Disc, Pt, Disc2, StemW, StemL, Length1, Length2, Width1, Width2]]

-- | Information derived from the Svg identifier, associated with a 'Shape'.
data Info = Info
    { infoFossil :: String
    -- ^ The fossil this represents.
    , infoLabel :: Label
    -- ^ The label information associated with it.
    }
    deriving (Show)

-- | Given the identifier and its label, create the info.
info :: Ident -> (String, Part)
info i@(Ident ident) = case split (`elem` "-_") $ dropPrefix "sp" $ lower ident of
    [surface, specimen, part] -> (surface ++ "_" ++ specimen, toPart part)
    _ -> error $ "Identifier must have exactly 3 _ separated components, got " ++ ident

groupFossils :: (Ident -> Label) -> [(Ident, shape)] -> [(Info, [(Part, shape)])]
groupFossils getLabel shapes = map f $ groupSort [(fossil, (part, (ident, shape))) | (ident, shape) <- shapes, let (fossil, part) = info ident]
  where
    f :: (String, [(Part, (Ident, shape))]) -> (Info, [(Part, shape)])
    f (fossil, parts) = case catMaybes [lookup Pt parts, lookup Disc parts] of
        [(ident, _)] -> (Info fossil $ getLabel ident, map (\(p, (_, s)) -> (p, s)) parts)
        xs -> error $ "Fossil " ++ fossil ++ " must have pt or disc, but has " ++ show (length xs) ++ " of them"
