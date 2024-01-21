{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- | A fossil is a set of shapes, all of which have an identifier, e.g. @photo1_sp75_part@.
  Each part for a given prefix is unique.
  Every fossil must have exactly one of a disc or a pt (point) from which label information is taken.
-}
module Fossil (Part (..), Info (..), Fossil (..), groupFossils) where

import Csv
import Data.List.Extra
import Data.Maybe
import Labels
import Svg

newtype Fossil = Fossil {unFossil :: String} deriving (Show, Eq, Ord, Csv)

data Part = FrondW | FrondL | Disc | Pt | Disc2 | StemW | StemL | Length1 | Length2 | Width1 | Width2 | Other String
    deriving (Show, Eq)

toPart :: String -> Part
toPart x = fromMaybe (Other x) $ lookup (lower x) builtin
  where
    builtin = ("ives", Disc) : [(lower $ show x, x) | x <- [FrondW, FrondL, Disc, Pt, Disc2, StemW, StemL, Length1, Length2, Width1, Width2]]

-- | Information derived from the Svg identifier, associated with a 'Shape'.
data Info = Info
    { infoFossil :: Fossil
    -- ^ The fossil this represents.
    , infoLabel :: Label
    -- ^ The label information associated with it.
    }
    deriving (Show)

-- | Given the identifier and its label, create the info.
info :: Ident -> Label -> (Info, Part)
info i@(Ident ident) label = case split (`elem` "-_") $ dropPrefix "sp" $ lower ident of
    [surface, specimen, part] -> (Info (Fossil $ surface ++ "_" ++ specimen) label, toPart part)
    _ -> error $ "Identifier must have exactly 3 _ separated components, got " ++ ident

groupByFossil :: [((Info, Part), a)] -> [(Fossil, [((Info, Part), a)])]
groupByFossil res = groupSort [(infoFossil $ fst i, (i, s)) | (i, s) <- res]

groupFossils :: (Ident -> Label) -> [(Ident, a)] ->  [(Fossil, [((Info, Part), a)])]
groupFossils getLabel shapes = groupByFossil [(info i $ getLabel i, a) | (i, a) <- shapes]
