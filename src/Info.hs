module Info (Part (..), Info (..), info, Fossil (..), groupByFossil) where

import Data.List.Extra
import Data.Maybe
import Labels
import Svg

newtype Fossil = Fossil {unFossil :: String} deriving (Show, Eq, Ord)

data Part = FrondW | FrondL | Disc | Pt | Disc2 | StemW | StemL | Length1 | Length2 | Width1 | Width2 | Other String
    deriving (Show, Eq)

toPart :: String -> Part
toPart x = fromMaybe (Other x) $ lookup (lower x) builtin
  where
    builtin = ("ives", Disc) : [(lower $ show x, x) | x <- [FrondW, FrondL, Disc, Pt, Disc2, StemW, StemL, Length1, Length2, Width1, Width2]]

-- | Information derived from the Svg identifier, associated with a 'Shape'.
data Info = Info
    { infoId :: Ident
    -- ^ The Svg identifier.
    , infoFossil :: Fossil
    -- ^ The fossil this represents.
    , infoPart :: Part
    -- ^ The part of the fossil this shape applies to.
    , infoLabel :: Label
    -- ^ The label information associated with it.
    }
    deriving (Show)

-- | Given the identifier and its label, create the info.
info :: Ident -> Label -> Info
info i@(Ident ident) = Info i (Fossil $ intercalate "_" $ take 2 parts) (toPart $ concat $ take 1 $ drop 2 parts)
  where
    parts = split (`elem` "-_") $ dropPrefix "sp" $ lower ident

groupByFossil :: [(Info, a)] -> [(Fossil, [(Info, a)])]
groupByFossil res = groupSort [(infoFossil i, (i, s)) | (i, s) <- res]
