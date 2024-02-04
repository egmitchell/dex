{- | A fossil is a set of shapes, all of which have an identifier, e.g. @photo1_sp75_part@.
  Each part for a given prefix is unique.
  Every fossil must have exactly one of a disc or a pt (point) from which label information is taken.
-}
{-# LANGUAGE NamedFieldPuns #-}
module Fossil (
    Part (..),
    LR (..),
    Fossil (..),
    groupFossils,
    fossilPath,
    fossilEllipse,
    fossilAnchor,
    fossilAlign,
    partParent,
) where

import Data.Char
import Data.List.Extra
import Data.Maybe
import Labels
import Svg

data LR = L | R deriving (Show, Eq, Ord)

data Part
    = FrondW
    | FrondL
    | Disc
    | Pt
    | Disc2
    | StemW
    | StemL
    | Length1
    | Length2
    | Width1
    | Width2
    | -- | A branch which is left/right, index along that branch.
      --   Null string means main branch, otherwise an offshoot of the given branch
      Branch LR Int String
    | Other String
    deriving (Eq, Ord)

instance Show Part where
    show x = case x of
        FrondW -> "FrondW"
        FrondL -> "FrondL"
        Disc -> "Disc"
        Pt -> "Pt"
        Disc2 -> "Disc2"
        StemW -> "StemW"
        StemL -> "StemL"
        Length1 -> "Length1"
        Length2 -> "Length2"
        Width1 -> "Width1"
        Width2 -> "Width2"
        Branch lr i x -> show lr ++ "B" ++ show i ++ x
        Other x -> x

toPart :: String -> Part
toPart x = case x of
    _ | Just v <- lookup (lower x) builtin -> v
    lr : 'B' : rest
        | lr `elem` "LR"
        , (a@(_ : _), b) <- span isDigit rest ->
            Branch (if lr == 'L' then L else R) (read a) b
    _ -> Other x
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
info (Ident ident) = case split (`elem` "-_") $ dropPrefix "sp" $ lower ident of
    [surface, specimen, part] -> (surface ++ "_" ++ specimen, toPart part)
    _ -> errorWithoutStackTrace $ "Identifier must have exactly 3 _ separated components, got " ++ ident

groupFossils :: (Ident -> Label) -> [(Ident, Shape)] -> [Fossil]
groupFossils getLabel shapes = map f $ groupSort [(fossil, (part, (ident, shape))) | (ident, shape) <- shapes, let (fossil, part) = info ident]
  where
    f :: (String, [(Part, (Ident, Shape))]) -> Fossil
    f (fossil, parts)
        | dupes@(_ : _) <- duplicates $ map fst parts = errorFossil fossil $ "has duplicate parts for " ++ show dupes
        | otherwise = Fossil fossil (getLabel $ fst $ snd $ lookupAnchor fossil parts) $ sortOn fst $ map (\(p, (_, s)) -> (p, s)) parts

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

-- | Look up a path in a fossil, return Nothing if not there and error if an ellipse
fossilPath :: Fossil -> Part -> Maybe APath
fossilPath fos part = fmap f $ lookup part $ fosParts fos
  where
    f (SPath x) = x
    f x = errorFossil (fosName fos) $ "part " ++ show part ++ " expected to be a line, but got " ++ show x

-- | Look up an ellipse in a fossil, return Nothing if not there and error if a path
fossilEllipse :: Fossil -> Part -> Maybe AEllipse
fossilEllipse fos part = fmap f $ lookup part $ fosParts fos
  where
    f (SEllipse x) = x
    f x = errorFossil (fosName fos) $ "part " ++ show part ++ " expected to be an ellipse, but got " ++ show x

errorFossil :: String -> String -> a
errorFossil fosName msg = errorWithoutStackTrace $ "Fossil " ++ fosName ++ ": " ++ msg

-- | Find the parent of a fossil part
partParent :: Fossil -> Part -> Maybe Part
partParent fos@Fossil{fosParts} part = case part of
    _ | part == root -> Nothing
    FrondL -> get StemL
    Branch _ _ "" -> get FrondL
    Branch lr n _ -> get $ Branch lr n ""
    _ -> Just root
    where
        get p = if isJust $ lookup p fosParts then Just p else partParent fos p
        root = fst $ fossilAnchor fos


-- | Make sure that all branches and elements start at the 
fossilAlign :: Fossil -> Fossil
fossilAlign fos@Fossil{fosParts} = fos{fosParts = align [] $ partialOrder parents}
    where
        parents = [(a, partParent fos a) | (a, _) <- fosParts]

        align done [] = reverse done
        align done (x:xs) = align ((x, next) : done) xs
            where
                next = case me of
                    SPath a | Just up <- up -> SPath $ pathAlign (shapeFinalPoint up) a
                    _ -> me

                me = fromJust $ lookup x fosParts
                up = do
                    p <- fromJust $ lookup x parents
                    Just $ fromJust $ lookup p fosParts


partialOrder :: Eq a => [(a, Maybe a)] -> [a]
partialOrder xs = f [] xs
    where
        f seen [] = seen
        f seen xs | null now = error "PartialOrder failed"
                  | otherwise = f (seen ++ map fst now) later
            where
                (now, later) = partition (maybe True (`elem` seen) . snd) xs
