{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Labels (Label (..), defaultLabel, readFileLabels) where

import Data.List.Extra
import Data.Maybe
import System.IO
import Text.HTML.TagSoup

import Svg (Ident (..))

data Label = Label
    { lblLabel :: String
    -- ^ The label given to the node
    , lblTitle :: String
    -- ^ The title given to the node
    , lblDescription :: String
    -- ^ The description given to the node
    }
    deriving (Show)

defaultLabel :: Label
defaultLabel = Label "" "" ""

-- | Given an SVG file, produce a mapping between node identifiers and label information.
readFileLabels :: FilePath -> IO (Ident -> Label)
readFileLabels file = do
    tags <- parseTags <$> readFile' file
    let lbls = labels tags
    let descs = titleDesc tags
    return $ \(Ident x) ->
        let (lblTitle, lblDescription) = fromMaybe ("", "") $ lookup x descs
            lblLabel = fromMaybe "" $ lookup x lbls
         in Label{..}

labels :: [Tag String] -> [(String, String)]
labels xs = [(id, dropPrefix "#" lbl) | TagOpen _ at <- xs, Just id <- [lookup "id" at], Just lbl <- [lookup "inkscape:label" at]]

--
titleDesc :: [Tag String] -> [(String, (String, String))]
titleDesc (TagOpen _ at : rest)
    | Just id <- lookup "id" at =
        f ("", "") id rest
  where
    f (a, b) id (TagText _ : TagOpen s1 _ : TagText txt : TagClose s2 : rest)
        | s1 == s2
        , s1 `elem` ["desc", "title"] =
            f ((if s1 == "desc" then (a,) else (,b)) $ trim txt) id rest
    f ab id xs = (id, ab) : titleDesc xs
titleDesc (x : xs) = titleDesc xs
titleDesc [] = []
