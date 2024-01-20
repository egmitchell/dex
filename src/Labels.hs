{-# LANGUAGE TupleSections #-}

module Labels (Label (..), defaultLabel, labels, titleDesc) where

import Data.List.Extra
import Text.HTML.TagSoup

data Label = Label
    { lblLabel :: String
    , lblTitle :: String
    , lblDescription :: String
    }
    deriving (Show)

defaultLabel :: Label
defaultLabel = Label "" "" ""

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
