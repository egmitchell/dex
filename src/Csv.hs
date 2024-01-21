{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Csv (Csv (..), CsvCell, writeCsvFile) where

import Control.DeepSeq
import Data.Char
import Data.List
import Data.List.Extra
import Data.Maybe

newtype CsvCell = CsvCell {unCsv :: String}
    deriving (NFData)

writeCsvFile :: FilePath -> [[(String, CsvCell)]] -> IO ()
writeCsvFile file xss = writeFile file $ unlines $ map (intercalate "," . map unCsv) $ map csv titles : map row xss
  where
    titles = nubOrd $ map fst $ concat xss
    row xs = [fromMaybe (CsvCell "") $ lookup t xs | t <- titles]

class Csv a where
    csv :: a -> CsvCell

instance Csv String where
    csv x
        | all isAlphaNum x = CsvCell x
        | otherwise = CsvCell $ "\"" ++ x ++ "\""

instance Csv Double where
    csv = CsvCell . show
