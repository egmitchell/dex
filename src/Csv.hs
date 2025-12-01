{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Csv (Csv (..), CsvCell, unCsv, writeCsvFile) where

import Control.DeepSeq
import Data.Char
import Data.List.Extra
import Data.Maybe

newtype CsvCell = CsvCell {unCsv :: String}
    deriving (NFData, Show)

writeCsvFile :: FilePath -> [[(String, CsvCell)]] -> IO ()
writeCsvFile file xss = writeFile file $ unlines $ map (intercalate "," . map unCsv) $ map csv titles : map row xss
  where
    titles = nubOrd $ map fst $ concat xss
    -- For all missing values, we generate a 0, beacuse that makes downstream processing easier.
    row xs = [fromMaybe (CsvCell "0") $ lookup t xs | t <- titles]

class Csv a where
    csv :: a -> CsvCell

instance Csv String where
    csv x
        | all isAlphaNum x = CsvCell x
        | otherwise = CsvCell $ "\"" ++ x ++ "\""

instance Csv Double where
    csv = CsvCell . show

instance (Csv a) => Csv (Maybe a) where
    csv Nothing = CsvCell ""
    csv (Just x) = csv x
