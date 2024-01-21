{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Csv (Csv (..), CsvCell, writeCsvFile) where

import Control.DeepSeq
import Data.Char
import Data.List

newtype CsvCell = CsvCell {unCsv :: String}
    deriving (NFData)

writeCsvFile :: FilePath -> [[CsvCell]] -> IO ()
writeCsvFile file = writeFile file . unlines . map (intercalate "," . map unCsv)

class Csv a where
    csv :: a -> CsvCell

instance Csv String where
    csv x
        | all isAlphaNum x = CsvCell x
        | otherwise = CsvCell $ "\"" ++ x ++ "\""

instance Csv Double where
    csv = CsvCell . show
