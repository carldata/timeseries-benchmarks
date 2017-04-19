module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array as A
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Node.FS.Sync (readTextFile)
import Node.FS (FS)
import Node.Encoding (Encoding(..))
import Control.Monad.Eff.Exception (EXCEPTION)

import Data.TimeSeries as TS
import Data.TimeSeries.IO as IO


main :: ∀ eff. Eff (console :: CONSOLE, exception :: EXCEPTION, fs :: FS | eff) Unit
main = do
  testFile "testdata/test60k.csv"


-- This function expects that:
--   * file is in CSV format
--   * Contains 2 value columns. First with raw data and second with corrected data
testFile :: ∀ eff. String -> Eff (console :: CONSOLE, exception :: EXCEPTION, fs :: FS | eff) Unit
testFile fileName = do
  log $ "\n# Benchmark: " <> fileName
  Tuple s1 s2 <- loadSeries fileName
  log $ "Dataset length: " <> show (TS.length s1)
  log $ "#Anomalies: " <> "not implemented yet"


-- Load raw and corrected Time Series from CSV file
loadSeries :: ∀ eff. String -> Eff (console :: CONSOLE, exception :: EXCEPTION, fs :: FS | eff) (Tuple (TS.Series Number) (TS.Series Number))
loadSeries fileName = do
  csv <- readTextFile UTF8 fileName
  let xs = IO.fromCsv csv
  let s1 = fromMaybe TS.empty (A.index xs 0)
  let s2 = fromMaybe TS.empty (A.index xs 1)
  pure $ Tuple s1 s2