module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array as A
import Data.Maybe (fromMaybe, fromJust)
import Data.Tuple (Tuple(..))
import Node.FS.Sync (readTextFile)
import Node.FS (FS)
import Node.Encoding (Encoding(..))
import Control.Monad.Eff.Exception (EXCEPTION)
import Partial.Unsafe (unsafePartial)

import Data.TimeSeries as TS
import Data.TimeSeries.IO as IO
import Learn.Unsupervised.OutlierDetection as OC
import Learn.Metrics.ConfusionMatrix as CM
import LinearAlgebra.Matrix as M


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
  let s3 = A.zipWith (/=) s1.values s2.values             
  log $ "Anomalies in test set: " <> show (A.length (A.filter ((==) true) $ s3))
  log "Train model..."
  let td = unsafePartial $ fromJust $ M.fromArray (TS.length s1) 1 s1.values
  let model = OC.train td
  log "Make predictions..."
  let predictions = OC.predict model td
  -- Mark anomalies
  let s4 = map (\p -> p < 0.01) predictions
  log $ "#Predicted anomalies: " <> show (A.length (A.filter id s4))
  let cm = CM.calculate s3 s4
  log "Confusion matrix"
  log $ CM.toString cm


-- Load raw and corrected Time Series from CSV file
loadSeries :: ∀ eff. String -> Eff (console :: CONSOLE, exception :: EXCEPTION, fs :: FS | eff) (Tuple (TS.Series Number) (TS.Series Number))
loadSeries fileName = do
  csv <- readTextFile UTF8 fileName
  let xs = IO.fromCsv csv
  let s1 = fromMaybe TS.empty (A.index xs 0)
  let s2 = fromMaybe TS.empty (A.index xs 1)
  pure $ Tuple s1 s2