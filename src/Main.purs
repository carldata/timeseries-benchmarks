module Main where

import Prelude
import Data.Array as A
import Learn.Metrics.ConfusionMatrix as CM
import Learn.Unsupervised.OutlierDetection as OC
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Maybe (fromMaybe, fromJust)
import Data.Tuple (Tuple(..), fst, snd)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Partial.Unsafe (unsafePartial)

import LinearAlgebra.Matrix as M
import Statistics.Sample as S
import Data.TimeSeries as TS
import Data.TimeSeries.IO as IO


type NSeries = TS.Series Number 


main :: ∀ eff. Eff (console :: CONSOLE, exception :: EXCEPTION, fs :: FS | eff) Unit
main = do
  anomalyReport "testdata/test60k.csv"


-- This function expects that:
--   * file is in CSV format
--   * Contains 2 value columns. First with raw data and second with corrected data
anomalyReport :: ∀ eff. String -> Eff (console :: CONSOLE, exception :: EXCEPTION, fs :: FS | eff) Unit
anomalyReport fileName = do
  log $ "\n# Benchmark: " <> fileName
  Tuple s1 s2 <- loadSeries fileName
  let s3 = A.zipWith (/=) s1.values s2.values             
  indexReport s1
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


indexReport :: ∀ eff. NSeries -> Eff (console :: CONSOLE, exception :: EXCEPTION, fs :: FS | eff) Unit
indexReport xs = do 
  log $ "Dataset length: " <> show (TS.length xs)
  let idx1 = xs.index
  let idx2 = A.zipWith (\x1 x2 -> x2-x1) idx1 (fromMaybe [] (A.tail idx1))
  log $ "Index histogram " <> show (S.histogram idx2)

  -- let idx3 = A.zip idx1 (fromMaybe [] (A.tail idx1))
  -- let idx4 = A.filter (\tu -> (fst tu) > (snd tu)) idx3
  -- log $ show idx4

