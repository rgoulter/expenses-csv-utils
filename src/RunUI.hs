module Main where

import System.Environment (getArgs)
import Text.CSV (CSV, printCSV, parseCSVFromFile)

import Control.Monad (void)
import Brick.Main (defaultMain)

import qualified Categorise as M
import qualified ParseExpensesDoc as D
import qualified CategoriseEntries as CE

import UI



main :: IO ()
main = do
  args <- getArgs
  if length args >= 1 then
    let csvFilename:_ = args
    in
      parseCSVFromFile csvFilename >>= \res ->
        case res of
          Left e -> do
            putStrLn "Unable to parse CSV file."
            print e
          Right csv ->
            let (initModel, initPrompt) = CE.initFromCSV csv
                initState = initialState initPrompt CE.nextPrompt initModel
            in  void $ defaultMain theApp initState
  else
    putStrLn "Please run with <csvfile.csv>"
