module Main where

import Control.Monad (forM_)

import System.Environment (getArgs)

import Text.CSV (printCSV)

import Text.Megaparsec (eof, parseErrorPretty, runParser)

import Data.Expenses.Parse.Megaparsec.ExpensesDoc
  (eitherOfLists, parseExpensesFile)
import Data.Expenses.Parse.Megaparsec.Types (LineDirective)
import Data.Expenses.ToCSV (recordsFromDirectives)



main :: IO ()
main = do
  args <- getArgs
  if length args == 2 then
    let [inputF, outputF] = args in
    process inputF outputF
  else
    putStrLn "Please run with <input.txt> <output.csv>"



process :: String -> String -> IO ()
process inputF outputF = do
  -- Parse the input file to list of [DateDir | ExpDir]
  rawResult <- runParser parseExpensesFile inputF <$> readFile inputF

  case rawResult of
    Left err ->
      putStrLn $ parseErrorPretty err
    Right result ->
      case eitherOfLists result of
        Left errors ->
          forM_ errors $ putStrLn . parseErrorPretty
        Right directives ->
          outputCSVFromDirectives outputF directives



outputCSVFromDirectives :: String -> [LineDirective] -> IO ()
outputCSVFromDirectives outputF directives =
  let rows = recordsFromDirectives directives
      outp = printCSV rows
  in  writeFile outputF outp
