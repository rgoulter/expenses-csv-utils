{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Monad (forM_)

import Data.Data (Data)
import Data.Typeable (Typeable)

import System.Environment (getArgs)

import System.Console.CmdArgs
   ( (&=)
   , argPos
   , cmdArgsMode
   , cmdArgsRun
   , help
   , modes
   , program
   , summary
   , typ
   )

import Text.CSV (printCSV)

import Text.Megaparsec (eof, errorBundlePretty, parseErrorPretty, runParser)

import Data.Expenses.Parse.Megaparsec.ExpensesDoc
  (eitherOfLists, parseExpensesFile)
import Data.Expenses.Parse.Megaparsec.Types (LineDirective)
import Data.Expenses.ToCSV (recordsFromDirectives)



data ExpensesCmd = CSV {src :: FilePath, out :: FilePath}
  deriving (Data,Typeable,Show,Eq)



csv = CSV
    { src = "expenses.txt" &= typ "EXPENSES.TXT" &= argPos 0
    , out = "output.csv" &= typ "OUT.CSV" &= argPos 1
    } &= help "Output to CSV"



mode =
  cmdArgsMode $ modes [csv]
    &= help "Utils for expenses file format"
    &= program "expenses-utils"
    &= summary "Expenses Utils v0.2.1"



main :: IO ()
main = do
  expensesArgs <- cmdArgsRun mode
  case expensesArgs of
    CSV inputF outputF -> process inputF outputF



process :: String -> String -> IO ()
process inputF outputF = do
  -- Parse the input file to list of [DateDir | ExpDir]
  rawResult <- runParser parseExpensesFile inputF <$> readFile inputF

  case rawResult of
    Left err ->
      putStrLn $ errorBundlePretty err
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
