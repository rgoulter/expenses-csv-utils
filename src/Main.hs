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
   , def
   , help
   , modes
   , program
   , summary
   , typ
   )

import Text.CSV (printCSV)

import Text.Megaparsec (eof, errorBundlePretty, parseErrorPretty, runParser)

import Data.Expenses.Ledger (outputLedgerFromEntries)
import Data.Expenses.Parse.Megaparsec.ExpensesDoc
  (eitherOfLists, entriesFromDirectives, parseExpensesFile)
import Data.Expenses.Parse.Megaparsec.Types (LineDirective)
import Data.Expenses.Query (attr, queryDirectives)
import Data.Expenses.ToCSV (recordsFromDirectives)



data ExpensesCmd
  = CSV {src :: FilePath, out :: FilePath}
  | Check {src :: FilePath}
  | Query {attribute :: String, src :: FilePath}
  | Ledger {src :: FilePath, out :: FilePath}
  deriving (Data,Typeable,Show,Eq)



csvMode :: ExpensesCmd
csvMode = CSV
  { src = def &= typ "EXPENSES.TXT" &= argPos 1
  , out = def &= typ "OUT.CSV" &= argPos 2
  } &= help "Output to CSV"



-- why is argPos 0 here, when it's 1 2 above?
checkMode :: ExpensesCmd
checkMode = Check
  { src = def &= typ "EXPENSES.TXT" &= argPos 0
  } &= help "Check expenses file"



queryMode :: ExpensesCmd
queryMode = Query
  { attribute = def &= typ "earliest|latest" &= argPos 1
  , src = def &= typ "EXPENSES.TXT" &= argPos 2
  } &= help "Query attributes of expenses file"



ledgerMode :: ExpensesCmd
ledgerMode = Ledger
  { src = def &= typ "EXPENSES.TXT" &= argPos 3
  , out = def &= typ "JOURNAL.LEDGER" &= argPos 4
  } &= help "Output to Ledger format"



mode =
  cmdArgsMode $ modes [checkMode, csvMode, queryMode, ledgerMode]
    &= help "Utils for expenses file format"
    &= program "expenses-utils"
    &= summary "Expenses Utils v0.2.1"



main :: IO ()
main = do
  expensesArgs <- cmdArgsRun mode
  case expensesArgs of
    CSV inputF outputF -> runCsvMode inputF outputF
    Check inputF -> runCheckMode inputF
    Query attribute inputF -> runQueryMode attribute inputF
    Ledger inputF outputF -> runLedgerMode inputF outputF



withFile :: String -> ([LineDirective] -> IO()) -> IO ()
withFile inputF f = do
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
          f directives



runCsvMode :: String -> String -> IO ()
runCsvMode inputF outputF =
  withFile inputF $ \directives ->
    outputCSVFromDirectives outputF directives



outputCSVFromDirectives :: String -> [LineDirective] -> IO ()
outputCSVFromDirectives outputF directives =
  let rows = recordsFromDirectives directives
      outp = printCSV rows
  in  writeFile outputF outp



runCheckMode :: String -> IO ()
runCheckMode inputF =
  withFile inputF $ \_directives -> return ()



runQueryMode :: String -> FilePath -> IO ()
runQueryMode attribute inputF = do
  case attr attribute of
    Nothing -> putStrLn $ "unknown attribute: " ++ attribute
    Just attr' ->
      withFile inputF $ \directives ->
        let entries = entriesFromDirectives directives
        in putStrLn $ queryDirectives attr' entries



runLedgerMode :: String -> String -> IO ()
runLedgerMode inputF outputF =
  withFile inputF $ \directives ->
    let entries = entriesFromDirectives directives
    in outputLedgerFromEntries outputF entries
