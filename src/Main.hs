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

import Data.Expenses.Parse.Megaparsec.ExpensesDoc
  (eitherOfLists, entriesFromDirectives, parseExpensesFile)
import Data.Expenses.Parse.Megaparsec.Types (LineDirective)
import Data.Expenses.Query (attr, queryDirectives)
import Data.Expenses.ToCSV (recordsFromDirectives)



data ExpensesCmd = CSV {src :: FilePath, out :: FilePath}
                 | Check {src :: FilePath}
                 | Query {attribute :: String, src :: FilePath}
  deriving (Data,Typeable,Show,Eq)



csv = CSV
  { src = def &= typ "EXPENSES.TXT" &= argPos 1
  , out = def &= typ "OUT.CSV" &= argPos 2
  } &= help "Output to CSV"



-- why is argPos 0 here, when it's 1 2 above?
check = Check
  { src = def &= typ "EXPENSES.TXT" &= argPos 0
  } &= help "Check expenses file"



query = Query
  { attribute = def &= typ "earliest|latest" &= argPos 1
  , src = def &= typ "EXPENSES.TXT" &= argPos 2
  } &= help "Check expenses file"



mode =
  cmdArgsMode $ modes [check, csv, query]
    &= help "Utils for expenses file format"
    &= program "expenses-utils"
    &= summary "Expenses Utils v0.2.1"



main :: IO ()
main = do
  expensesArgs <- cmdArgsRun mode
  case expensesArgs of
    Check inputF -> checkSyntax inputF
    CSV inputF outputF -> process inputF outputF
    Query attribute inputF -> queryFile attribute inputF



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



process :: String -> String -> IO ()
process inputF outputF =
  withFile inputF $ \directives ->
    outputCSVFromDirectives outputF directives



outputCSVFromDirectives :: String -> [LineDirective] -> IO ()
outputCSVFromDirectives outputF directives =
  let rows = recordsFromDirectives directives
      outp = printCSV rows
  in  writeFile outputF outp



checkSyntax :: String -> IO ()
checkSyntax inputF =
  withFile inputF $ \_directives -> return ()



queryFile :: String -> FilePath -> IO ()
queryFile attribute inputF = do
  case attr attribute of
    Nothing -> putStrLn $ "unknown attribute: " ++ attribute
    Just attr' ->
      withFile inputF $ \directives ->
        let entries = entriesFromDirectives directives
        in putStrLn $ queryDirectives attr' entries
