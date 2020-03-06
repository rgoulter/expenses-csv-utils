{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Control.Monad (forM_)

import qualified Data.List.NonEmpty as NE

import Data.String.Interpolate (i)

import qualified Data.Text.IO as TIO

import Hledger.Read (readJournal')

import System.IO (hFlush, stdout)

import Text.CSV (printCSV)

import Text.Megaparsec
  ( errorBundlePretty
  , parseErrorPretty
  , runParser
  )

import Data.Expenses.Ledger
  ( outputLedgerFromEntries
  , simpleTransactionsInJournal
  , directiveFromEntry
  , showEntryDateWithDay
  )
import Data.Expenses.Ledger.AccountSuggestions (SuggestionResult(..), suggestions)
import Data.Expenses.Parse.Megaparsec.Document
  (eitherOfLists, entriesFromDirectives, parseExpensesFile)
import Data.Expenses.Parse.Megaparsec.Types (LineDirective)
import Data.Expenses.Query (attr, queryDirectives)
import Data.Expenses.ToCSV (recordsFromDirectives)
import Data.Expenses.Types (Entry(..), SimpleTransaction)
import qualified Main.CmdArgs as Args



main :: IO ()
main = do
  mode <- Args.run
  case mode of
    Args.CSV inputF outputF -> runCsvMode inputF outputF
    Args.Check inputF -> runCheckMode inputF
    Args.Query attrib inputF -> runQueryMode attrib inputF
    Args.Ledger inputF outputF noAccounts journals -> runLedgerMode inputF outputF noAccounts journals



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
runQueryMode qattr inputF =
  case attr qattr of
    Nothing -> putStrLn $ "unknown attribute: " ++ qattr
    Just attr' ->
      withFile inputF $ \directives ->
        let entries = entriesFromDirectives directives
        in putStrLn $ queryDirectives attr' entries



readJournal :: FilePath -> IO [SimpleTransaction]
readJournal journalPath = do
  journalT <- TIO.readFile journalPath
  journal' <- readJournal' journalT
  return $ simpleTransactionsInJournal journal'



simpleTransactions :: [FilePath] -> IO [SimpleTransaction]
simpleTransactions journalPaths = do
  journals <- mapM readJournal journalPaths
  return $ concat journals





readAccount :: [String] -> IO String
readAccount [] =
  getLine
readAccount sugs = do
  line <- getLine
  let sugsLen = length sugs
  -- Inelegant, but easy to implement
  return $ case line of
    "1" | sugsLen > 0 -> sugs !! 0
    "2" | sugsLen > 1 -> sugs !! 1
    "3" | sugsLen > 2 -> sugs !! 2
    "4" | sugsLen > 3 -> sugs !! 3
    "5" | sugsLen > 4 -> sugs !! 4
    _ -> line


promptForEntry :: Entry -> IO String
promptForEntry e = do
  putStrLn $ showEntryDateWithDay e
  putStrLn $ directiveFromEntry e
  putStrLn ""
  putStr "Debitted account:"
  hFlush stdout
  readAccount []

promptForEntryWith :: [String] -> Entry -> IO String
promptForEntryWith sugs e = do
  putStrLn $ showEntryDateWithDay e
  putStrLn $ directiveFromEntry e
  putStrLn ""
  putStrLn "Suggested Accounts:"
  let zipped :: [(String, Int)]
      zipped = zip sugs [1..]
  forM_ zipped (\(s, idx) -> putStrLn [i| (#{idx}) #{s}|])
  putStr "Debitted account:"
  hFlush stdout
  readAccount sugs


runLedgerMode :: String -> String -> Bool -> [FilePath] -> IO ()
runLedgerMode inputF outputF useUndescribedAccounts journals = do
  txns <- simpleTransactions journals
  let
    acct :: Entry -> IO String
    acct e =
     if useUndescribedAccounts then
       return "Undescribed"
     else
       case suggestions txns (entryRemark e) of
         Exact a -> return a
         Ambiguous sugs -> promptForEntryWith (take 9 $ NE.toList sugs) e
         None -> promptForEntry e
  withFile inputF $ \directives ->
    let entries = entriesFromDirectives directives
    in outputLedgerFromEntries outputF entries acct
