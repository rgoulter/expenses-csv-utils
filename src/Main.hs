{-# LANGUAGE QuasiQuotes #-}

module Main
  ( main
  )
where

import           Control.Monad                  ( forM_ )

import qualified Data.List.NonEmpty            as NE

import           Data.String.Interpolate        ( i )

import           System.IO                      ( hFlush
                                                , stdout
                                                )

import           Text.CSV                       ( printCSV )

import           Data.Expenses.Ledger           ( outputLedgerFromEntries
                                                , directiveFromEntry
                                                , showEntryDateWithDay
                                                )
import           Data.Expenses.Ledger.AccountSuggestions
                                                ( SuggestionResult(..)
                                                , suggestions
                                                )
import qualified Data.Expenses.Ledger.Process  as LP
import qualified Data.Expenses.Ledger.Xml      as LX
import           Data.Expenses.Parse.Megaparsec.Document
                                                ( withFile )
import           Data.Expenses.Query            ( attr
                                                , queryDirectives
                                                )
import           Data.Expenses.ToCSV            ( recordsFromEntries )
import           Data.Expenses.Types            ( Entry(..)
                                                , SimpleTransaction
                                                , entriesFromModel
                                                )
import qualified Main.CmdArgs                  as Args



main :: IO ()
main = do
  mode <- Args.run
  case mode of
    Args.CSV inputF outputF  -> runCsvMode inputF outputF
    Args.Check inputF        -> runCheckMode inputF
    Args.Query attrib inputF -> runQueryMode attrib inputF
    Args.Ledger inputF outputF noAccounts journals ->
      runLedgerMode inputF outputF noAccounts journals



runCsvMode :: String -> String -> IO ()
runCsvMode inputF outputF = withFile inputF
  $ \model -> outputCSVFromDirectives outputF $ entriesFromModel model



outputCSVFromDirectives :: String -> [Entry] -> IO ()
outputCSVFromDirectives outputF entries =
  let rows = recordsFromEntries entries
      outp = printCSV rows
  in  writeFile outputF outp



runCheckMode :: String -> IO ()
runCheckMode inputF = withFile inputF $ \_directives -> return ()



runQueryMode :: String -> FilePath -> IO ()
runQueryMode qattr inputF = case attr qattr of
  Nothing    -> putStrLn $ "unknown attribute: " ++ qattr
  Just attr' -> withFile inputF $ \model ->
    let entries = entriesFromModel model
    in  putStrLn $ queryDirectives attr' entries



readJournal :: FilePath -> IO [SimpleTransaction]
readJournal journalPath = do
  xml <- LP.xmlOf journalPath
  return $ LX.simpleTransactionsInXmlDocument xml



simpleTransactions :: [FilePath] -> IO [SimpleTransaction]
simpleTransactions journalPaths = do
  journals <- mapM readJournal journalPaths
  return $ concat journals





readAccount :: [String] -> IO String
readAccount []   = getLine
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
    "6" | sugsLen > 5 -> sugs !! 5
    "7" | sugsLen > 6 -> sugs !! 6
    "8" | sugsLen > 7 -> sugs !! 7
    "9" | sugsLen > 8 -> sugs !! 8
    _                 -> line


promptForEntry :: Entry -> IO String
promptForEntry e = do
  putStrLn $ showEntryDateWithDay e
  putStrLn $ directiveFromEntry e
  putStrLn ""
  putStr "Debitted account:"
  hFlush stdout
  readAccount [] <* putStrLn ""

promptForEntryWith :: [String] -> Entry -> IO String
promptForEntryWith sugs e = do
  putStrLn $ showEntryDateWithDay e
  putStrLn $ directiveFromEntry e
  putStrLn ""
  putStrLn "Suggested Accounts:"
  let zipped :: [(String, Int)]
      zipped = zip sugs [1 ..]
  forM_ zipped (\(s, idx) -> putStrLn [i| (#{idx}) #{s}|])
  putStr "Debitted account:"
  hFlush stdout
  readAccount sugs <* putStrLn ""


runLedgerMode :: String -> String -> Bool -> [FilePath] -> IO ()
runLedgerMode inputF outputF useUndescribedAccounts journals = do
  txns <- simpleTransactions journals
  let acct :: Entry -> IO String
      acct e = if useUndescribedAccounts
        then return "Undescribed"
        else case suggestions txns (entryRemark e) of
          Exact     a    -> return a
          Ambiguous sugs -> promptForEntryWith (take 9 $ NE.toList sugs) e
          None           -> promptForEntry e
  withFile inputF $ \model ->
    let entries = entriesFromModel model
    in  outputLedgerFromEntries outputF entries acct
