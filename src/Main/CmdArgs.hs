{-# LANGUAGE DeriveDataTypeable #-}

module Main.CmdArgs (ExpensesCmd(..), run) where

import Data.Data (Data)
import Data.Typeable (Typeable)

import System.Console.CmdArgs
   ( (&=)
   , CmdArgs
   , Mode
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



data ExpensesCmd
  = CSV {src :: FilePath, out :: FilePath}
  | Check {src :: FilePath}
  | Query {attribute :: String, src :: FilePath}
  | Ledger { src :: FilePath
           , out :: FilePath
           , no_accounts :: Bool
           , journal :: [FilePath]
           }
  deriving (Data, Eq, Show, Typeable)



csvMode :: ExpensesCmd
csvMode = CSV
  { src = def &= typ "EXPENSES.TXT" &= argPos 1
  , out = def &= typ "OUT.CSV" &= argPos 2
  } &= help "Output to CSV"



-- I'm not sure why argPos is 0 here, when it's 1 2 above;
-- but if I change the csvMode then the ledgerMode breaks.
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
  { src = def &= argPos 0 &= typ "EXPENSES.TXT"
  , out = def &= argPos 1 &= typ "JOURNAL.LEDGER"
  , no_accounts = def &= typ "Fill in accounts "
  , journal = def &= typ "ledger.dat"
  } &= help "Output to Ledger format"



mode :: Mode (CmdArgs ExpensesCmd)
mode =
  cmdArgsMode $ modes [checkMode, csvMode, queryMode, ledgerMode]
    &= help "Utils for expenses file format"
    &= program "expenses-utils"
    &= summary "Expenses Utils v0.2.1"



run :: IO ExpensesCmd
run = cmdArgsRun mode
