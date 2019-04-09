{-# LANGUAGE QuasiQuotes #-}

module Data.Expenses.Ledger where

import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)

import Text.Printf (printf)

import Data.Expenses.Parse.Megaparsec.Entry
  (Entry, entryDate, entryPrice, entryRemark)
import Data.Expenses.Parse.Megaparsec.ExpensesDoc (entriesFromDirectives)
import Data.Expenses.Parse.Megaparsec.Types (LineDirective)



showLedgerTransactionFromEntry :: Entry -> String
showLedgerTransactionFromEntry entry =
  unindent [i|
  #{date} #{remark}
    Undescribed  #{price} #{cur}
    Assets:Cash:#{cur}|]
  where
    (y, m, d) = entryDate entry
    date   = printf "%4d-%02d-%02d" y m d :: String
    (dollars, cents, cur) = entryPrice entry
    price  = printf "%d.%d" dollars cents :: String
    remark = entryRemark entry



outputLedgerFromEntries :: String -> [Entry] -> IO ()
outputLedgerFromEntries outputF entries =
  let transactions = map showLedgerTransactionFromEntry entries
      outp = unlines transactions
  in  writeFile outputF outp
