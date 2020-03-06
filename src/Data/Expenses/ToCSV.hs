module Data.Expenses.ToCSV (recordsFromDirectives) where

import Text.CSV as CSV

import Text.Printf (printf)

import Data.Expenses.Parse.Megaparsec.Entry
  (Entry, entryDate, entryPrice, entryRemark)
import Data.Expenses.Parse.Megaparsec.Document (entriesFromDirectives)
import Data.Expenses.Parse.Megaparsec.Types (LineDirective)



recordFromEntry :: Entry -> CSV.Record
recordFromEntry entry =
  [date, price, cur, remark]
  where
    (y, m, d) = entryDate entry
    date   = printf "%4d-%02d-%02d" y m d
    (amount', cur) = entryPrice entry
    price  = show amount'
    remark = entryRemark entry



recordsFromDirectives :: [LineDirective] -> [CSV.Record]
recordsFromDirectives directives =
  map recordFromEntry $ entriesFromDirectives directives
