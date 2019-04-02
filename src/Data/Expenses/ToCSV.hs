module Data.Expenses.ToCSV where

import Text.CSV as CSV

import Text.Printf (printf)

import Data.Expenses.Parse.Megaparsec.Entry
  (Entry, entryDate, entryPrice, entryRemark)
import Data.Expenses.Parse.Megaparsec.ExpensesDoc
  (LineDirective, entriesFromDirectives)



recordFromEntry :: Entry -> CSV.Record
recordFromEntry entry =
  [date, price, cur, remark]
  where
    (y, m, d) = entryDate entry
    date   = printf "%4d-%02d-%02d" y m d
    (dollars, cents, cur) = entryPrice entry
    price  = printf "%d.%d" dollars cents
    remark = entryRemark entry



recordsFromDirectives :: [LineDirective] -> [CSV.Record]
recordsFromDirectives directives =
  map recordFromEntry $ entriesFromDirectives directives
