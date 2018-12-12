module ToCSV where


import Text.CSV as CSV

import Text.Printf (printf)

import Entry
import ParseExpensesDoc (LineDirective, entriesFromDirectives)



recordFromEntry :: Entry -> CSV.Record
recordFromEntry entry =
  [date, price, cur, remark] ++ map stringFromCategory (entryCategories entry)
  where
    (y, m, d) = entryDate entry
    date   = printf "%4d-%02d-%02d" y m d
    (dollars, cents, cur) = entryPrice entry
    price  = printf "%d.%d" dollars cents
    remark = entryRemark entry



recordsFromDirectives :: [LineDirective] -> [CSV.Record]
recordsFromDirectives directives =
  map recordFromEntry $ entriesFromDirectives directives
