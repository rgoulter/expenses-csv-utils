module Main (main) where

import Test.Hspec (hspec)

import TestDateParser (parseDateDirectiveSpec)
import TestExpense (expenseSpec)
import TestExpenseParser (parseExpenseDirectiveSpec)
import TestExpenseDocParser (parseExpensesFileSpec)
import TestQuery (querySpec)



main :: IO ()
main = hspec $ do
  expenseSpec
  querySpec
  parseDateDirectiveSpec
  parseExpenseDirectiveSpec
  parseExpensesFileSpec
