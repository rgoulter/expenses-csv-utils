module Main (main) where

import Test.Hspec (hspec)

import Data.Expenses.Parse.Megaparsec.DateDirective (Parser)
import TestDateParser (parseDateDirectiveSpec)
import TestExpense (expenseSpec)
import TestExpenseParser (parseExpenseDirectiveSpec)
import TestExpenseDocParser (parseExpensesFileSpec)



main :: IO ()
main = hspec $ do
  expenseSpec
  parseDateDirectiveSpec
  parseExpenseDirectiveSpec
  parseExpensesFileSpec

