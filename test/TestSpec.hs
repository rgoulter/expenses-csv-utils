module Main (main) where

import Test.Hspec (hspec)

import TestDateParser (parseDateDirectiveSpec)
import TestExpense (expenseSpec)
import TestExpenseParser (parseExpenseDirectiveSpec)
import TestUsingParser (parseUsingDirectiveSpec)
import TestDocumentParser (parseExpensesFileSpec)
import TestQuery (querySpec)
import TestLedger (ledgerSpec)
import TestLedgerProcess (ledgerProcessSpec)
import TestLedgerXml (ledgerXmlSpec)
import TestLedgerAccountSuggestions (ledgerAccountSuggestionsSpec)



main :: IO ()
main = hspec $ do
  expenseSpec
  querySpec
  parseDateDirectiveSpec
  parseExpenseDirectiveSpec
  parseUsingDirectiveSpec
  parseExpensesFileSpec
  ledgerSpec
  ledgerAccountSuggestionsSpec
  ledgerProcessSpec
  ledgerXmlSpec
