{-# LANGUAGE QuasiQuotes #-}

module TestLedger where

import Test.Hspec (Spec, describe, it, shouldBe)

import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)

import qualified Data.Time.Calendar as DT

import qualified Data.Expenses.Expense as E
import qualified Data.Expenses.Ledger as L
import Data.Expenses.Types (Entry(..))



sampleEntry :: Entry
sampleEntry =
  Entry (2018, 01, 01) (5, 0, "SGD") "on McDonalds" Nothing



sampleTransaction :: String
sampleTransaction =
  unindent [i|
  2018-01-01 on McDonalds
    Undescribed  5.0 SGD
    Assets:Cash:SGD|]



entryWithComment :: Entry
entryWithComment =
  Entry (2018, 01, 01) (5, 0, "SGD") "on McDonalds" (Just "# comment")



transactionWithComment :: String
transactionWithComment =
  unindent [i|
  2018-01-01 on McDonalds
    Undescribed  5.0 SGD
    Assets:Cash:SGD
  # comment|]



entryWithMultilineComment :: Entry
entryWithMultilineComment =
  Entry (2018, 01, 01)
        (5, 0, "SGD")
        "on McDonalds"
        (Just "# comment1\n# comment2")



transactionWithMultilineComment :: String
transactionWithMultilineComment =
  unindent [i|
  2018-01-01 on McDonalds
    Undescribed  5.0 SGD
    Assets:Cash:SGD
  # comment1
  # comment2|]



ledgerSpec :: Spec
ledgerSpec =
  describe "Data.Expenses.Ledger" $ do
    describe "showLedgerTransactionFromEntry" $ do
      it "should show a simple Ledger transaction for an Entry" $ do
        L.showLedgerTransactionFromEntry sampleEntry
          `shouldBe`
            sampleTransaction
      it "should show a simple Ledger transaction for an Entry with a comment" $ do
        L.showLedgerTransactionFromEntry entryWithComment
          `shouldBe`
            transactionWithComment
      it "should show a simple Ledger transaction for an Entry with a multiline comment" $ do
        L.showLedgerTransactionFromEntry entryWithMultilineComment
          `shouldBe`
            transactionWithMultilineComment
