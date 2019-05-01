{-# LANGUAGE QuasiQuotes #-}

module TestLedger (ledgerSpec) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)

import qualified Data.Text as T

import qualified Data.Time.Calendar as DT

import Hledger.Read (readJournal')

import qualified Data.Expenses.Expense as E
import qualified Data.Expenses.Ledger as L
import Data.Expenses.Types (Money(Amount), Entry(..), SimpleTransaction(..))



sampleEntry :: Entry
sampleEntry =
  Entry (2018, 01, 01) (5, 0, "SGD") "on McDonalds" Nothing



sampleTransaction :: String
sampleTransaction =
  unindent [i|
  # Spent 5.0 SGD on McDonalds
  2018-01-01 on McDonalds
    Undescribed  5.0 SGD
    Assets:Cash:SGD|]



sampleJournalWithTransactions :: String
sampleJournalWithTransactions =
  unindent [i|
  2018-01-01 on McDonalds
    Expenses:Food  5.0 SGD
    Assets:Cash:SGD

  2018-01-02 at Guardian
    Expenses:Toiletries  5.0 SGD
    Assets:Cash:SGD|]



entryWithComment :: Entry
entryWithComment =
  Entry (2018, 01, 01) (5, 0, "SGD") "on McDonalds" (Just "# comment")



transactionWithComment :: String
transactionWithComment =
  unindent [i|
  # Spent 5.0 SGD on McDonalds
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
  # Spent 5.0 SGD on McDonalds
  2018-01-01 on McDonalds
    Undescribed  5.0 SGD
    Assets:Cash:SGD
  # comment1
  # comment2|]



ledgerSpec :: Spec
ledgerSpec =
  describe "Data.Expenses.Ledger" $ do
    describe "simpleTransactionsInJournal" $ do
      it "should get a SimpleTransaction from a sample ledger journal" $ do
        journal <- readJournal' $ T.pack $ unindent [i|
          2018/01/01 on McDonalds
            Expenses:Food  5.0 SGD
            Assets:Cash:SGD
          |]
        let actualTransactions = L.simpleTransactionsInJournal journal
        actualTransactions
          `shouldBe`
            [ SimpleTransaction "on McDonalds"
                                (Amount 5 0 (Just "SGD") False)
                                "Expenses:Food"
                                "Assets:Cash:SGD"
            ]
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

    describe "showLedgerJournalFromEntries" $ do
      it "should show a Ledger journal with multiple transactions" $ do
        let inputEntries =
              [ Entry (2018, 01, 01) (5, 0, "SGD") "on McDonalds" Nothing
              , Entry (2018, 01, 03) (40, 0, "SGD") "on ez-link top up" Nothing
              ]
            expectedJournal =
              unindent [i|
              # 2018-01-01 Monday
              # Spent 5.0 SGD on McDonalds
              2018-01-01 on McDonalds
                Undescribed  5.0 SGD
                Assets:Cash:SGD

              # 2018-01-03 Wednesday
              # Spent 40.0 SGD on ez-link top up
              2018-01-03 on ez-link top up
                Undescribed  40.0 SGD
                Assets:Cash:SGD
              |]
        L.showLedgerJournalFromEntries inputEntries `shouldBe` expectedJournal
