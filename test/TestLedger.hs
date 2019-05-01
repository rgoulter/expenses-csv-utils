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
  # Spent 5 SGD on McDonalds
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
  # Spent 5 SGD on McDonalds
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
  # Spent 5 SGD on McDonalds
  2018-01-01 on McDonalds
    Undescribed  5.0 SGD
    Assets:Cash:SGD
  # comment1
  # comment2|]



ledgerSpec :: Spec
ledgerSpec =
  describe "Data.Expenses.Ledger" $ do
    describe "showCommaSeparatedNumber" $ do
      it "should show numbers with commas (e.g. 1000 -> 1,000)" $ do
        L.showCommaSeparatedNumber 1 `shouldBe` "1"
        L.showCommaSeparatedNumber 1000 `shouldBe` "1,000"
        L.showCommaSeparatedNumber 1234567 `shouldBe` "1,234,567"
    describe "showHumanReadableMoney" $ do
      it "should output numbers in a human readable format (e.g. 5 SGD, 3.1m VND)" $ do
        L.showHumanReadableMoney (5, 0, "SGD") `shouldBe` "5 SGD"
        L.showHumanReadableMoney (1, 25, "SGD") `shouldBe` "1.25 SGD"
        L.showHumanReadableMoney (1, 50, "SGD") `shouldBe` "1.50 SGD"
        L.showHumanReadableMoney (2000, 0, "SGD") `shouldBe` "2k SGD"
        L.showHumanReadableMoney (65000, 0, "VND") `shouldBe` "65k VND"
        L.showHumanReadableMoney (10500, 0, "VND") `shouldBe` "10.5k VND"
        L.showHumanReadableMoney (3000000, 0, "VND") `shouldBe` "3m VND"
        L.showHumanReadableMoney (3100000, 0, "VND") `shouldBe` "3.1m VND"
        L.showHumanReadableMoney (3100005, 0, "VND") `shouldBe` "3,100,005 VND"
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
              , Entry (2018, 01, 03) (2000, 0, "SGD") "on new computer" Nothing
              ]
            expectedJournal =
              unindent [i|
              # 2018-01-01 Monday
              # Spent 5 SGD on McDonalds
              2018-01-01 on McDonalds
                Undescribed  5.0 SGD
                Assets:Cash:SGD

              # 2018-01-03 Wednesday
              # Spent 2k SGD on new computer
              2018-01-03 on new computer
                Undescribed  2,000.0 SGD
                Assets:Cash:SGD
              |]
        L.showLedgerJournalFromEntries inputEntries `shouldBe` expectedJournal
