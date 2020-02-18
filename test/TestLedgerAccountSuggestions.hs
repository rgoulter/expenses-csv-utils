module TestLedgerAccountSuggestions (ledgerAccountSuggestionsSpec) where

import qualified Data.List.NonEmpty as NE

import Test.Hspec (Spec, describe, it, shouldBe)

import qualified Data.Expenses.Ledger.AccountSuggestions as AS
import Data.Expenses.Types (Money(..), SimpleTransaction(..))


simpleTransactionWithDescription :: String -> String -> SimpleTransaction
simpleTransactionWithDescription description debittedAccount =
  SimpleTransaction
  { transactionDescription = description
  , transactionAmount = Amount { moneyAmount = 5, moneyCurrency = Just "SGD", moneyIsApprox = False }
  , transactionCredittedAccount = "Assets:Cash"
  , transactionDebittedAccount = debittedAccount
  }


sampleTxn1 :: SimpleTransaction
sampleTxn1 = simpleTransactionWithDescription "at McDonalds" "Expenses:Food"

sampleTxn1Alt :: SimpleTransaction
sampleTxn1Alt = simpleTransactionWithDescription "at McDonalds" "Expenses:Dessert"


sampleTxn2 :: SimpleTransaction
sampleTxn2 = simpleTransactionWithDescription "at 7-11" "Expenses:Snacks"


sampleTxn3 :: SimpleTransaction
sampleTxn3 = simpleTransactionWithDescription "at 7-11" "Expenses:Drinks"


ambiguous :: [String] -> AS.SuggestionResult
ambiguous xs = AS.Ambiguous $ NE.fromList xs


ledgerAccountSuggestionsSpec :: Spec
ledgerAccountSuggestionsSpec =
  describe "Data.Expenses.Ledger.AccountSuggestions" $ do
    describe "suggestions" $ do
      it "returns None for empty input" $
        AS.suggestions [] "" `shouldBe` AS.None
      describe "matching description" $ do
        it "returns an Exact match for when only one account associated with a description" $ do
          {-
            i.e. in this case, the description exactly matches previous
             descriptions, and transactions with that description ALL
             have the same debitted account.
          -}
          AS.suggestions [sampleTxn1] "at McDonalds" `shouldBe`
            AS.Exact "Expenses:Food"
          AS.suggestions [sampleTxn1, sampleTxn1] "at McDonalds" `shouldBe`
            AS.Exact "Expenses:Food"
        it "returns an Ambiguous match for when multiple associated with a description" $
          AS.suggestions [sampleTxn1, sampleTxn1Alt] "at McDonalds" `shouldBe`
            ambiguous ["Expenses:Dessert", "Expenses:Food"]
      -- {-
      describe "keyword-based descriptions" $ do
        it "returns accounts sorted by frequency for when input keywords not previously used" $
          AS.suggestions
            [ sampleTxn1
            , sampleTxn1
            , sampleTxn1
            , sampleTxn2
            , sampleTxn3
            , sampleTxn3
            ]
            "KFC"
          `shouldBe`
          ambiguous ["Expenses:Food", "Expenses:Drinks", "Expenses:Snacks"]
        it "returns an Ambiguous match for when multiple associated with keywords" $
          -- This case is still 'ambiguous' because, even though there's only
          --  one 'matching' account, it's not been used as a debitted account
          --  for a transaction with that description.
          AS.suggestions [sampleTxn1] "McDonalds"
          `shouldBe`
          ambiguous ["Expenses:Food"]
      -- -}
