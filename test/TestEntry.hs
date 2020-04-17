module TestEntry (entrySpec) where

import Test.Hspec (Spec, context, describe, it, shouldBe)

import qualified Data.Expenses.Types as E
import qualified Data.Expenses.Parse.Megaparsec.Types as PE
import qualified Data.Expenses.Parse.Megaparsec.Entry as PE

entrySpec :: Spec
entrySpec =
  describe "Data.Expenses.Parse.Megaparsec.Entry" $
    describe "entryFromExpense" $ do
      it "should use default currency if none given" $
        PE.entryFromExpense
          (2018, 01, 01)
          "SGD"
          (PE.Expense PE.Spent (E.Amount 5 Nothing False) "" Nothing)
        `shouldBe`
          (E.Entry (2018, 01, 01) (5, "SGD") "" Nothing)
      context "should use Expense currency if given" $ do
        it "default SGD, given SGD" $
          PE.entryFromExpense
            (2018, 01, 01)
            "SGD"
            (PE.Expense PE.Spent (E.Amount 5 (Just "SGD") False) "" Nothing)
          `shouldBe`
            (E.Entry (2018, 01, 01) (5, "SGD") "" Nothing)
        it "default VND, given SGD" $
          PE.entryFromExpense
            (2018, 01, 01)
            "VND"
            (PE.Expense PE.Spent (E.Amount 5 (Just "SGD") False) "" Nothing)
          `shouldBe`
            (E.Entry (2018, 01, 01) (5, "SGD") "" Nothing)
        it "default SGD, given VND" $
          PE.entryFromExpense
            (2018, 01, 01)
            "SGD"
            (PE.Expense PE.Spent (E.Amount 10000 (Just "VND") False) "" Nothing)
          `shouldBe`
            (E.Entry (2018, 01, 01) (10000, "VND") "" Nothing)
