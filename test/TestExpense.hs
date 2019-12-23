module TestExpense where

import Test.Hspec (Spec, describe, it, shouldBe)

import qualified Data.Time.Calendar.Compat as DT

import qualified Data.Expenses.Expense as E

expenseSpec :: Spec
expenseSpec =
  describe "Data.Expenses.Expense" $ do
    describe "numDaysAfter" $ do
      it "should compute Tue is 1 day after Mon" $ do
        (E.numDaysAfter DT.Monday DT.Tuesday) `shouldBe` (1 :: Int)
      it "should compute Mon is 6 days after Tue" $ do
        (E.numDaysAfter DT.Tuesday DT.Monday) `shouldBe` (6 :: Int)
