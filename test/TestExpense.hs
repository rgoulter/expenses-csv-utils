module TestExpense where

import Test.Hspec (Spec, describe, it, shouldBe)

import qualified Data.Expenses.Expense as E

expenseSpec :: Spec
expenseSpec =
  describe "Data.Expenses.Expense" $ do
    describe "numDaysAfter" $ do
      it "should compute Tue is 1 day after Mon" $ do
        (E.numDaysAfter E.Mon E.Tue) `shouldBe` (1 :: Int)
      it "should compute Mon is 6 days after Tue" $ do
        (E.numDaysAfter E.Tue E.Mon) `shouldBe` (6 :: Int)
    describe "addDays" $ do
      it "should compute 2000-01-02 is 1 day after 2000-01-01" $ do
        (E.addDays (2000, 01, 01) 1) `shouldBe` ((2000, 01, 02) :: (Int, Int, Int))
      it "should compute 2001-01-01 is 31 days after 2000-12-01" $ do
        (E.addDays (2000, 12, 01) 31) `shouldBe` ((2001, 01, 01) :: (Int, Int, Int))
