module TestExpense where

import Test.Hspec

import qualified Expense as E

expenseSpec :: Spec
expenseSpec =
  describe "Expense" $ do
    describe "numDaysAfter" $ do
      it "should compute Tue is 1 day after Mon" $ do
        (E.numDaysAfter E.Mon E.Tue) `shouldBe` (1 :: Int)
