module TestExpenseParser where

import Data.List.NonEmpty (NonEmpty (..))

import qualified Data.Set as E

import Text.Heredoc (here)

import Test.Hspec (Spec, describe, it)

import Test.Hspec.Megaparsec (failsLeaving, initialState, shouldFailOn, shouldParse, succeedsLeaving)

import Text.Megaparsec (parse, runParser')


import qualified Data.Expenses.Parse.Megaparsec.ExpenseDirective as PE
import qualified Data.Expenses.Expense as E



{-
 Expense directive:
 SPENT | RECEIVED
 AMOUNT: [$] xy.zw [CUR]
 REMARK: [words till EOL]
 & together
-}
parseExpenseDirectiveSpec :: Spec
parseExpenseDirectiveSpec =
  describe "Data.Expenses.Parse.Megaparsec.ExpenseDirective" $ do
    describe "direction" $ do
      -- direction Spent/Rcv
      it "should parse 'Spent', 'Received'" $ do
        parse PE.direction "" "Spent"    `shouldParse` E.Spent
        parse PE.direction "" "Received" `shouldParse` E.Received
      it "should not parse not-direction" $ do
        parse PE.direction "" `shouldFailOn` "NotADirection"
        parse PE.direction "" `shouldFailOn` "MON"
        parse PE.direction "" `shouldFailOn` "2016-02-01 MON"
        parse PE.direction "" `shouldFailOn` "Sent"

    describe "amount" $ do
      -- amount [~] 1[.23] [CUR]
      it "should parse cases like '1', '~1', '1.23', '1 NZD', etc." $ do
        parse PE.amount ""  "1.23"  `shouldParse` E.Amount 1 23 Nothing False
        parse PE.amount "" "~1.23"  `shouldParse` E.Amount 1 23 Nothing True
        parse PE.amount ""  "1"     `shouldParse` E.Amount 1  0 Nothing False
        -- Note that, if we test for currencies other than USD,
        parse PE.amount ""  "1 USD"
          `shouldParse` E.Amount 1  0 (Just "USD") False
        parse PE.amount ""  "1 NZD"
          `shouldParse` E.Amount 1  0 (Just "NZD") False
        parse PE.amount ""  "1 SGD"
          `shouldParse` E.Amount 1  0 (Just "SGD") False
        parse PE.amount ""  "1 MYR"
          `shouldParse` E.Amount 1  0 (Just "MYR") False
      it "should fail to parse not-amount (e.g. S$123, $123, MON, etc.)" $ do
        parse PE.amount "" `shouldFailOn` "NotAnAmount"
        parse PE.amount "" `shouldFailOn` "S$123"
        parse PE.amount "" `shouldFailOn` "$123"
        parse PE.amount "" `shouldFailOn` "Spent"
        parse PE.amount "" `shouldFailOn` "MON"

    describe "expense" $ do
      -- expense (dir, amt, remark)
      it "should parse expense directive (working cases)" $ do
        parse PE.expense "" "Spent 1.23 on food"
          `shouldParse` E.Expense E.Spent (E.Amount 1 23 Nothing False) "on food"
      it "should not parse not expense directive" $ do
        parse PE.expense "" `shouldFailOn` "NotAnExpenseDirective"

      it "doesn't parse beyond EOL" $ do
        -- consume everything until the newline, for the 'remark'
        runParser' PE.expense (initialState "Spent 1 on x\nnext")
          `succeedsLeaving` "\nnext"

      it "shouldn't consume input on failure" $ do
        -- Preserve all input on case of date directive
        runParser' PE.expense (initialState "MON\nnext")
          `failsLeaving` "MON\nnext"
        runParser' PE.expense (initialState "2016-02-01 MON\nnext")
          `failsLeaving` "2016-02-01 MON\nnext"
        runParser' PE.expense (initialState "Sent 3 on x\nnext")
          `failsLeaving` "Sent 3 on x\nnext"

        -- Comments are taken care of in main
        runParser' PE.expense (initialState "#cmt\nnext")
          `failsLeaving` "#cmt\nnext"
