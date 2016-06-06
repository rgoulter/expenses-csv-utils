module TestExpenseParser where

import Data.List.NonEmpty (NonEmpty (..))
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Data.Set as E
import Text.Heredoc (here)

import ParseDateDirective as D
import ParseExpenseDirective as E
import ParseExpensesDoc as ED



{-
 Expense directive:
 SPENT | RECEIVED
 AMOUNT: [$] xy.zw [CUR]
 REMARK: [words till EOL]
 & together
-}
parseExpenseDirectiveSpec :: Spec
parseExpenseDirectiveSpec =
  describe "ParseExpenseDirective" $ do
    -- direction Spent/Rcv
    it "should parse 'direction'" $ do
      parse E.direction "" "Spent"    `shouldParse` E.Spent
      parse E.direction "" "Received" `shouldParse` E.Received
    it "should not parse not-direction" $ do
      parse E.direction "" `shouldFailOn` "NotADirection"
      parse E.direction "" `shouldFailOn` "MON"
      parse E.direction "" `shouldFailOn` "2016-02-01 MON"
      parse E.direction "" `shouldFailOn` "Sent"

    -- amount [~] 1[.23] [CUR]
    it "should parse amount (working cases)" $ do
      parse E.amount ""  "1.23"  `shouldParse` E.Amount 1 23 Nothing False
      parse E.amount "" "~1.23"  `shouldParse` E.Amount 1 23 Nothing True
      parse E.amount ""  "1"     `shouldParse` E.Amount 1  0 Nothing False
      -- Note that, if we test for currencies other than USD,
      -- it'll fail. Should remedy that..
      parse E.amount ""  "1 USD" `shouldParse` E.Amount 1  0 (Just "USD") False
    it "should not parse not-amount" $ do
      parse E.amount "" `shouldFailOn` "NotAnAmount"
      parse E.amount "" `shouldFailOn` "S$123"
      parse E.amount "" `shouldFailOn` "$123"
      parse E.amount "" `shouldFailOn` "Spent"
      parse E.amount "" `shouldFailOn` "MON"

    -- expense (dir, amt, remark)
    it "should parse expense directive (working cases)" $ do
      parse E.expense "" "Spent 1.23 on food" `shouldParse` E.Expense E.Spent (E.Amount 1 23 Nothing False) "on food"
    it "should not parse not expense directive" $ do
      parse E.amount "" `shouldFailOn` "NotAnExpenseDirective"

    it "shouldn't consume too much" $ do
      -- consume everything until the newline, for the 'remark'
      runParser' E.expense (initialState "Spent 1 on x\nnext") `succeedsLeaving` "\nnext"

    it "shouldn't consume too much (on failure)" $ do
      -- Preserve all input on case of date directive
      runParser' E.expense (initialState "MON\nnext") `failsLeaving` "MON\nnext"
      runParser' E.expense (initialState "2016-02-01 MON\nnext") `failsLeaving` "2016-02-01 MON\nnext"
      runParser' E.expense (initialState "Sent 3 on x\nnext") `failsLeaving` "Sent 3 on x\nnext"

      -- Comments are taken care of in main
      runParser' E.expense (initialState "#cmt\nnext") `failsLeaving` "#cmt\nnext"

