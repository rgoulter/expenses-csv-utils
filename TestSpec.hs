{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

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

-- Adapted from
-- https://raw.githubusercontent.com/mrkkrp/hspec-megaparsec/0.2.0/tests/Main.hs



{-
 Date-Directive
 DAY : MON, TUE, etc.
 DATE yyyy-mm-dd
 (& together, as a 'date directive')
-}
parseDateDirectiveSpec :: Spec
parseDateDirectiveSpec =
  describe "ParseDateDirective" $ do
    it "should parse day (working cases)" $ do
      parse D.day "" "MON"   `shouldParse` D.Mon
      parse D.day "" "TUE"   `shouldParse` D.Tue
      parse D.day "" "TUES"  `shouldParse` D.Tue
      parse D.day "" "WED"   `shouldParse` D.Wed
      parse D.day "" "THU"   `shouldParse` D.Thu
      parse D.day "" "THURS" `shouldParse` D.Thu
      parse D.day "" "FRI"   `shouldParse` D.Fri
      parse D.day "" "SAT"   `shouldParse` D.Sat
      parse D.day "" "SUN"   `shouldParse` D.Sun
    it "should not parse not-day" $ do
      parse D.day "" `shouldFailOn` "NotADay"
      parse D.day "" `shouldFailOn` "2016"
      parse D.day "" `shouldFailOn` "Spent"
      parse D.day "" `shouldFailOn` "Received"

    it "should parse date" $ do
      parse D.date "" "1234-56-78"   `shouldParse` (1234,56,78)
    it "should not parse not-date" $ do
      parse D.date "" `shouldFailOn` "NotADate"
      parse D.date "" `shouldFailOn` "1234"
      parse D.date "" `shouldFailOn` "1234-56"
      parse D.date "" `shouldFailOn` "Spent"
      parse D.date "" `shouldFailOn` "Received"
      parse D.date "" `shouldFailOn` "MON"

    it "should parse dateDirective" $ do
      parse D.dateDirective "" "SUN" `shouldParse` D.DateDir Nothing D.Sun
      parse D.dateDirective "" "1234-56-78 SUN" `shouldParse` D.DateDir (Just (1234,56,78)) D.Sun
    it "should not parse not-dateDirective" $ do
      parse D.dateDirective "" `shouldFailOn` "NotADateDirective"
      parse D.dateDirective "" `shouldFailOn` "Spent"
      parse D.dateDirective "" `shouldFailOn` "MO"
      -- n.b. "SUN 1234-56-78" consumes the "SUN", so not a failure case.

    it "shouldn't consume too much" $ do
      runParser' D.dateDirective (initialState "MON \nnext") `succeedsLeaving` " \nnext"
      runParser' D.dateDirective (initialState "FRI \nnext") `succeedsLeaving` " \nnext"
      runParser' D.dateDirective (initialState "SAT \nnext") `succeedsLeaving` " \nnext"
      runParser' D.dateDirective (initialState "SUN \nnext") `succeedsLeaving` " \nnext"

      -- Tues, Weds, Thurs consume *until* newline.
      runParser' D.dateDirective (initialState "TUE   \nnext") `succeedsLeaving` "\nnext"
      runParser' D.dateDirective (initialState "TUES  \nnext") `succeedsLeaving` "\nnext"
      runParser' D.dateDirective (initialState "WED   \nnext") `succeedsLeaving` "\nnext"
      runParser' D.dateDirective (initialState "WEDS  \nnext") `succeedsLeaving` "\nnext"
      runParser' D.dateDirective (initialState "THU   \nnext") `succeedsLeaving` "\nnext"
      runParser' D.dateDirective (initialState "THURS \nnext") `succeedsLeaving` "\nnext"

    it "shouldn't consume too much (on failure)" $ do
      runParser' D.dateDirective (initialState "Spent 3 on whatever") `failsLeaving` "Spent 3 on whatever"
      runParser' D.dateDirective (initialState "Sent etc.") `failsLeaving` "Sent etc."



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



goodExpensesDoc :: String
goodExpensesDoc = [here|
2016-01-01 MON
Spent 1 on stuff

TUE
Spent 2 on thing
|]

goodExpensesDocWithCmts :: String
goodExpensesDocWithCmts = [here|
# Comment
2016-01-01 MON
Spent 1 on stuff

# Comment
TUE
Spent 2 on thing
# Comment
|]

badExpensesDoc :: String
badExpensesDoc = [here|
2016-01-01 MON
Spent 1 on stuff
Sent 1.5 on stuff

TUE
Spent 2 on thing
|]

-- XXX do a couple of working examples,
-- XXX and, like. do some 'failed' examples, which prev. failed.
parseExpensesFileSpec :: Spec
parseExpensesFileSpec =
  describe "Parse Expenses File" $ do
    -- direction Spent/Rcv
    it "should parse well-formed doc" $ do
      parse docParser "" `shouldSucceedOn` goodExpensesDoc
      parse docParser "" `shouldSucceedOn` goodExpensesDocWithCmts
    it "should not parse malformed doc" $ do
      parse docParser "" `shouldFailOn` badExpensesDoc
  where
    docParser = ED.parseExpensesFile <* eof



-- hspec :: Spec -> IO ()

main :: IO ()
main = hspec $ do
  parseDateDirectiveSpec
  parseExpenseDirectiveSpec
  parseExpensesFileSpec

sample :: Spec
sample = do
  describe "shouldParse" $
    it "works" $
      parse (letterChar :: Parser Char) "" "x" `shouldParse` 'x'
  describe "parseSatisfies" $
    it "works" $
      parse (many punctuationChar :: Parser String) "" "?!!"
        `parseSatisfies` ((== 3) . length)
  describe "shouldFailOn" $
    it "works" $
      parse (char 'x' :: Parser Char) "" `shouldFailOn` "a"
  describe "shouldSucceedOn" $
    it "works" $
      parse (char 'x' :: Parser Char) "" `shouldSucceedOn` "x"
  describe "shouldFailWith" $
    it "works" $
      parse (char 'x' :: Parser Char) "" "b" `shouldFailWith`
        ParseError
          { errorPos        = initialPos "" :| []
          , errorUnexpected = E.singleton (Tokens $ 'b' :| [])
          , errorExpected   = E.singleton (Tokens $ 'x' :| [])
          , errorCustom     = E.empty }

  describe "failsLeaving" $
    it "works" $
      runParser' (many (char 'x') <* eof :: Parser String) (initialState "xxa")
        `failsLeaving` "a"
  describe "succeedsLeaving" $
    it "works" $
      runParser' (many (char 'x') :: Parser String) (initialState "xxa")
        `succeedsLeaving` "a"
