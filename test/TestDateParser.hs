module TestDateParser where

import Data.List.NonEmpty (NonEmpty (..))

import qualified Data.Set as E

import Text.Heredoc (here)

import Test.Hspec (Spec, describe, it)

import Test.Hspec.Megaparsec
  (failsLeaving, initialState, shouldFailOn, shouldParse, succeedsLeaving)

import Text.Megaparsec (parse, runParser')


import Data.Expenses.Parse.Megaparsec.Types (Parser)
import qualified Data.Expenses.Parse.Megaparsec.DateDirective as PD
import qualified Data.Expenses.Expense as D



{-
 Date-Directive
 DAY : MON, TUE, etc.
 DATE yyyy-mm-dd
 (& together, as a 'date directive')
-}
parseDateDirectiveSpec :: Spec
parseDateDirectiveSpec =
  describe "Data.Expenses.Parse.Megaparsec.DateDirective" $ do
    describe "day" $ do
      it "should parse cases like MON, TUE, etc." $ do
        parse PD.day "" "MON"   `shouldParse` D.Mon
        parse PD.day "" "TUE"   `shouldParse` D.Tue
        parse PD.day "" "TUES"  `shouldParse` D.Tue
        parse PD.day "" "WED"   `shouldParse` D.Wed
        parse PD.day "" "THU"   `shouldParse` D.Thu
        parse PD.day "" "THURS" `shouldParse` D.Thu
        parse PD.day "" "FRI"   `shouldParse` D.Fri
        parse PD.day "" "SAT"   `shouldParse` D.Sat
        parse PD.day "" "SUN"   `shouldParse` D.Sun
      it "should fail to parse non-days (Spent, 2016, etc.)" $ do
        parse PD.day "" `shouldFailOn` "NotADay"
        parse PD.day "" `shouldFailOn` "2016"
        parse PD.day "" `shouldFailOn` "Spent"
        parse PD.day "" `shouldFailOn` "Received"

    describe "date" $ do
      it "should parse cases like yyyy-mm-dd" $ do
        parse PD.date "" "1234-56-78"   `shouldParse` (1234,56,78)
      it "should fail on non-functional cases" $ do
        parse PD.date "" `shouldFailOn` "NotADate"
        parse PD.date "" `shouldFailOn` "1234"
        parse PD.date "" `shouldFailOn` "1234-56"
        parse PD.date "" `shouldFailOn` "Spent"
        parse PD.date "" `shouldFailOn` "Received"
        parse PD.date "" `shouldFailOn` "MON"

    describe "dateDirective" $ do
      it "should handle cases like 'yyyy-mm-dd SUN', 'SUN', etc." $ do
        parse PD.dateDirective "" "SUN" `shouldParse` D.DateDir Nothing D.Sun
        parse PD.dateDirective "" "1234-56-78 SUN"
          `shouldParse` D.DateDir (Just (1234,56,78)) D.Sun
      it "should fail to parse not-dateDirective" $ do
        parse PD.dateDirective "" `shouldFailOn` "NotADateDirective"
        parse PD.dateDirective "" `shouldFailOn` "Spent"
        parse PD.dateDirective "" `shouldFailOn` "MO"
        -- n.b. "SUN 1234-56-78" consumes the "SUN", so not a failure case.

      it "shouldn't consume too much" $ do
        runParser' PD.dateDirective (initialState "MON \nnext")
          `succeedsLeaving` " \nnext"
        runParser' PD.dateDirective (initialState "FRI \nnext")
          `succeedsLeaving` " \nnext"
        runParser' PD.dateDirective (initialState "SAT \nnext")
          `succeedsLeaving` " \nnext"
        runParser' PD.dateDirective (initialState "SUN \nnext")
          `succeedsLeaving` " \nnext"

        -- Tues, Weds, Thurs consume *until* newline.
        runParser' PD.dateDirective (initialState "TUE   \nnext")
          `succeedsLeaving` "\nnext"
        runParser' PD.dateDirective (initialState "TUES  \nnext")
          `succeedsLeaving` "\nnext"
        runParser' PD.dateDirective (initialState "WED   \nnext")
          `succeedsLeaving` "\nnext"
        runParser' PD.dateDirective (initialState "WEDS  \nnext")
          `succeedsLeaving` "\nnext"
        runParser' PD.dateDirective (initialState "THU   \nnext")
          `succeedsLeaving` "\nnext"
        runParser' PD.dateDirective (initialState "THURS \nnext")
          `succeedsLeaving` "\nnext"

      it "shouldn't consume too much (on failure)" $ do
        runParser' PD.dateDirective (initialState "Spent 3 on whatever")
          `failsLeaving` "Spent 3 on whatever"
        runParser' PD.dateDirective (initialState "Sent etc.")
          `failsLeaving` "Sent etc."
