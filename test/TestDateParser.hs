module TestDateParser where

import Data.List.NonEmpty (NonEmpty (..))

import qualified Data.Set as E

import qualified Data.Time.Calendar as DT

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
        parse PD.dayOfWeek "" "MON"   `shouldParse` DT.Monday
        parse PD.dayOfWeek "" "TUE"   `shouldParse` DT.Tuesday
        parse PD.dayOfWeek "" "TUES"  `shouldParse` DT.Tuesday
        parse PD.dayOfWeek "" "WED"   `shouldParse` DT.Wednesday
        parse PD.dayOfWeek "" "THU"   `shouldParse` DT.Thursday
        parse PD.dayOfWeek "" "THURS" `shouldParse` DT.Thursday
        parse PD.dayOfWeek "" "FRI"   `shouldParse` DT.Friday
        parse PD.dayOfWeek "" "SAT"   `shouldParse` DT.Saturday
        parse PD.dayOfWeek "" "SUN"   `shouldParse` DT.Sunday
      it "should fail to parse non-days (Spent, 2016, etc.)" $ do
        parse PD.dayOfWeek "" `shouldFailOn` "NotADay"
        parse PD.dayOfWeek "" `shouldFailOn` "2016"
        parse PD.dayOfWeek "" `shouldFailOn` "Spent"
        parse PD.dayOfWeek "" `shouldFailOn` "Received"

    describe "date" $ do
      it "should parse cases like yyyy-mm-dd" $ do
        parse PD.date "" "1234-56-78"   `shouldParse` (DT.fromGregorian 1234 56 78)
      it "should fail on non-functional cases" $ do
        parse PD.date "" `shouldFailOn` "NotADate"
        parse PD.date "" `shouldFailOn` "1234"
        parse PD.date "" `shouldFailOn` "1234-56"
        parse PD.date "" `shouldFailOn` "Spent"
        parse PD.date "" `shouldFailOn` "Received"
        parse PD.date "" `shouldFailOn` "MON"

    describe "dateDirective" $ do
      it "should handle cases like 'SUN', etc." $ do
        parse PD.dateDirective "" "SUN" `shouldParse` D.DateDir Nothing DT.Sunday
      it "should handle cases like 'yyyy-mm-dd SUN', etc." $ do
        parse PD.dateDirective "" "1234-56-78 SUN"
          `shouldParse` D.DateDir (Just (DT.fromGregorian 1234 56 78)) DT.Sunday
      it "should handle cases like 'yyyy-mm-dd', etc." $ do
        parse PD.dateDirective "" "2018-01-01"
          `shouldParse` D.DateDir (Just (DT.fromGregorian 2018 01 01)) DT.Monday
        parse PD.dateDirective "" "2019-01-01"
          `shouldParse` D.DateDir (Just (DT.fromGregorian 2019 01 01)) DT.Tuesday
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
