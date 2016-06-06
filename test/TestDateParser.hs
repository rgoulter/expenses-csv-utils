module TestDateParser where

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

