module Main (main) where

import Data.List.NonEmpty (NonEmpty (..))
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Data.Set as E

import ParseDateDirective as D

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
    -- it "should parse date" $
    -- it "should parse dateDirective" $



{-
 Expense directive:
 SPENT | RECEIVED
 AMOUNT: [$] xy.zw [CUR]
 REMARK: [words till EOL]
 & together
-}
-- parseExpenseDirective :: Spec
-- parseExpenseDirective =
--   ???



-- hspec :: Spec -> IO ()

main :: IO ()
main = hspec $ do
  parseDateDirectiveSpec
  -- parseExpenseDirective

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
