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

import TestDateParser
import TestExpenseParser
import TestExpenseDocParser
import TestMergeEntries
import TestCategoriseEntries

-- Adapted from
-- https://raw.githubusercontent.com/mrkkrp/hspec-megaparsec/0.2.0/tests/Main.hs



-- hspec :: Spec -> IO ()

main :: IO ()
main = hspec $ do
  parseDateDirectiveSpec
  parseExpenseDirectiveSpec
  parseExpensesFileSpec
  entryListMerge
  categoriseExpensesSpec

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
