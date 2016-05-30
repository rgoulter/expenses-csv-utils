module Main (main) where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Error (newErrorMessages)
import Text.Megaparsec.Pos (initialPos)

-- Adapted from
-- https://raw.githubusercontent.com/mrkkrp/hspec-megaparsec/0.1.1/tests/Main.hs
-- 0.1.1 version is for MegaParsec 0.4.4

main :: IO ()
main = hspec $ do
  describe "shouldParse" $
    it "works" $
      parse letterChar "" "x" `shouldParse` 'x'
  describe "parseSatisfies" $
    it "works" $
      parse (many punctuationChar) "" "?!!" `parseSatisfies` ((== 3) . length)
  describe "shouldFailOn" $
    it "works" $
      parse (char 'x') "" `shouldFailOn` "a"
  describe "shouldSucceedOn" $
    it "works" $
      parse (char 'x') "" `shouldSucceedOn` "x"
  describe "shouldFailWith" $
    it "works" $
      parse (char 'x') "" "b" `shouldFailWith`
        newErrorMessages [Unexpected "'b'", Expected "'x'"] (initialPos "")
  describe "failsLeaving" $
    it "works" $
      runParser' (many (char 'x') <* eof) (initialState "xxa")
        -- n.b. with MegaParsec >= 0.4.4, the remaining input is "a"
        -- but we're currently only using < 0.4.4
        `failsLeaving` "xxa"
  describe "succeedsLeaving" $
    it "works" $
      runParser' (many (char 'x')) (initialState "xxa")
        `succeedsLeaving` "a"
