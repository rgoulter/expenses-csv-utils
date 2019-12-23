module TestQuery where

import Test.Hspec (Spec, describe, it, shouldBe)

import qualified Data.Expenses.Query as Q
import Data.Expenses.Types (Entry(..), QueryAttribute(..))

sampleEntries :: [Entry]
sampleEntries =
  [ Entry (2018, 01, 01) (100, "SGD") "on McDonalds" Nothing
  , Entry (2018, 06, 15) (100, "SGD") "on McDonalds" Nothing
  ]

querySpec :: Spec
querySpec =
  describe "Data.Expenses.Query" $ do
    describe "attr" $
      it "should parse 'earliest' as Earliest, 'latest' as Lastest" $ do
        Q.attr "earliest" `shouldBe` Just Earliest
        Q.attr "latest" `shouldBe` Just Latest
    describe "queryDirectives" $
      it "should compute latest dates" $ do
        Q.queryDirectives Earliest sampleEntries `shouldBe` "2018-01-01"
        Q.queryDirectives Latest sampleEntries `shouldBe` "2018-06-15"
