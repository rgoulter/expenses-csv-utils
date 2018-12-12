{-# LANGUAGE QuasiQuotes #-}

module TestExpenseDocParser where

import Data.List.NonEmpty (NonEmpty (..))

import qualified Data.Set as E

import Text.Heredoc (here)

import Test.Hspec (Spec, describe, it)

import Test.Hspec.Megaparsec (initialState, shouldSucceedOn, shouldFailOn, shouldParse)

import Text.Megaparsec (eof, parse)


import qualified ParseExpensesDoc as PED



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
    docParser = PED.parseExpensesFile <* eof

