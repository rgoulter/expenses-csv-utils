{-# LANGUAGE QuasiQuotes #-}

module TestExpenseDocParser where

import Data.List.NonEmpty (NonEmpty (..))

import qualified Data.Set as E

import Text.Heredoc (here)

import Test.Hspec (Spec, describe, it)

import Test.Hspec.Megaparsec
  ( err
  , utok
  , utoks
  , eeof
  , elabel
  , initialState
  , shouldSucceedOn
  , shouldFailOn
  , shouldFailWith
  , shouldParse)

import Text.Megaparsec (SourcePos(..), eof, mkPos, parse)


import qualified Data.Expenses.Parse.Megaparsec.ExpensesDoc as PED



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



badExpensesDocWithMultipleTypos :: String
badExpensesDocWithMultipleTypos = [here|
2016-01-01 MON
Spent 1 on stuff
Sent 1.5 on stuff

TUE
spent 2 on thing
|]



parseExpensesFileSpec :: Spec
parseExpensesFileSpec =
  describe "Data.Expenses.Parse.Megaparsec.ExpensesDoc" $ do
    describe "parseExpensesFile" $ do
      -- direction Spent/Rcv
      it "should successfully parse well-formed doc" $ do
        parse docParser "" `shouldSucceedOn` goodExpensesDoc
        parse docParser "" `shouldSucceedOn` goodExpensesDocWithCmts
      it "should fail to pass malformed doc" $ do
        parse docParser "" `shouldFailOn` badExpensesDoc

      describe "parse error for 1x error (typo 'Sent')" $ do
        it "should show unexpected \"Sent\", expected \"Spent\" or \"Received\"" $ do
          parse docParser "" badExpensesDoc
          `shouldFailWith` err (mkSrcPos 4 1)
                               (utoks "Sent" <>
                                elabel "Date directive" <>
                                elabel "Expense directive" <>
                                eeof)

      describe "parse error for 2x error (typos: 'Sent', 'spent')" $ do
        it "should show unexpected \"Sent\", expected \"Spent\" or \"Received\"" $ do
          parse docParser "" badExpensesDocWithMultipleTypos
          `shouldFailWith` err (mkSrcPos 4 1)
                               (utoks "Sent" <>
                                elabel "Date directive" <>
                                elabel "Expense directive" <>
                                eeof)
    where
      docParser = PED.parseExpensesFile
      mkSrcPos r c = (SourcePos "" (mkPos r) (mkPos c)) :| []
