{-# LANGUAGE QuasiQuotes #-}

module TestExpenseDocParser where

import Data.List.NonEmpty (NonEmpty (..))

import qualified Data.Set as E

import Text.Heredoc (here)

import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

import Test.Hspec.Megaparsec
  ( eeof
  , err
  , elabel
  , initialState
  , shouldSucceedOn
  , shouldFailOn
  , shouldFailWith
  , shouldParse
  , utok
  , utoks
  )

import Text.Megaparsec (SourcePos(..), eof, mkPos, parse)


import qualified Data.Expenses.Parse.Megaparsec.ExpensesDoc as PED
import Data.Expenses.Parse.Megaparsec.ExpensesDoc (eitherOfLists)



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
      it "does not fail to parse malformed doc" $ do
        -- n.b. it should successfully return a list with at least one Left.
        parse docParser "" `shouldSucceedOn` badExpensesDoc

      -- XXX: need to update these to assert that the Lefts of the
      -- raw result are as asserted!
      describe "parse error for 1x error (typo 'Sent')" $ do
        it "should show unexpected \"Sent\", expected \"Spent\" or \"Received\"" $ do
          let rawResult = parse docParser "" badExpensesDoc

          case rawResult of
            Left _ -> expectationFailure "parser shouldn't fail"
            Right rawDirectives ->
              case eitherOfLists rawDirectives of
                Left errors ->
                  errors
                    `shouldBe`
                      [ err 33    -- row 4, col 1
                        (utoks "Sent" <>
                         elabel "Date directive" <>
                         elabel "Expense directive")
                      ]
                Right _ ->
                  expectationFailure "should have recovered from an error"

      describe "parse error for 2x error (typos: 'Sent', 'spent')" $ do
        it "should show unexpected \"Sent\" and \"spent\", expected \"Spent\" or \"Received\"" $ do
          let rawResult = parse docParser "" badExpensesDocWithMultipleTypos

          case rawResult of
            Left _ -> expectationFailure "parser shouldn't fail"
            Right rawDirectives ->
              case eitherOfLists rawDirectives of
                Left errors ->
                  errors
                    `shouldBe`
                      [ err 33    -- row 4, col 1
                        (utoks "Sent" <>
                         elabel "Date directive" <>
                         elabel "Expense directive")
                      , err 56    -- row 7, col 1
                        (utoks "spent" <>
                         elabel "Date directive" <>
                         elabel "Expense directive")
                      ]
                Right _ ->
                  expectationFailure "should have recovered from an error"
    where
      docParser = PED.parseExpensesFile
