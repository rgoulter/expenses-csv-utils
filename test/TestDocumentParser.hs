{-# LANGUAGE QuasiQuotes #-}

module TestDocumentParser (parseExpensesFileSpec) where

import Data.Either (isLeft, isRight)

import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)

import Test.Hspec (Spec, context, describe, expectationFailure, it, shouldBe, shouldSatisfy)

import Test.Hspec.Megaparsec
  ( err
  , elabel
  , shouldSucceedOn
  , utoks
  )

import Text.Megaparsec (parse)


import qualified Data.Expenses.Parse.Megaparsec.Document as PED
import qualified Data.Expenses.Parse.Megaparsec.Types as PT
import Data.Expenses.Parse.Megaparsec.Document (modelFromAst)



goodExpensesDoc :: String
goodExpensesDoc =
  unindent [i|
    2016-01-01 MON
    Spent 1 on stuff

    TUE
    Spent 2 on thing
  |]



goodExpensesDocWithCmts :: String
goodExpensesDocWithCmts =
  unindent [i|
    # Comment
    2016-01-01 MON
    Spent 1 on stuff

    # Comment
    TUE
    Spent 2 on thing
    # Comment
  |]



goodExpensesDocWithUsing :: String
goodExpensesDocWithUsing =
  unindent [i|
    Using VND as the default currency

    2016-01-01 MON
    Spent 10k on stuff
  |]



badExpensesDoc :: String
badExpensesDoc =
  unindent [i|
    2016-01-01 MON
    Spent 1 on stuff
    Sent 1.5 on stuff

    TUE
    Spent 2 on thing
  |]



badExpensesDocWithMultipleTypos :: String
badExpensesDocWithMultipleTypos =
  unindent [i|
    2016-01-01 MON
    Spent 1 on stuff
    Sent 1.5 on stuff

    TUE
    spent 2 on thing
  |]



badExpensesDocTypoLastLine :: String
badExpensesDocTypoLastLine =
  unindent [i|
    2016-01-01 MON
    Spent 1 on stuff
    Sent 1.5 on stuff|]



parseExpensesFileSpec :: Spec
parseExpensesFileSpec =
  describe "Data.Expenses.Parse.Megaparsec.ExpensesDoc" $
    describe "parseExpensesFile" $ do
      context "should successfully parse well-formed doc" $ do
        let shouldSuccessfullyParseDocument p d = do
              parse p "" `shouldSucceedOn` d
              let rawResult = parse p "" d
              case rawResult of
                Left _ -> expectationFailure "parser shouldn't fail"
                Right (PT.AST rawLines) -> rawLines `shouldSatisfy` all isRight
        it "simple document" $
          docParser `shouldSuccessfullyParseDocument` goodExpensesDoc
        it "document with comments" $
          docParser `shouldSuccessfullyParseDocument` goodExpensesDocWithCmts
        it "document with Using directive" $
          docParser `shouldSuccessfullyParseDocument` goodExpensesDocWithUsing

      context "recovers parsing malformed doc" $ do
        let shouldRecoverWithAtLeastOneError p d = do
              parse p "" `shouldSucceedOn` d
              let rawResult = parse p "" d
              case rawResult of
                Left _ -> expectationFailure "parser shouldn't fail"
                Right (PT.AST rawLines) -> rawLines `shouldSatisfy` any isLeft
        it "malformed with simple typo" $
          docParser `shouldRecoverWithAtLeastOneError` badExpensesDoc
        it "malformed typo on last line (no EOL)" $
          docParser `shouldRecoverWithAtLeastOneError` badExpensesDocTypoLastLine

      -- XXX: need to update these to assert that the Lefts of the
      -- raw result are as asserted!
      describe "parse error for 1x error (typo 'Sent')" $
        it "should show unexpected \"Sent\", expected \"Spent\" or \"Received\"" $ do
          let rawResult = parse docParser "" badExpensesDoc

          case rawResult of
            Left _ -> expectationFailure "parser shouldn't fail"
            Right ast ->
              case modelFromAst ast of
                Left errors ->
                  errors
                    `shouldBe`
                      [ err 32    -- row 4, col 1
                        -- Extra space b/c 'Using' parser not as strict as directives
                        (utoks "Sent " <>
                         elabel "Date directive" <>
                         elabel "Expense directive" <>
                         elabel "Using directive")
                      ]
                Right _ ->
                  expectationFailure "should have recovered from an error"

      describe "parse error for 2x error (typos: 'Sent', 'spent')" $
        it "should show unexpected \"Sent\" and \"spent\", expected \"Spent\" or \"Received\"" $ do
          let rawResult = parse docParser "" badExpensesDocWithMultipleTypos

          case rawResult of
            Left _ -> expectationFailure "parser shouldn't fail"
            Right ast ->
              case modelFromAst ast of
                Left errors ->
                  errors
                    `shouldBe`
                      [ err 32    -- row 4, col 1
                        -- Extra space b/c 'Using' parser not as strict as directives
                        (utoks "Sent " <>
                         elabel "Date directive" <>
                         elabel "Expense directive" <>
                         elabel "Using directive")
                      , err 55    -- row 7, col 1
                        (utoks "spent" <>
                         elabel "Date directive" <>
                         elabel "Expense directive" <>
                         elabel "Using directive")
                      ]
                Right _ ->
                  expectationFailure "should have recovered from an error"
    where
      docParser = PED.parseExpensesFile
