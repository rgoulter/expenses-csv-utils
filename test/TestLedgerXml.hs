{-# LANGUAGE QuasiQuotes #-}

module TestLedgerXml (ledgerXmlSpec) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)

import qualified Text.XML.HXT.Core as HXT

import qualified Data.Expenses.Ledger.Xml as LX
import qualified Data.Expenses.Types as LT



ledgerXmlSpec :: Spec
ledgerXmlSpec =
  describe "Data.Expenses.Ledger.Xml" $ do
    describe "textAsDecimal" $ do
      it "should return digits for simple input" $ do
        HXT.runLA LX.textAsDecimal "0" `shouldBe` [0]
        HXT.runLA LX.textAsDecimal "5" `shouldBe` [5]
        HXT.runLA LX.textAsDecimal "123" `shouldBe` [123]
      it "should parse negative numbers" $ do
        HXT.runLA LX.textAsDecimal "-5" `shouldBe` [-5]
      it "should parse non-integer numbers" $ do
        HXT.runLA LX.textAsDecimal "1.23" `shouldBe` [1.23]
      it "should return empty for non-digit input" $ do
        HXT.runLA LX.textAsDecimal "" `shouldBe` []
        HXT.runLA LX.textAsDecimal "x" `shouldBe` []

    describe "transctionsInXmlDocument" $
      it "should correctly parse transactions" $ do
        let
          document = unindent [i|
            <?xml version="1.0" encoding="utf-8"?>
            <ledger>
              <transactions>
                <transaction>
                  <date>2018/01/01</date>
                  <payee>food</payee>
                  <postings>
                    <posting>
                      <account>
                        <name>Expenses:Food</name>
                      </account>
                      <post-amount>
                        <amount>
                          <commodity flags="S"><symbol>SGD</symbol></commodity>
                          <quantity>5</quantity>
                        </amount>
                      </post-amount>
                    </posting>
                    <posting generated="true">
                      <account><name>Assets:SGD</name></account>
                      <post-amount>
                        <amount>
                          <commodity flags="S"><symbol>SGD</symbol></commodity>
                          <quantity>-5</quantity>
                        </amount>
                      </post-amount>
                    </posting>
                  </postings>
                </transaction>
              </transactions>
            </ledger>
            |]
        LX.transactionsInXmlDocument document
        `shouldBe`
        [ LX.Transaction
            "2018/01/01"
            "food"
            [ LX.Posting "Expenses:Food" (LX.Amount "SGD" 5)
            , LX.Posting "Assets:SGD" (LX.Amount "SGD" (-5))
            ]
        ]

    describe "simpleTransactionFromTransaction" $
      it "simple case" $
        LX.simpleTransactionFromTransaction
          (LX.Transaction
            "2018/01/01"
            "food"
            [ LX.Posting "Expenses:Food" (LX.Amount "SGD" 5)
            , LX.Posting "Assets:SGD" (LX.Amount "SGD" (-5))
            ])
        `shouldBe`
        Just
          (LT.SimpleTransaction
           { LT.transactionDescription = "food"
           , LT.transactionAmount =
               LT.Amount
               { LT.moneyAmount = 5
               , LT.moneyCurrency = Just "SGD"
               , LT.moneyIsApprox = False
               }
           , LT.transactionDebittedAccount = "Expenses:Food"
           , LT.transactionCredittedAccount = "Assets:SGD"
           })
