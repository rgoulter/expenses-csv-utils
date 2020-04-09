{-# LANGUAGE QuasiQuotes #-}

module TestLedgerProcess (ledgerProcessSpec) where

import Test.Hspec (Spec, describe, it, shouldReturn)

import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)

import Text.RE.Replace
import Text.RE.TDFA.String

import qualified Data.Expenses.Ledger.Process as LP



{-|
  Removes @version=".*"@, @id=".*"@, @ref=".*"@
  attributes from the output of the @ledger xml@ command,
  since these are non-deterministically generated.
|-}
stripUnstableAttributes :: String -> String
stripUnstableAttributes xml =
  let
    mtch = xml *=~ [re| (id|ref|version)="[^"]*">|]
  in
  replaceAll ">" mtch


-- | Replace CRLF with LF.
normalizeNewlines :: String -> String
normalizeNewlines s =
  let
    mtch = s *=~ [re|\r\n|]
  in
  replaceAll "\n" mtch


ledgerProcessSpec :: Spec
ledgerProcessSpec =
  describe "Data.Expenses.Ledger.Process" $
    describe "xmlOfString" $
      it "should return digits for simple input" $
        stripUnstableAttributes . normalizeNewlines <$> LP.xmlOfString simpleLedgerJournal
          `shouldReturn` simpleLedgerXml
    where
  simpleLedgerJournal = unindent [i|
      bucket Assets:SGD

      2018/01/01 food
        Expenses:Food  5 SGD

    |]
  simpleLedgerXml = unindent [i|
    <?xml version="1.0" encoding="utf-8"?>
    <ledger>
      <commodities>
        <commodity flags="S">
          <symbol>SGD</symbol>
        </commodity>
      </commodities>
      <accounts>
        <account>
          <name/>
          <fullname/>
          <account-total>
            <amount>
              <commodity flags="S">
                <symbol>SGD</symbol>
              </commodity>
              <quantity>0</quantity>
            </amount>
          </account-total>
          <account>
            <name>Assets</name>
            <fullname>Assets</fullname>
            <account-total>
              <amount>
                <commodity flags="S">
                  <symbol>SGD</symbol>
                </commodity>
                <quantity>-5</quantity>
              </amount>
            </account-total>
            <account>
              <name>SGD</name>
              <fullname>Assets:SGD</fullname>
              <account-amount>
                <amount>
                  <commodity flags="S">
                    <symbol>SGD</symbol>
                  </commodity>
                  <quantity>-5</quantity>
                </amount>
              </account-amount>
              <account-total>
                <amount>
                  <commodity flags="S">
                    <symbol>SGD</symbol>
                  </commodity>
                  <quantity>-5</quantity>
                </amount>
              </account-total>
            </account>
          </account>
          <account>
            <name>Expenses</name>
            <fullname>Expenses</fullname>
            <account-total>
              <amount>
                <commodity flags="S">
                  <symbol>SGD</symbol>
                </commodity>
                <quantity>5</quantity>
              </amount>
            </account-total>
            <account>
              <name>Food</name>
              <fullname>Expenses:Food</fullname>
              <account-amount>
                <amount>
                  <commodity flags="S">
                    <symbol>SGD</symbol>
                  </commodity>
                  <quantity>5</quantity>
                </amount>
              </account-amount>
              <account-total>
                <amount>
                  <commodity flags="S">
                    <symbol>SGD</symbol>
                  </commodity>
                  <quantity>5</quantity>
                </amount>
              </account-total>
            </account>
          </account>
        </account>
      </accounts>
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
                  <commodity flags="S">
                    <symbol>SGD</symbol>
                  </commodity>
                  <quantity>5</quantity>
                </amount>
              </post-amount>
              <total>
                <amount>
                  <commodity flags="S">
                    <symbol>SGD</symbol>
                  </commodity>
                  <quantity>5</quantity>
                </amount>
              </total>
            </posting>
            <posting generated="true">
              <account>
                <name>Assets:SGD</name>
              </account>
              <post-amount>
                <amount>
                  <commodity flags="S">
                    <symbol>SGD</symbol>
                  </commodity>
                  <quantity>-5</quantity>
                </amount>
              </post-amount>
              <total>
                <amount>
                  <commodity flags="S">
                    <symbol>SGD</symbol>
                  </commodity>
                  <quantity>0</quantity>
                </amount>
              </total>
            </posting>
          </postings>
        </transaction>
      </transactions>
    </ledger>

    |]
