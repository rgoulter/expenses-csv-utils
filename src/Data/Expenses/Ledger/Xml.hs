{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module Data.Expenses.Ledger.Xml
  ( Amount(..)
  , Posting(..)
  , Transaction(..)
  , transactionsInXmlDocument
  , simpleTransactionFromTransaction
  , simpleTransactionsInXmlDocument
  , textAsDecimal
  ) where

import qualified Data.Decimal as D

import Data.Maybe (mapMaybe, maybeToList)

import qualified Text.XML.HXT.Core as HXT
import Text.XML.HXT.Core ((/>), (>>>), (<<<))
import Text.XML.HXT.DOM.TypeDefs (XmlTree)

import Text.Read (readMaybe)

import qualified Data.Expenses.Types as ET



-- | Data type for the @\<amount\>@ element in a ledger XML document.
data Amount = Amount { amountCurrency :: String, amountQuantity :: D.Decimal }
  deriving (Show, Eq)



-- | Data type for the @\<posting\>@ element in a ledger XML document.
data Posting = Posting { postingAccount :: String, postingAmount :: Amount }
  deriving (Show, Eq)



-- | Data type for the @\<transaction\>@ element in a ledger XML document.
data Transaction = Transaction
  { transactionDate :: String
  , transactionPayee :: String
  , transactionPostings :: [Posting]
  }
  deriving (Show, Eq)



getChildText :: HXT.ArrowXml a => String -> a XmlTree String
getChildText child =
  proc f -> do
    node <- HXT.hasName child <<< HXT.getChildren -< f
    s <- HXT.getText <<< HXT.getChildren -< node
    HXT.returnA -< s



getNestedText :: HXT.ArrowXml a => String -> String -> a XmlTree String
getNestedText child1 child2 =
  proc f -> do
    node1 <- HXT.hasName child1 <<< HXT.getChildren -< f
    node2 <- HXT.hasName child2 <<< HXT.getChildren -< node1
    s <- HXT.getText <<< HXT.getChildren -< node2
    HXT.returnA -< s



-- | Results in a parsed number, or empty if the string is not numeric.
textAsDecimal :: HXT.ArrowList a => a String D.Decimal
textAsDecimal =
  HXT.arrL $ maybeToList . readMaybe



-- | An 'Amount' from an @\<amount\>@ XML element.
amount  :: HXT.ArrowXml a => a XmlTree Amount
amount =
  HXT.hasName "amount" >>>
  proc a -> do
    amtCurrency <- getNestedText "commodity" "symbol" -< a
    amtQuantity <- textAsDecimal <<< getChildText "quantity" -< a
    HXT.returnA -< Amount { amountCurrency = amtCurrency
                          , amountQuantity = amtQuantity
                          }



-- | An 'Amount' from a @\<posting\>@ XML element.
postAmount :: HXT.ArrowXml a => a XmlTree Amount
postAmount =
  HXT.hasName "posting" />
  HXT.hasName "post-amount" />
  HXT.hasName "amount" >>>
  amount



-- | 'Posting's from a given @\<transaction\>@ element.
postings :: HXT.ArrowXml a => a XmlTree [Posting]
postings =
  HXT.listA $
  HXT.hasName "transaction" />
  HXT.hasName "postings" />
  HXT.hasName "posting" >>>
  proc p -> do
    pstAccount <- getNestedText "account" "name" -< p
    pstAmount <- postAmount -< p
    HXT.returnA -< Posting { postingAccount = pstAccount
                           , postingAmount = pstAmount
                           }



-- | 'Transaction' from a @\<transaction\>@ element.
transaction :: HXT.ArrowXml a => a XmlTree Transaction
transaction =
  HXT.hasName "transaction" >>>
  proc txn -> do
    txnPayee <- getChildText "payee" -< txn
    txnDate <- getChildText "date" -< txn
    txnPostings <- postings -< txn
    HXT.returnA -< Transaction { transactionDate = txnDate
                               , transactionPayee = txnPayee
                               , transactionPostings = txnPostings
                               }



-- | 'Transaction's from a @\<ledger\>@ XML document.
transactions :: HXT.ArrowXml a => a XmlTree Transaction
transactions =
  HXT.hasName "ledger" />
  HXT.hasName "transactions" />
  HXT.hasName "transaction" >>>
  transaction



-- | 'Transaction's from an XML document.
transactionsInXmlDocument :: String -> [Transaction]
transactionsInXmlDocument =
  HXT.runLA (HXT.xreadDoc >>> transactions)



-- | 'SimpleTransaction's from an XML document.
simpleTransactionsInXmlDocument :: String -> [ET.SimpleTransaction]
simpleTransactionsInXmlDocument doc =
  mapMaybe simpleTransactionFromTransaction $ transactionsInXmlDocument doc



simpleTransactionFromTransaction :: Transaction -> Maybe ET.SimpleTransaction
-- Pattern match Transaction for case with two postings.
simpleTransactionFromTransaction
  Transaction
  { transactionPayee = payee
  , transactionPostings =
      [ Posting
        { postingAccount = acct0
        , postingAmount =
            Amount
            { amountCurrency = currency0
            , amountQuantity = quantity0
            }
        }
      , Posting
        { postingAccount = acct1
        , postingAmount =
            Amount
            { amountCurrency = currency1
            , amountQuantity = quantity1
            }
        }
      ]
  }

  -- Guard the two postings have suitable, simple values.
  -- (Same currency and same amount).
  | currency0 == currency1 && quantity0 == -1 * quantity1 =
    let
      (debittedAccount, credittedAccount) =
        if quantity0 > 0 then
          (acct0, acct1)
        else
          (acct1, acct0)
    in
    Just $
      ET.SimpleTransaction
      { ET.transactionDescription = payee
      , ET.transactionAmount = ET.Amount
                               { ET.moneyAmount = quantity0
                               , ET.moneyCurrency = Just currency0
                               , ET.moneyIsApprox = False
                               }
      , ET.transactionDebittedAccount = debittedAccount
      , ET.transactionCredittedAccount = credittedAccount
      }

  | otherwise = Nothing

simpleTransactionFromTransaction _ = Nothing
