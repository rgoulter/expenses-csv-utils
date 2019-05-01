{-# LANGUAGE QuasiQuotes #-}

module Data.Expenses.Ledger where

import Data.List (groupBy, intercalate)
import Data.Maybe (mapMaybe)

import Data.String.Interpolate (i)
import Data.String.Interpolate.Util (unindent)

import qualified Data.Text as T
import qualified Data.Text.IO as TI

import qualified Data.Time.Calendar as DT

import qualified Hledger.Data.Posting as HDP
import qualified Hledger.Data.Transaction as HDT
import qualified Hledger.Data.Types as HT
import Hledger.Read (readJournal')

import Text.Printf (printf)

import Data.Expenses.Types (Money(..), SimpleTransaction(..))
import Data.Expenses.Parse.Megaparsec.Entry
  (Entry(..), entryComment, entryDate, entryPrice, entryRemark)
import Data.Expenses.Parse.Megaparsec.ExpensesDoc (entriesFromDirectives)
import Data.Expenses.Parse.Megaparsec.Types (LineDirective)



readJournal :: FilePath -> IO HT.Journal
readJournal filepath = do
  content <- TI.readFile filepath
  readJournal' content



moneyFromLedgerAmount :: HT.MixedAmount -> Maybe Money
moneyFromLedgerAmount (HT.Mixed [a]) =
  -- So, assume: only one amount
  let (dollars, cents) = properFraction $ HT.aquantity a
      cur = T.unpack $ HT.acommodity a
  in Just Amount
     { moneyDollar = dollars
     , moneyCents = round (cents * 100)
     , moneyCurrency = Just cur
     , moneyIsApprox = False
     }
moneyFromLedgerAmount _ = Nothing



simpleTransactionFromTransaction :: HT.Transaction -> Maybe SimpleTransaction
simpleTransactionFromTransaction t =
  let postings = HDT.realPostings t
  in if length postings == 2 then
       -- ASSUMPTION: First posting is credit, second posting is debit
       let description = T.unpack $ HT.tdescription t
           [credPosting, _debPosting] = postings
           [credAccount, debAccount] = map (T.unpack . HT.paccount) postings
           credAmount = HT.pamount credPosting
       in fmap
          (\money ->
             SimpleTransaction
             { transactionDescription = description
             , transactionAmount = money
             , transactionCredittedAccount = credAccount
             , transactionDebittedAccount = debAccount
             })
          (moneyFromLedgerAmount credAmount)
     else
       Nothing



simpleTransactionsInJournal :: HT.Journal -> [SimpleTransaction]
simpleTransactionsInJournal j =
  mapMaybe simpleTransactionFromTransaction (HT.jtxns j)



directiveFromEntry :: Entry -> String
directiveFromEntry Entry
                   { entryDate = (y, m, d)
                   , entryPrice = (dollars, cents, currency)
                   , entryRemark = remark
                   } =
  [i|#{direction} #{dollars}.#{cents} #{currency} #{remark}|]
    where
      direction = if (dollars >= 0) then "Spent" else "Received"



showEntryDate :: Entry -> String
showEntryDate Entry { entryDate = (y, m, d) } =
  printf "%4d-%02d-%02d" y m d



showEntryDateWithDay :: Entry -> String
showEntryDateWithDay Entry { entryDate = (y, m, d) } =
  printf "%4d-%02d-%02d %s" y m d day
    where
      day = show $ DT.dayOfWeek $ DT.fromGregorian (fromIntegral y) m d



showLedgerTransactionFromEntry :: Entry -> String
showLedgerTransactionFromEntry entry =
  unindent [i|
  # #{originalDirective}
  #{date} #{remark}
    Undescribed  #{price} #{cur}
    Assets:Cash:#{cur}|] ++ cmt
  where
    (y, m, d) = entryDate entry
    date   = showEntryDate entry
    (dollars, cents, cur) = entryPrice entry
    price  = printf "%d.%d" dollars cents :: String
    remark = entryRemark entry
    cmt = maybe "" ((:) '\n') $ entryComment entry
    originalDirective = directiveFromEntry entry



equalBy :: Eq b => (a -> b) -> (a -> a -> Bool)
equalBy f a1 a2 =
  f a1 == f a2



showLedgerJournalFromEntries :: [Entry] -> String
showLedgerJournalFromEntries entries =
  let groupedEntries = groupBy (equalBy entryDate) entries
      journalForDay :: [Entry] -> String
      journalForDay es =
        let dateComment = [i|# #{showEntryDateWithDay $ head es}|]
            transactions = unlines $ map showLedgerTransactionFromEntry es
        in dateComment ++ "\n" ++ transactions
      journalParts = map journalForDay groupedEntries

  in intercalate "\n" journalParts



outputLedgerFromEntries :: String -> [Entry] -> IO ()
outputLedgerFromEntries outputF entries =
  writeFile outputF $ showLedgerJournalFromEntries entries
