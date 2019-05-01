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
                   , entryPrice = price@(dollars, _, _)
                   , entryRemark = remark
                   } =
  [i|#{direction} #{price'} #{remark}|]
    where
      price' = showHumanReadableMoney price
      direction = if (dollars >= 0) then "Spent" else "Received"



showEntryDate :: Entry -> String
showEntryDate Entry { entryDate = (y, m, d) } =
  printf "%4d-%02d-%02d" y m d



showEntryDateWithDay :: Entry -> String
showEntryDateWithDay Entry { entryDate = (y, m, d) } =
  printf "%4d-%02d-%02d %s" y m d day
    where
      day = show $ DT.dayOfWeek $ DT.fromGregorian (fromIntegral y) m d



showMoney :: (Int, Int, String) -> String
showMoney (dollars, cents, currency) =
  let dollars' = showCommaSeparatedNumber dollars
  in [i|#{dollars'}.#{cents} #{currency}|]



showCommaSeparatedNumber :: Int -> String
showCommaSeparatedNumber x | x < 1000 = show x
showCommaSeparatedNumber x =
  let head = showCommaSeparatedNumber (x `div` 1000)
      rest = printf "%03d" (x `mod` 1000) :: String
  in [i|#{head},#{rest}|]



showHumanReadableMoney :: (Int, Int, String) -> String
showHumanReadableMoney (dollars, cents, currency) =
  let trailing3Zeros = dollars `mod` 1000 == 0 && cents == 0
      useM = dollars > 1000000 && trailing3Zeros
      useK = dollars > 1000 && (dollars `mod` 1000 == 0 || dollars `mod` 1000 >= 100) && cents == 0
      (dollars', cents', modifier) =
        if useM then
          (dollars `div` 1000000, (dollars `mod` 1000000) `div` 1000, "m")
        else
          if useK then
            (dollars `div` 1000, dollars `mod` 1000, "k")
          else
            (dollars, cents, "")
      truncateZeros x | x <= 0 = x
      truncateZeros x | x `mod` 10 == 0 = truncateZeros (x `div` 10)
      truncateZeros x | otherwise = x
      humanReadableDollars = showCommaSeparatedNumber dollars'
      humanReadableCents =
        if cents' == 0 then
          ""
        else
          if useM || useK then
            [i|.#{truncateZeros cents'}|]
          else
            [i|.#{cents'}|]
  in [i|#{humanReadableDollars}#{humanReadableCents}#{modifier} #{currency}|]



showEntryPrice :: Entry -> String
showEntryPrice = showMoney . entryPrice



showLedgerTransactionFromEntry :: Entry -> String
showLedgerTransactionFromEntry entry =
  unindent [i|
  # #{originalDirective}
  #{date} #{remark}
    Undescribed  #{price}
    Assets:Cash:#{cur}|] ++ cmt
  where
    date   = showEntryDate entry
    (_, _, cur) = entryPrice entry
    price = showEntryPrice entry
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
