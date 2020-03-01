module Data.Expenses.Ledger.AccountSuggestions
  ( SuggestionResult(..)
  , suggestions
  ) where

import qualified Data.List as L
import qualified Data.List.NonEmpty as NE


import qualified Data.TagFrequencies as TF
import Data.Expenses.Types (SimpleTransaction(..))



data SuggestionResult
  = Ambiguous (NE.NonEmpty String)
  | Exact String
  | None
  deriving (Show, Eq)



addTransaction :: SimpleTransaction -> TF.Table -> TF.Table
addTransaction txn tbl =
  let
    phrase = transactionDescription txn
    tag = transactionDebittedAccount txn
  in
    TF.addPhrase phrase tag tbl



frequencyTableForTransactions :: [SimpleTransaction] -> TF.Table
frequencyTableForTransactions =
  L.foldr addTransaction TF.empty



suggestions :: [SimpleTransaction] -> String -> SuggestionResult
suggestions txns description =
  let
    tbl = frequencyTableForTransactions txns
  in
  case TF.tagsForExactPhrase description tbl of
    Nothing ->
      case TF.tagsByWords description tbl of
        [] -> None
        accounts -> Ambiguous $ NE.fromList accounts
    Just [account] -> Exact account
    Just accounts -> Ambiguous $ NE.fromList accounts
