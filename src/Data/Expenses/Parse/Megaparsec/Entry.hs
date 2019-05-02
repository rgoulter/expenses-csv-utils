module Data.Expenses.Parse.Megaparsec.Entry
  ( Entry(..)
  , entryFromExpense
  )
where

import Data.Maybe (fromMaybe)

import Data.Expenses.Expense (Expense(..), Direction(..))
import qualified Data.Expenses.Expense as E

import Data.Expenses.Types(Entry(..))



entryFromExpense :: (Int, Int, Int) -> Expense -> Entry
entryFromExpense (y, m, d) expense =
  Entry { entryDate       = (y, m, d)
        , entryPrice      = (value, cur)
        , entryRemark     = E.expenseRemark expense
        , entryComment    = E.expenseComment expense
        }
  where
    amount  = E.expenseAmount expense
    mult    = case E.expenseDirection expense of
               Spent -> (1 *)
               Received -> ((-1) *)
    value = mult $ E.moneyAmount amount

    -- MAGIC: Implicit currency is SGD if not given.
    cur     = fromMaybe "SGD" (E.moneyCurrency amount)
