module Data.Expenses.Parse.Megaparsec.Entry
  ( Entry(..)
  , entryFromExpense
  , categoryFromString)
where

import Data.Maybe (fromMaybe)

import Data.Expenses.Expense(DateDirective, Expense(..),
                Day(Mon), nextDate,
                Direction(..))
import qualified Data.Expenses.Expense as E

import Data.Expenses.Types(Category(..), Entry(..))



entryFromExpense :: (Int, Int, Int) -> Expense -> Entry
entryFromExpense (y,m,d) exp =
  Entry { entryDate       = (y,m,d)
        , entryPrice      = (dollars, cents, cur)
        , entryRemark     = E.expenseRemark exp
        -- MAGIC: 2x categories.
        , entryCategories = [ Uncategorised
                            , Uncategorised
                            ]
        }
  where
    amount  = E.expenseAmount exp
    mult    = case E.expenseDirection exp of
               Spent -> (1 *)
               Received -> ((-1) *)
    dollars = mult $ E.moneyDollar amount
    cents   = E.moneyCents amount

    -- MAGIC: Implicit currency is SGD if not given.
    cur     = fromMaybe "SGD" (E.moneyCurrency amount)



categoryFromString :: String -> Category
categoryFromString "Uncategorised" = Uncategorised
categoryFromString "" = Uncategorised
categoryFromString s = Category s
