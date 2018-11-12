module Entry where

import Data.Hashable (Hashable(..))

import Data.Maybe (fromMaybe)

import Expense (DateDirective, Expense(..),
                Day(Mon), nextDate,
                Direction(..))
import qualified Expense as E




-- For now, we'll just have categories as strings
data Category = Uncategorised
              | Category String
              deriving (Eq)



data Entry = Entry
  { entryDate       :: (Int, Int, Int)    -- (y,m,d)
  , entryPrice      :: (Int, Int, String) -- (dlr,cents,cur)
  , entryRemark     :: String
  , entryCategories :: [Category]
  } deriving (Show, Eq)



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



stringFromCategory :: Category -> String
stringFromCategory Uncategorised = "Uncategorised"
stringFromCategory (Category c) = c



categoryFromString :: String -> Category
categoryFromString "Uncategorised" = Uncategorised
categoryFromString "" = Uncategorised
categoryFromString s = Category s



instance Hashable Category where
  hashWithSalt s Uncategorised = s
  hashWithSalt s (Category c) = hashWithSalt s c



instance Show Category where
  show = stringFromCategory



