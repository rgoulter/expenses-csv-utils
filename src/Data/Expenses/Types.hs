module Data.Expenses.Types where

import qualified Data.Decimal as D

import Data.Time.Calendar (Day, DayOfWeek)

data DateDirective = DateDir
    { dateDirDate :: Maybe Day
    , dateDirDay  :: DayOfWeek
    } deriving (Show, Eq)

-- ~ 1234.12 CUR
data Money = Amount
  { moneyAmount   :: D.Decimal
  , moneyCurrency :: Maybe String
  , moneyIsApprox :: Bool
  } deriving (Show, Eq)

data Direction = Spent | Received deriving (Show, Eq)

data Expense = Expense
  { expenseDirection :: Direction
  , expenseAmount    :: Money
  , expenseRemark    :: String
  , expenseComment   :: Maybe String
  } deriving (Show, Eq)

data Entry = Entry
  { entryDate       :: (Int, Int, Int)    -- (y,m,d)
  , entryPrice      :: (D.Decimal, String) -- (dlr,cents,cur)
  , entryRemark     :: String
  , entryComment    :: Maybe String
  } deriving (Show, Eq)

data QueryAttribute
  = Earliest
  | Latest
  deriving (Show, Eq)

data SimpleTransaction = SimpleTransaction
  { transactionDescription :: String
  , transactionAmount :: Money
  , transactionCredittedAccount :: String
  , transactionDebittedAccount :: String
  } deriving (Show, Eq)
