module Data.Expenses.Types where

import Data.Hashable (Hashable(..))

data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show, Eq)

data DateDirective = DateDir {
                       dateDirDate :: Maybe (Int, Int, Int),
                       dateDirDay  :: Day
                     } deriving (Show, Eq)

-- ~ 1234.12 CUR
data Money = Amount {
               moneyDollar   :: Int,
               moneyCents    :: Int,
               moneyCurrency :: Maybe String,
               moneyIsApprox :: Bool
             } deriving (Show, Eq)

data Direction = Spent | Received deriving (Show, Eq)

data Expense = Expense {
                 expenseDirection :: Direction,
                 expenseAmount    :: Money,
                 expenseRemark    :: String
               } deriving (Show, Eq)

-- For now, we'll just have categories as strings
data Category = Uncategorised
              | Category String
              deriving (Eq)

instance Hashable Category where
  hashWithSalt s Uncategorised = s
  hashWithSalt s (Category c) = hashWithSalt s c

stringFromCategory :: Category -> String
stringFromCategory Uncategorised = "Uncategorised"
stringFromCategory (Category c) = c

instance Show Category where
  show = stringFromCategory

data Entry = Entry
  { entryDate       :: (Int, Int, Int)    -- (y,m,d)
  , entryPrice      :: (Int, Int, String) -- (dlr,cents,cur)
  , entryRemark     :: String
  , entryCategories :: [Category]
  } deriving (Show, Eq)
