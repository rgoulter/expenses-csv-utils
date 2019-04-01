module Data.Expenses.Types where

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

data Entry = Entry
  { entryDate       :: (Int, Int, Int)    -- (y,m,d)
  , entryPrice      :: (Int, Int, String) -- (dlr,cents,cur)
  , entryRemark     :: String
  } deriving (Show, Eq)
