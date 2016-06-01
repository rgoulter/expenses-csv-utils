module ParseExpensesDoc where

import Text.Printf (printf)
import Data.Maybe (fromMaybe)

import Control.Monad (void, forM_)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String -- input stream is of type ‘String’
import qualified Text.Megaparsec.Lexer as L
import qualified Text.Megaparsec.Char as C

import ParseDateDirective hiding (sc)
import ParseExpenseDirective hiding (sc)



data LineDirective = DateCmd DateDirective | ExpCmd Expense deriving (Show, Eq)


-- For now, we'll just have categories as strings
data Category = Uncategorised
              | Category String



data Entry = Entry
  { entryDate       :: (Int, Int, Int)    -- (y,m,d)
  , entryPrice      :: (Int, Int, String) -- (dlr,cents,cur)
  , entryRemark     :: String
  , entryCategories :: [Category]
  }



-- PARSER



sc :: Parser ()
sc = hidden . skipMany $ choice [void spaceChar,
                                 void eol,
                                 L.skipLineComment "#"]



parseExpensesFile :: Parser [LineDirective]
parseExpensesFile =
  some $ (sc *> (DateCmd <$> dateDirective <* sc <?> "Date directive") <|>
                (ExpCmd  <$> expense <* sc <?> "Expense directive"))



-- UTILITY FUNCTIONS



entryFromExpense :: (Int, Int, Int) -> Expense -> Entry
entryFromExpense (y,m,d) exp =
  Entry { entryDate       = (y,m,d)
        , entryPrice      = (dollars, cents, cur)
        , entryRemark     = expenseRemark exp
        -- MAGIC: 2x categories.
        , entryCategories = [ Uncategorised
                            , Uncategorised
                            ]
        }
  where
    amount  = expenseAmount exp
    mult    = case expenseDirection exp of
               Spent -> (1 *)
               Received -> ((-1) *)
    dollars = mult $ moneyDollar amount
    cents   = moneyCents amount

    -- MAGIC: Implicit currency is SGD if not given.
    cur     = fromMaybe "SGD" (moneyCurrency amount)



stringOfCategory :: Category -> String
stringOfCategory Uncategorised = "Uncategorised"
stringOfCategory (Category c) = c



rowFromEntry :: Entry -> [String]
rowFromEntry entry =
  [date, price, cur, remark] ++ map stringOfCategory (entryCategories entry)
  where
    (y, m, d) = entryDate entry
    date   = printf "%4d-%02d-%02d" y m d
    (dollars, cents, cur) = entryPrice entry
    price  = printf "%d.%d" dollars cents
    remark = entryRemark entry



entriesFromDirectives :: [LineDirective] -> [Entry]
entriesFromDirectives directives =
  let init = ((-1, -1, -1), Mon, [])

      -- Fold over a (Date, Day, GatheredRows)
      (_, _, rows) =
        foldl (\(date, day, rows) lineD ->
                 case lineD of
                   -- For ExpenseDirectives, simply add to list of 'rows'.
                   ExpCmd exp ->
                     (date, day, entryFromExpense date exp : rows)

                   -- For DateDirectives, increment/set the date/day.
                   DateCmd dateDir ->
                     let (date', day') = nextDate (date, day) dateDir
                     in  (date', day', rows))
              init
              directives
      rows' = reverse rows
  in  rows'



rowsFromDirectives :: [LineDirective] -> [[String]]
rowsFromDirectives directives =
  map rowFromEntry $ entriesFromDirectives directives

