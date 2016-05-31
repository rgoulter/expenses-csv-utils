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



sc :: Parser ()
sc = hidden . skipMany $ choice [void spaceChar,
                                 void eol,
                                 L.skipLineComment "#"]



parseExpensesFile :: Parser [LineDirective]
parseExpensesFile =
  some $ (sc *> (DateCmd <$> try dateDirective <* sc <?> "Date directive") <|>
                (ExpCmd  <$> try expense <* sc <?> "Expense directive"))



rowFromExp :: (Int, Int, Int) -> Expense -> [String]
rowFromExp (y,m,d) exp =
  [date, price, cur, remark, "Uncategorised"]
  where
    date   = printf "%4d-%02d-%02d" y m d
    amount = expenseAmount exp
    mult   = case expenseDirection exp of
               Spent -> (1 *)
               Received -> ((-1) *)
    price  = printf "%d.%d" (mult $ moneyDollar amount) (moneyCents amount)
    cur    = fromMaybe "SGD" (moneyCurrency amount)
    remark = expenseRemark exp



rowsFromDirectives :: [LineDirective] -> [[String]]
rowsFromDirectives directives =
  let init = ((-1, -1, -1), Mon, [])

      -- Fold over a (Date, Day, GatheredRows)
      (_, _, rows) =
        foldl (\(date, day, rows) lineD ->
                 case lineD of
                   -- For ExpenseDirectives, simply add to list of 'rows'.
                   ExpCmd exp ->
                     (date, day, rowFromExp date exp : rows)

                   -- For DateDirectives, increment/set the date/day.
                   DateCmd dateDir ->
                     let (date', day') = nextDate (date, day) dateDir
                     in  (date', day', rows))
              init
              directives
      rows' = reverse rows
  in  rows'

