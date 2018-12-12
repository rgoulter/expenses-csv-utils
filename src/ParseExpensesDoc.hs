module ParseExpensesDoc where


import Control.Monad (void, forM_)

import Data.Maybe (fromMaybe, mapMaybe)

import Text.CSV as CSV

import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.Char (eol, spaceChar)
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import Expense (DateDirective, Expense(..),
                Day(Mon), nextDate,
                Direction(..))
import Entry
import ParseDateDirective (Parser)
import qualified ParseDateDirective as PD
import qualified ParseExpenseDirective as PE
import qualified Expense as E



-- LineDirective serves as the "AST" of an Expenses document
data LineDirective = DateCmd DateDirective
                   | ExpCmd Expense
                   deriving (Show, Eq)



-- PARSER



sc :: Parser ()
sc = hidden . skipMany $ choice [void spaceChar,
                                 void eol,
                                 L.skipLineComment "#"]



parseExpensesFile :: Parser [LineDirective]
parseExpensesFile =
  some
    (sc *> (DateCmd <$> PD.dateDirective <* sc <?> "Date directive") <|>
           (ExpCmd  <$> PE.expense <* sc <?> "Expense directive"))



-- UTILITY FUNCTIONS



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

