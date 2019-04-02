module Data.Expenses.Parse.Megaparsec.ExpensesDoc where

import Control.Monad (void, forM_)

import Data.Maybe (fromMaybe, mapMaybe)

import qualified Text.CSV as CSV

import Text.Megaparsec (choice, eof, hidden, skipMany, some, (<?>), (<|>))
import Text.Megaparsec (dbg)
import Text.Megaparsec.Char (eol, space, spaceChar, tab)
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Expenses.Expense
  (DateDirective, Day(Mon), Direction(..), Expense(..), nextDate)
import Data.Expenses.Parse.Megaparsec.Entry
import Data.Expenses.Parse.Megaparsec.DateDirective (Parser)
import qualified Data.Expenses.Parse.Megaparsec.DateDirective as PD
import qualified Data.Expenses.Parse.Megaparsec.ExpenseDirective as PE
import qualified Data.Expenses.Expense as E



-- LineDirective serves as the "AST" of an Expenses document
data LineDirective = DateCmd DateDirective
                   | ExpCmd Expense
                   deriving (Show, Eq)



-- PARSER



sc :: Parser ()
sc = hidden . skipMany $ choice [ void space
                                , void tab
                                , L.skipLineComment "#"
                                ]



scn :: Parser ()
scn = hidden . skipMany $ choice [ void spaceChar
                                 , L.skipLineComment "#"
                                 ]



parseExpensesFile :: Parser [LineDirective]
parseExpensesFile =
  scn *> lines
    where
  lines = (eof *> return []) <|>
          ((:) <$>
           ((DateCmd
             <$> PD.dateDirective <* scn
             <?> "Date directive") <|>
            (ExpCmd
             <$> PE.expense <* scn
             <?> "Expense directive")) <*>
           lines)



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
