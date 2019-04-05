module Data.Expenses.Parse.Megaparsec.ExpensesDoc where

import Data.Void (Void)

import Control.Monad (void, forM_)

import Data.Either (Either(..), partitionEithers)
import Data.Maybe (fromMaybe, mapMaybe)

import qualified Text.CSV as CSV

import Text.Megaparsec
  ( ParseError
  , anySingle
  , between
  , choice
  , eof
  , hidden
  , manyTill
  , sepEndBy
  , skipMany
  , some
  , withRecovery
  , (<?>)
  , (<|>)
  )
import Text.Megaparsec.Char (eol, space, spaceChar, tab)
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Expenses.Expense
  (DateDirective, Day(Mon), Direction(..), Expense(..), nextDate)
import Data.Expenses.Parse.Megaparsec.Entry
import Data.Expenses.Parse.Megaparsec.Types
  (LineDirective(..), Parser, RawLineDirective)
import qualified Data.Expenses.Parse.Megaparsec.DateDirective as PD
import qualified Data.Expenses.Parse.Megaparsec.ExpenseDirective as PE
import qualified Data.Expenses.Expense as E




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



eitherOfLists :: [Either a b] -> Either [a] [b]
eitherOfLists xs =
  f pxs
    where
  f ([], xs) = Right xs
  f (xs, _) = Left xs
  pxs = partitionEithers xs



lineDirective :: Parser LineDirective
lineDirective =
  (DateCmd
   <$> PD.dateDirective <* scn
   <?> "Date directive") <|>
  (ExpCmd
   <$> PE.expense <* scn
   <?> "Expense directive")



recover :: ParseError String Void -> Parser RawLineDirective
recover err = Left err <$ manyTill anySingle eol



parseExpensesFile :: Parser [RawLineDirective]
parseExpensesFile =
  between scn eof (sepEndBy rawLine scn)
   where
     rawLine = withRecovery recover (Right <$> lineDirective)



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
