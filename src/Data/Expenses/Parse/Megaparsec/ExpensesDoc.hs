module Data.Expenses.Parse.Megaparsec.ExpensesDoc
  ( eitherOfLists
  , entriesFromDirectives
  , parseExpensesFile
  ) where

import Data.Void (Void)

import Control.Monad (void)

import Data.Either (Either(..), partitionEithers)

import qualified Data.Time.Calendar.Compat as DT

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
  , someTill
  , try
  , withRecovery
  , (<?>)
  , (<|>)
  )
import Text.Megaparsec.Char (eol, spaceChar)
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Expenses.Expense
  (nextDate)
import Data.Expenses.Parse.Megaparsec.Entry
import Data.Expenses.Parse.Megaparsec.Types
  (LineDirective(..), Parser, RawLineDirective)
import qualified Data.Expenses.Parse.Megaparsec.DateDirective as PD
import qualified Data.Expenses.Parse.Megaparsec.ExpenseDirective as PE




-- PARSER



scn :: Parser ()
scn = hidden . skipMany $ choice [ void spaceChar
                                 , L.skipLineComment "#"
                                 ]



eitherOfLists :: [Either a b] -> Either [a] [b]
eitherOfLists xxs =
  f pxs
    where
  f ([], xs) = Right xs
  f (xs, _) = Left xs
  pxs = partitionEithers xxs



lineDirective :: Parser LineDirective
lineDirective =
  (DateCmd
   <$> PD.dateDirective <* scn
   <?> "Date directive") <|>
  (ExpCmd
   <$> PE.expense <* scn
   <?> "Expense directive")



recover :: ParseError String Void -> Parser RawLineDirective
recover err =
  Left err <$ (try restOfLine <|> lastLine)
    where
      restOfLine = void $ manyTill anySingle eol
      lastLine = void $ someTill (void anySingle) eof



parseExpensesFile :: Parser [RawLineDirective]
parseExpensesFile =
  between scn eof (sepEndBy rawLine scn)
   where
     rawLine = withRecovery recover (Right <$> lineDirective)



-- UTILITY FUNCTIONS



entriesFromDirectives :: [LineDirective] -> [Entry]
entriesFromDirectives directives =
  let initial = ((-1, -1, -1), DT.Monday, [])

      -- Fold over a (Date, Day, GatheredRows)
      (_, _, directiveRows) =
        foldl (\(date, day, rows) lineD ->
                 case lineD of
                   -- For ExpenseDirectives, simply add to list of 'rows'.
                   ExpCmd expense ->
                     (date, day, entryFromExpense date expense : rows)

                   -- For DateDirectives, increment/set the date/day.
                   DateCmd dateDir ->
                     let (date', day') = nextDate (date, day) dateDir
                     in  (date', day', rows))
              initial
              directives
      rows' = reverse directiveRows
  in  rows'
