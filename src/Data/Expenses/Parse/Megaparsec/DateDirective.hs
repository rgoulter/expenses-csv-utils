-- Adapted from
-- https://mrkkrp.github.io/megaparsec/tutorials/parsing-simple-imperative-language.html
--
-- Parse strings like:
--   "2016-01-01 MON"
--   "TUE"
-- (Maybe yyyy-mm-mm) DAY
--
-- MON, TUE, WED, THURS, FRI, SAT, SUN
--  (also TUES, WEDS)

module Data.Expenses.Parse.Megaparsec.DateDirective where

import Control.Monad (void)

import Data.Functor (($>))

import Data.Void (Void)

import Text.Megaparsec (Parsec, hidden, optional, skipMany, (<|>))
import Text.Megaparsec.Char (noneOf, spaceChar, string)
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Expenses.Parse.Megaparsec.Types (Parser)
import Data.Expenses.Expense (Day(..), DateDirective(..))


sc :: Parser ()
sc = hidden . skipMany $ void spaceChar


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc



integer :: Parser Integer
integer = lexeme L.decimal

dash :: Parser String
dash = symbol "-"



-- TODO: No need to be case-sensitive here.
day :: Parser Day
day =
  (string "MON" $> Mon) <|>
  ((string "TUE" <* skipMany (noneOf "\n\r\0")) $> Tue) <|>
  ((string "WED" <* skipMany (noneOf "\n\r\0")) $> Wed) <|>
  ((string "THU" <* skipMany (noneOf "\n\r\0")) $> Thu) <|>
  (string "FRI" $> Fri) <|>
  (string "SAT" $> Sat) <|>
  (string "SUN" $> Sun)



-- TODO: at the moment, not strict about `yyyy-mm-dd`
date :: Parser (Int, Int, Int)
date =
  do yyyy <- fromIntegral <$> integer
     void  dash
     mm <- fromIntegral <$> integer
     void  dash
     dd <- fromIntegral <$> integer
     return (yyyy, mm, dd)



dateDirective :: Parser DateDirective
dateDirective =
  do dt <- optional date
     DateDir dt <$> day
