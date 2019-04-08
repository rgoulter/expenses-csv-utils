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

import qualified Data.Time.Calendar as DT

import Text.Megaparsec
  (Parsec, choice, hidden, noneOf, option, optional, skipMany, (<|>))
import Text.Megaparsec.Char (spaceChar, string)
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Expenses.Parse.Megaparsec.Types (Parser)
import Data.Expenses.Expense (DateDirective(..))


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



dayOfWeek :: Parser DT.DayOfWeek
dayOfWeek =
  choice
    [ string "MON" $> DT.Monday
    , (string "TUE" <* skipMany (noneOf "\n\r\0")) $> DT.Tuesday
    , (string "WED" <* skipMany (noneOf "\n\r\0")) $> DT.Wednesday
    , (string "THU" <* skipMany (noneOf "\n\r\0")) $> DT.Thursday
    , string "FRI" $> DT.Friday
    , string "SAT" $> DT.Saturday
    , string "SUN" $> DT.Sunday
    ]



date :: Parser DT.Day
date =
  do yyyy <- fromIntegral <$> integer
     void  dash
     mm <- fromIntegral <$> integer
     void  dash
     dd <- fromIntegral <$> integer
     return $ DT.fromGregorian yyyy mm dd



dateDirective :: Parser DateDirective
dateDirective =
  do dt <- optional date
     case dt of
       Just day -> do
         dow <- option (DT.dayOfWeek day) dayOfWeek
         return $ DateDir dt dow
       Nothing ->
         DateDir dt <$> dayOfWeek
