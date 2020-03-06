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

module Data.Expenses.Parse.Megaparsec.DateDirective
  (date, dateDirective, dayOfWeek) where

import Control.Monad (void)

import qualified Data.Time.Calendar.Compat as DT

import Text.Megaparsec
  (choice, hidden, noneOf, option, optional, skipMany)
import Text.Megaparsec.Char (spaceChar, string)
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Expenses.Parse.Megaparsec.Types (Parser, DateDirective(..))



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
    [ DT.Monday <$ string "MON"
    , DT.Tuesday <$ (string "TUE" <* skipMany (noneOf "\n\r\0"))
    , DT.Wednesday <$ (string "WED" <* skipMany (noneOf "\n\r\0"))
    , DT.Thursday <$ (string "THU" <* skipMany (noneOf "\n\r\0"))
    , DT.Friday <$ string "FRI"
    , DT.Saturday <$ string "SAT"
    , DT.Sunday <$ string "SUN"
    ]



date :: Parser DT.Day
date =
  do yyyy <- integer
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
