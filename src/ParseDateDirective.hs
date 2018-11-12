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

module ParseDateDirective where

import Control.Monad (void)

import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char (noneOf)
import Text.Megaparsec.Char (spaceChar, string)
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L

import Expense (Day(..), DateDirective(..))


type Parser = Parsec Void String


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
  ((string "MON")  *> pure Mon) <|>
  ((string "TUE" <* skipMany (noneOf "\n\r\0")) *> pure Tue) <|>
  ((string "WED" <* skipMany (noneOf "\n\r\0"))  *> pure Wed) <|>
  ((string "THU" <* skipMany (noneOf "\n\r\0")) *> pure Thu) <|>
  ((string "FRI")  *> pure Fri) <|>
  ((string "SAT")  *> pure Sat) <|>
  ((string "SUN")  *> pure Sun)



-- TODO: at the moment, not strict about `yyyy-mm-dd`
date :: Parser (Int, Int, Int)
date =
  do yyyy <- fromIntegral <$> integer
     void  dash
     mm <- fromIntegral <$> integer
     void  dash
     dd <- fromIntegral <$> integer
     return $ (yyyy, mm, dd)



dateDirective :: Parser DateDirective
dateDirective =
  do dt <- optional date
     dy <- day
     return $ DateDir dt dy

