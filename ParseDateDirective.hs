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
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String -- input stream is of type ‘String’
import qualified Text.Megaparsec.Lexer as L



data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show)

data DateDirective = DateDir (Maybe (Int, Int, Int)) Day deriving (Show)



sc :: Parser ()
sc = hidden . skipMany $ void spaceChar


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc



integer :: Parser Integer
integer = lexeme L.integer

dash :: Parser String
dash = symbol "-"



-- TODO: No need to be case-sensitive here.
day :: Parser Day
day =
  ((      string "MON")  *> pure Mon) <|>
  ((try $ string "TUE" <* skipMany anyChar) *> pure Tue) <|>
  ((      string "WED" <* skipMany anyChar)  *> pure Wed) <|>
  ((try $ string "THU" <* skipMany anyChar) *> pure Thu) <|>
  ((      string "FRI")  *> pure Fri) <|>
  ((try $ string "SAT")  *> pure Sat) <|>
  ((try $ string "SUN")  *> pure Sun)

-- TODO: at the moment, not strict about `yyyy-mm-dd`
date :: Parser (Int, Int, Int)
date =
  do yyyy <- fromIntegral <$> integer
     void  dash
     mm <- fromIntegral integer
     void  dash
     dd <- fromIntegral integer
     return $ (yyyy, mm, dd)

dateDirective :: Parser DateDirective
dateDirective =
  do dt <- optional date
     dy <- day
     return $ DateDir dt dy
