-- Adapted from
-- https://mrkkrp.github.io/megaparsec/tutorials/parsing-simple-imperative-language.html
--
-- Parse strings like:
--   "Spent 10.3 on whatever"
-- into
-- Spent|Recv AMNT REMARK
--
-- No real value in distinguishing various details in the remark
-- at this point. (e.g. "location").

module Data.Expenses.Parse.Megaparsec.ExpenseDirective where

import Control.Monad (void)

import Data.Functor (($>))

import Data.Maybe (isJust)

import Text.Megaparsec (count, hidden, many, optional, skipMany, some, try, (<|>))
import Text.Megaparsec.Char (noneOf, spaceChar, string, upperChar)
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Expenses.Expense (Money(..),
                Direction(..),
                Expense(..))
import Data.Expenses.Parse.Megaparsec.DateDirective (Parser)



sc :: Parser ()
sc = hidden . skipMany $ void spaceChar


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc



integer :: Parser Integer
integer = lexeme L.decimal



-- TODO: No benefit to case-sensitivity here?
direction :: Parser Direction
direction =
  ((string "Spent" $> Spent) <|>
   (string "Received" $> Received)) <* sc



currency :: Parser String
currency =
  lexeme $ count 3 upperChar



amount :: Parser Money
amount =
  do approx <- optional $ symbol "~"
     dollars <- read <$> some C.digitChar
     cents <- fromIntegral <$> try (C.char '.' *> integer) <|> (0 <$ sc)
     cur <- optional currency
     void sc
     return $ Amount dollars cents cur (isJust approx)

-- n.b. this doesn't allow for comments at the end-of-line
expense :: Parser Expense
expense =
  do dir <- direction
     am  <- amount
     remark <- many (noneOf "\n\r\0")
     return $ Expense dir am remark
