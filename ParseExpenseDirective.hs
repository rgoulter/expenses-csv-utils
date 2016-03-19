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

module ParseExpenseDirective where

import Control.Monad (void)
import Data.Maybe (isJust)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String -- input stream is of type ‘String’
import qualified Text.Megaparsec.Lexer as L
import qualified Text.Megaparsec.Char as C



-- ~ 1234.12 CUR
data Money = Amount Int Int (Maybe String) Bool deriving (Show)

data Direction = Spent | Received deriving (Show)

data Expense = Expense Direction Money String deriving (Show)



sc :: Parser ()
sc = hidden . skipMany $ void spaceChar


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc



integer :: Parser Integer
integer = lexeme L.integer



-- TODO: No benefit to case-sensitivity here?
direction :: Parser Direction
direction =
  ((string "Spent" *> pure Spent) <|>
   (string "Received" *> pure Received)) <* sc



amount :: Parser Money
amount =
  do approx <- optional $ symbol "~"
     dollars <- read <$> some C.digitChar
     -- XXX cents shouldn't be more than two digits
     cents <- fromIntegral <$> try (C.char '.' *> integer) <|> (pure 0 <* sc)
     cur <- optional (string "USD") -- XXX various currencies
     void sc
     return $ Amount dollars cents cur (isJust approx)

-- n.b. this doesn't allow for comments at the end-of-line
expense :: Parser Expense
expense =
  do dir <- direction
     am  <- amount
     remark <- many (noneOf "\n\r\0")
     return $ Expense dir am remark
