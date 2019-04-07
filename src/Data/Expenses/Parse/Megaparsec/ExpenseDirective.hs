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

import Control.Monad.Combinators.Expr

import Control.Monad (void)

import Data.Functor (($>))

import qualified Data.List.NonEmpty as NE

import qualified Data.Set as Set


import Data.Maybe (isJust)

import Text.Megaparsec
  ( ErrorItem(Tokens)
  , count
  , failure
  , hidden
  , lookAhead
  , many
  , noneOf
  , optional
  , skipMany
  , some
  , try
  , unexpected
  , (<|>)
  )
import Text.Megaparsec.Char (letterChar, spaceChar, string, upperChar)
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Expenses.Expense (Money(..), Direction(..), Expense(..))

import Data.Expenses.Parse.Megaparsec.Types (Parser)


sc :: Parser ()
sc = hidden . skipMany $ void spaceChar


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc



integer :: Parser Integer
integer = lexeme L.decimal



direction :: Parser Direction
direction = do
  word <- lookAhead $ many letterChar :: Parser String
  case word of
    "Spent" -> (string "Spent" :: Parser String) *> sc *> return Spent
    "Received" -> (string "Received" :: Parser String) *> sc *> return Received
    _ -> failure (Just $ Tokens $ NE.fromList word)
                 (Set.fromList [ Tokens (NE.fromList "Spent")
                               , Tokens (NE.fromList "Received")
                               ])



currency :: Parser String
currency =
  lexeme $ count 3 upperChar



modifyDollarsAndCents :: Int -> (Int, Int) -> (Int, Int)
modifyDollarsAndCents mul (dollars, cents) =
  let len :: Int -> Int
      len n =
        if n == 0 then
          0
        else
          1 + floor(logBase 10 (fromIntegral n :: Double))
      modifiedDollars = mul * dollars
      modifiedCents = mul * cents `div` (10 ^ len cents)
  in
    (modifiedDollars + modifiedCents, 0)



dollarsAndCents :: Parser (Int, Int)
dollarsAndCents =
  do dollars <- read <$> some C.digitChar
     cents <- fromIntegral <$> try (C.char '.' *> integer) <|> (0 <$ sc)
     return (dollars, cents)



amount :: Parser Money
amount =
  do approx <- optional $ symbol "~"
     (dollars, cents) <- dollarsAndCents
     let kModifier = 1000 <$ C.char 'k'
         mModifier = 1000000 <$ C.char 'm'
     modifier <- optional $ (kModifier <|> mModifier) <* sc
     cur <- optional currency
     void sc
     -- return $ Amount dollars cents cur (isJust approx)
     return $ case modifier of
       Nothing -> Amount dollars cents cur (isJust approx)
       Just mul ->
         let (dollars', cents') = modifyDollarsAndCents mul (dollars, cents)
         in
           Amount dollars' cents' cur (isJust approx)

-- n.b. this doesn't allow for comments at the end-of-line
expense :: Parser Expense
expense =
  do dir <- direction
     am  <- amount
     remark <- many (noneOf "\n\r\0")
     return $ Expense dir am remark
