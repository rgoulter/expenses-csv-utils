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

import qualified Data.Decimal as D

import Data.Functor (($>))

import Data.List (intercalate)

import Data.Maybe (fromMaybe)

import qualified Data.List.NonEmpty as NE

import qualified Data.Set as Set


import Data.Maybe (isJust)

import Text.Megaparsec
  ( ErrorItem(Tokens)
  , anySingle
  , choice
  , count
  , eof
  , failure
  , hidden
  , lookAhead
  , many
  , noneOf
  , optional
  , skipMany
  , some
  , someTill
  , try
  , unexpected
  , (<|>)
  )
import Text.Megaparsec.Char
  ( char
  , letterChar
  , space
  , string
  , tab
  , upperChar
  )
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Expenses.Expense (Money(..), Direction(..), Expense(..))

import Data.Expenses.Parse.Megaparsec.Types (Parser)



sc :: Parser ()
sc = hidden . skipMany . void $ choice [char ' ', tab]


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



dollarsAndCents :: Parser D.Decimal
dollarsAndCents =
  do dollarsString <- some (skipMany (C.char ',') *> C.digitChar)
     centsString <- try (C.char '.' *> (Just <$> some C.digitChar <* sc)) <|>
                        Nothing <$ sc
     let centsLength = fromIntegral $ maybe 0 length centsString
         number = read $ dollarsString ++ fromMaybe "" centsString
     return $ D.Decimal centsLength number



shiftDecimalPlaces :: Int -> D.Decimal -> D.Decimal
shiftDecimalPlaces mul value =
  let places = D.decimalPlaces value
      mantissa = D.decimalMantissa value
      mul' = (fromIntegral mul) - places
      value' = D.Decimal
                { D.decimalPlaces = 0
                , D.decimalMantissa =  (10 ^ mul') * mantissa
                }
  in value'



amount :: Parser Money
amount =
  do approx <- optional $ symbol "~"
     -- (dollars, cents) <- dollarsAndCents
     value <- dollarsAndCents
     let kModifier = 3 <$ C.char 'k'
         mModifier = 6 <$ C.char 'm'
     modifier <- optional $ (kModifier <|> mModifier) <* sc
     cur <- optional currency
     void sc
     -- return $ Amount dollars cents cur (isJust approx)
     return $ case modifier of
       Nothing -> Amount value cur (isJust approx)
       Just mul ->
         let value' = shiftDecimalPlaces mul value
         in
           Amount value' cur (isJust approx)



commentOnNextLine :: Parser String
commentOnNextLine = do
  sc
  C.newline
  c <- C.char '#'
  s <- someTill anySingle (lookAhead (void C.eol <|> eof))
  return (c:s)



commentsOnFollowingLines :: Parser String
commentsOnFollowingLines =
  intercalate "\n" <$> some (try commentOnNextLine)



-- n.b. this doesn't allow for comments at the end-of-line
expense :: Parser Expense
expense =
  do dir <- direction
     am  <- amount
     remark <- many (noneOf "\n\r\0")
     comment <- optional $ commentsOnFollowingLines
     return $ Expense dir am remark comment
