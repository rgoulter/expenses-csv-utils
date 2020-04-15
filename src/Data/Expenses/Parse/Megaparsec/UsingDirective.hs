module Data.Expenses.Parse.Megaparsec.UsingDirective
  ( using
  )
where

import           Control.Monad                  ( void )

import           Text.Megaparsec                ( choice
                                                , count
                                                , hidden
                                                , skipMany
                                                )
import           Text.Megaparsec.Char           ( char
                                                , string
                                                , tab
                                                , upperChar
                                                )
import qualified Text.Megaparsec.Char.Lexer    as L

import           Data.Expenses.Parse.Megaparsec.Types
                                                ( Configuration(..)
                                                , Parser
                                                )



sc :: Parser ()
sc = hidden . skipMany . void $ choice [char ' ', tab]


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc



currency :: Parser String
currency = lexeme $ count 3 upperChar



using :: Parser Configuration
using = do
  _   <- lexeme $ string "Using"
  cur <- currency
  _   <- lexeme $ string "as"
  _   <- lexeme $ string "the"
  _   <- lexeme $ string "default"
  _   <- lexeme $ string "currency"
  return $ Configuration { configDefaultCurrency = cur }
