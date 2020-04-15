module Data.Expenses.Parse.Megaparsec.UsingDirective
  ( using
  )
where

import           Control.Monad                  ( void )

import qualified Data.List.NonEmpty            as NE

import qualified Data.Set                      as Set

import           Text.Megaparsec                ( ErrorItem(Tokens)
                                                , choice
                                                , count
                                                , failure
                                                , hidden
                                                , lookAhead
                                                , many
                                                , skipMany
                                                )
import           Text.Megaparsec.Char           ( char
                                                , letterChar
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


keyword :: String -> Parser ()
keyword keyw = do
  word <- lookAhead $ many letterChar :: Parser String
  case word of
    x | x == keyw -> string keyw *> sc
    _             -> failure (Just $ Tokens $ NE.fromList word)
                             (Set.fromList [Tokens (NE.fromList keyw)])



using :: Parser Configuration
using = do
  _   <- keyword "Using"
  cur <- currency
  _   <- lexeme $ string "as"
  _   <- lexeme $ string "the"
  _   <- lexeme $ string "default"
  _   <- lexeme $ string "currency"
  return $ Configuration { configDefaultCurrency = cur }
