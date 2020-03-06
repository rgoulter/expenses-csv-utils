module Data.Expenses.Parse.Megaparsec.Types
  (Parser, LineDirective(..), RawLineDirective) where

import Data.Void (Void)

import Text.Megaparsec (ParseError, Parsec)

import Data.Expenses.Types (DateDirective, Expense)



type Parser = Parsec Void String



-- LineDirective serves as the "AST" of an Expenses document

data LineDirective = DateCmd DateDirective
                   | ExpCmd Expense
                   deriving (Show, Eq)



type RawLineDirective = Either (ParseError String Void) LineDirective
