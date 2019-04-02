module Data.Expenses.Parse.Megaparsec.Types where

import Data.Void (Void)

import Text.Megaparsec (Parsec, hidden, optional, skipMany, (<|>))

import Data.Expenses.Expense (DateDirective, Expense)



type Parser = Parsec Void String



-- LineDirective serves as the "AST" of an Expenses document

data LineDirective = DateCmd DateDirective
                   | ExpCmd Expense
                   deriving (Show, Eq)
