module Data.Expenses.Parse.Megaparsec.Types
  ( AST(..)
  , DateDirective(..)
  , Direction(..)
  , Expense(..)
  , Parser
  , LineDirective(..)
  , RawLineDirective
  )
where

import           Data.Time.Calendar.Compat      ( Day
                                                , DayOfWeek
                                                )

import           Data.Void                      ( Void )

import           Text.Megaparsec                ( ParseError
                                                , Parsec
                                                )

import           Data.Expenses.Types            ( Money )



type Parser = Parsec Void String



data DateDirective = DateDir
    { dateDirDate :: Maybe Day
    , dateDirDay  :: DayOfWeek
    } deriving (Show, Eq)



data Direction = Spent | Received deriving (Show, Eq)



data Expense = Expense
  { expenseDirection :: Direction
  , expenseAmount    :: Money
  , expenseRemark    :: String
  , expenseComment   :: Maybe String
  } deriving (Show, Eq)



data LineDirective = DateCmd DateDirective
                   | ExpCmd Expense
                   deriving (Show, Eq)



-- XXX: inline this into AST
type RawLineDirective = Either (ParseError String Void) LineDirective



newtype AST = AST [RawLineDirective]
  deriving (Show, Eq)
