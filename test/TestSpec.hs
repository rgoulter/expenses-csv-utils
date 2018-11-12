module Main (main) where

import Data.List.NonEmpty (NonEmpty (..))

import qualified Data.Set as E

import Text.Heredoc (here)

import Test.Hspec

import Test.Hspec.Megaparsec

import Text.Megaparsec


import ParseDateDirective (Parser)
import TestDateParser
import TestExpenseParser
import TestExpenseDocParser

-- Adapted from
-- https://raw.githubusercontent.com/mrkkrp/hspec-megaparsec/0.2.0/tests/Main.hs



-- hspec :: Spec -> IO ()

main :: IO ()
main = hspec $ do
  parseDateDirectiveSpec
  parseExpenseDirectiveSpec
  parseExpensesFileSpec

