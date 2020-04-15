module TestUsingParser (parseUsingDirectiveSpec) where

import Test.Hspec (Spec, describe, it)

import Test.Hspec.Megaparsec
  ( initialState
  , shouldParse
  , succeedsLeaving
  )

import Text.Megaparsec (parse, runParser')


import qualified Data.Expenses.Parse.Megaparsec.UsingDirective as PU
import qualified Data.Expenses.Parse.Megaparsec.Types as E



parseUsingDirectiveSpec :: Spec
parseUsingDirectiveSpec =
  describe "Data.Expenses.Parse.Megaparsec.UsingDirective" $
    describe "using" $ do
      it "should parse 'Using xxx as the default currency'" $ do
        parse PU.using "" "Using SGD as the default currency"
          `shouldParse` E.Configuration "SGD"
        parse PU.using "" "Using VND as the default currency"
          `shouldParse` E.Configuration "VND"
      it "doesn't parse beyond EOL" $
        runParser' PU.using (initialState "Using SGD as the default currency\nnext")
          `succeedsLeaving` "\nnext"
