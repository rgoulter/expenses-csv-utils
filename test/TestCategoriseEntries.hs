module TestCategoriseEntries where

import Data.List.NonEmpty (NonEmpty (..))
import Test.Hspec
import qualified Data.Set as E
import Text.Heredoc (here)

import qualified ParseExpensesDoc  as ED
import qualified Categorise        as C
import qualified CategoriseEntries as CE



-- promptFromEntry :: D.Entry -> [M.Model D.Category] -> CategorisePrompt



mkEntry :: String -> [ED.Category] -> ED.Entry
mkEntry remark cat =
  -- use arbitrary (but constant) date, price;
  -- so the remark keys the entries
  ED.Entry (2000,1,1) (1,0,"USD") remark cat



category1 = ED.Category "C1"
category2 = ED.Category "C2"

remark1 = "this is a remark"

entry1 = mkEntry remark1 [category1, category2]
entry2 = mkEntry remark1 [ED.Uncategorised, ED.Uncategorised]

model1 = ( [entry1, entry2]
         , []
         , [C.emptyModel, C.emptyModel]
         )
model1' = CE.nextModel model1

-- All empty
model2 = ( [entry2, entry2]
         , []
         , [C.emptyModel, C.emptyModel]
         )



categoriseExpensesSpec :: Spec
categoriseExpensesSpec =
  describe "promptFromEntry" $ do
    it "nextModel trivial case" $ do
      let (todo,done,_) = CE.nextModel model1
      todo `shouldBe` [entry2]
      done `shouldBe` [entry1]

    -- it "promptFromEntry trivial case" $ do
    --   -- So, categorise some entry, giving it a model..
    --   -- then should predict.
    --   let model = CE.nextModel model1
    --       (s, (_,xs1):(_,xs2):_) = CE.promptFromModel model
    --   -- XXX xs1 :: [CE.Sug] == [(ED.Category, C.Probability)]
    --   xs1 `shouldBe` ["C1"]
    --   xs2 `shouldBe` ["C2"]

    it "updateModelWith" $ do
      -- If we give the same categories,
      -- the models should be the same
      let model' = CE.updateModelWith model2 ["C1", "C2"]
      model' `shouldBe` model1'

