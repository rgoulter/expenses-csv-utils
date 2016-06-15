module TestCategoriseEntries where

import Data.List.NonEmpty (NonEmpty (..))
import Test.Hspec
import qualified Data.Set as E
import Text.Heredoc (here)

import qualified Categorise        as C
import qualified CategoriseEntries as CE
import qualified Entry             as E



-- promptFromEntry :: D.Entry -> [M.Model D.Category] -> CategorisePrompt



mkEntry :: String -> [E.Category] -> E.Entry
mkEntry remark cat =
  -- use arbitrary (but constant) date, price;
  -- so the remark keys the entries
  E.Entry (2000,1,1) (1,0,"USD") remark cat



category1 = E.Category "C1"
category2 = E.Category "C2"

remark1 = "this is a remark"

entry1 = mkEntry remark1 [category1, category2]
entry2 = mkEntry remark1 [E.Uncategorised, E.Uncategorised]

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
    --   -- XXX xs1 :: [CE.Sug] == [(E.Category, C.Probability)]
    --   xs1 `shouldBe` ["C1"]
    --   xs2 `shouldBe` ["C2"]

    it "updateModelWith" $ do
      -- If we give the same categories,
      -- the models should be the same
      let model' = CE.updateModelWith model2 ["C1", "C2"]
      model' `shouldBe` model1'

