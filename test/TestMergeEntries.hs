module TestMergeEntries where

import Data.List.NonEmpty (NonEmpty (..))
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Data.Set as E
import Text.Heredoc (here)

import ParseDateDirective as D
import ParseExpenseDirective as E
import ParseExpensesDoc as ED



-- want some test entries: A, B, X, Y
mkEntry :: String -> [String] -> ED.Entry
mkEntry remark cat =
  -- use arbitrary (but constant) date, price;
  -- so the remark keys the entries
  ED.Entry (2000,1,1) (1,0,"USD") remark (map ED.Category cat)

-- Entries (unspecified, specified)
ex  = mkEntry "X" []
exs = mkEntry "X" ["Cat"]
ey  = mkEntry "Y" []
eys = mkEntry "Y" ["Cat"]
-- Entries ('new')
ea  = mkEntry "A" []
eb  = mkEntry "B" []



entryListMerge :: Spec
entryListMerge =
  describe "Entry Merging" $ do
    -- Trivial case: non-empty and empty
    it "should merge trivial lists with empty" $ do
      ED.mergeEntryLists [] [] `shouldBe` []
      ED.mergeEntryLists [ex] [] `shouldBe` [ex]
      ED.mergeEntryLists [] [ex] `shouldBe` [ex]

    -- Simple case: Two identical (unspecified)
    it "should merge two identical (unspecified)" $ do
      ED.mergeEntryLists [ex] [ex] `shouldBe` [ex]
      ED.mergeEntryLists [ex, ey] [ex, ey] `shouldBe` [ex, ey]

    -- Simple case: Two identical (specified)
    it "should merge two identical (specified)" $ do
      ED.mergeEntryLists [exs] [exs] `shouldBe` [exs]
      ED.mergeEntryLists [exs, eys] [exs, eys] `shouldBe` [exs, eys]

    -- Simple case: Fresh merge Specified
    it "should merge specified > unspecified" $ do
      ED.mergeEntryLists [ex] [exs] `shouldBe` [exs]
      ED.mergeEntryLists [ex, ey] [exs, eys] `shouldBe` [exs, eys]
      ED.mergeEntryLists [exs, ey] [ex, eys] `shouldBe` [exs, eys]
      ED.mergeEntryLists [ex, eys] [exs, ey] `shouldBe` [exs, eys]

    -- case: new item
    it "should merge with new item" $ do
      ED.mergeEntryLists [ex, ea] [ex] `shouldBe` [ex, ea]
      ED.mergeEntryLists [ex] [ex, ea] `shouldBe` [ex, ea]
      ED.mergeEntryLists [ea, ex] [ex] `shouldBe` [ea, ex]

    -- case: new item in between
    it "should merge with new item (in between)" $ do
      ED.mergeEntryLists [ex, ea, ey] [ex, ey] `shouldBe` [ex, ea, ey]
      ED.mergeEntryLists [ex, ey] [ex, ea, ey] `shouldBe` [ex, ea, ey]

    -- case: swapped.
    it "should merge swaps/permutations" $ do
      -- This is impl. dependent, but we impl. this way, so.
      ED.mergeEntryLists [ex, ey] [ey, ex] `shouldBe` [ex, ey]

    -- case: swapped and updated
    it "should merge updated swaps/permutations" $ do
      ED.mergeEntryLists [ex, ey] [ey, exs] `shouldBe` [exs, ey]
      ED.mergeEntryLists [ex, eys] [ey, exs] `shouldBe` [exs, eys]

    -- case: swapped and updated, new item
    it "should merge updated swaps/permutations, with new item" $ do
      ED.mergeEntryLists [ex, eys, ea] [ey, exs] `shouldBe` [exs, eys, ea]
      -- This is impl. dependent, but we impl. this way, so.
      ED.mergeEntryLists [ex, ea, eys] [ey, exs] `shouldBe` [exs, ea, eys]

    -- case: duplicate
    it "should merge lists with duplicate" $ do
      ED.mergeEntryLists [ex, ex] [ex] `shouldBe` [ex, ex]
      ED.mergeEntryLists [ex, ex] [ex, ex] `shouldBe` [ex, ex]

    -- case: preserves date/order..
    -- it "should merge ..." $ do
    --   ED.mergeEntryLists [] [] `shouldBe` []

