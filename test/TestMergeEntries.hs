module TestMergeEntries where

import Data.List.NonEmpty (NonEmpty (..))
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Data.Set as E
import Text.Heredoc (here)

import qualified Entry as E
import qualified Expenses.Merge.Constructive as ME



-- want some test entries: A, B, X, Y
mkEntry :: String -> [String] -> E.Entry
mkEntry remark cat =
  -- use arbitrary (but constant) date, price;
  -- so the remark keys the entries
  E.Entry (2000,1,1) (1,0,"USD") remark (map E.Category cat)

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
      ME.mergeEntryLists [] [] `shouldBe` []
      ME.mergeEntryLists [ex] [] `shouldBe` [ex]
      ME.mergeEntryLists [] [ex] `shouldBe` [ex]

    -- Simple case: Two identical (unspecified)
    it "should merge two identical (unspecified)" $ do
      ME.mergeEntryLists [ex] [ex] `shouldBe` [ex]
      ME.mergeEntryLists [ex, ey] [ex, ey] `shouldBe` [ex, ey]

    -- Simple case: Two identical (specified)
    it "should merge two identical (specified)" $ do
      ME.mergeEntryLists [exs] [exs] `shouldBe` [exs]
      ME.mergeEntryLists [exs, eys] [exs, eys] `shouldBe` [exs, eys]

    -- Simple case: Fresh merge Specified
    it "should merge specified > unspecified" $ do
      ME.mergeEntryLists [ex] [exs] `shouldBe` [exs]
      ME.mergeEntryLists [ex, ey] [exs, eys] `shouldBe` [exs, eys]
      ME.mergeEntryLists [exs, ey] [ex, eys] `shouldBe` [exs, eys]
      ME.mergeEntryLists [ex, eys] [exs, ey] `shouldBe` [exs, eys]

    -- case: new item
    it "should merge with new item" $ do
      ME.mergeEntryLists [ex, ea] [ex] `shouldBe` [ex, ea]
      ME.mergeEntryLists [ex] [ex, ea] `shouldBe` [ex, ea]
      ME.mergeEntryLists [ea, ex] [ex] `shouldBe` [ea, ex]

    -- case: new item in between
    it "should merge with new item (in between)" $ do
      ME.mergeEntryLists [ex, ea, ey] [ex, ey] `shouldBe` [ex, ea, ey]
      ME.mergeEntryLists [ex, ey] [ex, ea, ey] `shouldBe` [ex, ea, ey]

    -- case: swapped.
    it "should merge swaps/permutations" $ do
      -- This is impl. dependent, but we impl. this way, so.
      ME.mergeEntryLists [ex, ey] [ey, ex] `shouldBe` [ex, ey]

    -- case: swapped and updated
    it "should merge updated swaps/permutations" $ do
      ME.mergeEntryLists [ex, ey] [ey, exs] `shouldBe` [exs, ey]
      ME.mergeEntryLists [ex, eys] [ey, exs] `shouldBe` [exs, eys]

    -- case: swapped and updated, new item
    it "should merge updated swaps/permutations, with new item" $ do
      ME.mergeEntryLists [ex, eys, ea] [ey, exs] `shouldBe` [exs, eys, ea]
      -- This is impl. dependent, but we impl. this way, so.
      ME.mergeEntryLists [ex, ea, eys] [ey, exs] `shouldBe` [exs, ea, eys]

    -- case: duplicate
    it "should merge lists with duplicate" $ do
      ME.mergeEntryLists [ex, ex] [ex] `shouldBe` [ex, ex]
      ME.mergeEntryLists [ex, ex] [ex, ex] `shouldBe` [ex, ex]

    -- case: preserves date/order..
    -- it "should merge ..." $ do
    --   ME.mergeEntryLists [] [] `shouldBe` []

