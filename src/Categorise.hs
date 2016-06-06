module Categorise where

import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Numeric.Natural
import Data.List.Split (wordsBy)
import Data.List (sortOn)



-- DATA STRUCTURE



type Term = String
type Count = Natural
type Probability = Rational

data Model category = Model
  { wordCategoryCounts :: HM.HashMap Term (HM.HashMap category Count)
  -- Could compute the next ones from the above, but,
  , wordCounts :: HM.HashMap Term Count
  , totalWordCount :: Count
  } deriving (Eq, Show)



emptyModel :: Model category
emptyModel =
  Model { wordCategoryCounts = HM.empty
        , wordCounts = HM.empty
        , totalWordCount = 0
        }



termsOfString :: String -> [Term]
termsOfString = wordsBy (' ' ==)



addEntry :: (Eq category, Hashable category) => Model category -> (String, category) -> Model category
addEntry model (remark, cat) =
  let incr :: (Eq k, Hashable k) => k -> HM.HashMap k Count -> HM.HashMap k Count
      incr k = HM.insertWith (+) k 1

      -- addTerm :: Model category -> Term -> Model category
      addTerm model t =
        -- Need to incr. wordcount and totalWordCt (easy)
        -- Need to incr. count of category in the toplevel HM..
        Model { wordCategoryCounts =
                  HM.insertWith (HM.unionWith (+))   -- just add counts together if already present
                                t                    -- keyed by the term
                                (HM.singleton cat 1) -- with a count of 1
                                (wordCategoryCounts model)
              , wordCounts =
                  incr t (wordCounts model)
              , totalWordCount = 1 + totalWordCount model
              }
  in
    foldl addTerm model (termsOfString remark)



freqOfWord :: Model category -> Term -> Count
freqOfWord model t =
  HM.lookupDefault 0 t (wordCounts model)


-- roughly, ... how many times occurs in that cat / how many times the word occurs
-- sums to 1
-- would make the most sense, right?
categoryProbabilities :: Model category -> Term -> [(category, Probability)]
categoryProbabilities model t =
  -- The counts of the categories, divided-by the count of the word.
  let termCt = freqOfWord model t
      prob ct = fromIntegral ct / fromIntegral termCt

      -- catCts :: HM.HashMap category Count
      catCts = HM.lookupDefault HM.empty t (wordCategoryCounts model)
  in
    map (\(cat, ct) -> (cat, prob ct)) (HM.toList catCts)



-- for each word, take freqOfWord * category of probabilities,
--                       Prob     * [(Cat, Prob)]  -> [(Cat, Prob)]
--  ^^ but this doesn't add to 1, right?
-- then, combine these [(Cat, Prob)] from each of the words, sum(1/n * each?)
-- & sort the one with largest prob. ...
probableCategories :: (Eq category, Hashable category) =>
                      Model category -> String -> [(category, Probability)]
probableCategories model phrase =
  let scale :: Rational -> Rational -> Rational
      scale s p        = s * p
      terms            = termsOfString phrase
      catProbs         = categoryProbabilities model
      scaledCatProbs t =
        -- Scale the terms by inverse of (wordCt / totalWordCt),
        -- i.e. words which occur rarely have larger impact.
        let k :: Rational
            k = fromIntegral (totalWordCount model)
              / fromIntegral (freqOfWord model t)
        in  map (\(c,p) -> (c, scale k p)) (catProbs t)

      -- Just combine using hashmaps
      hashMaps = map (HM.fromList . scaledCatProbs) terms
      -- Since the `k` above not normalised,
      -- the additions won't be proper 'probabilities'
      combine = HM.unionWith (+)
      categories = HM.toList $ foldl combine HM.empty hashMaps

      -- Normalise
      normalisedCategories =
        let totalP = sum $ map snd categories
        in  map (\(c,p) -> (c, p / totalP)) categories
  in
    reverse $ sortOn snd normalisedCategories
