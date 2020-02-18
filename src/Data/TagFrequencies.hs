module Data.TagFrequencies
  ( Table(..)
  , empty
  , addPhrase
  , tagsForExactPhrase
  , tagsByWords
  ) where

import qualified Data.List as L
import qualified Data.List.NonEmpty as NE

import qualified Data.Map.Strict as M
import qualified Data.Set as S



data SuggestionResult
  = Ambiguous (NE.NonEmpty String)
  | Exact String
  | None
  deriving (Show, Eq)



data Table =
  FrequencyTable
    { tableAllTags :: M.Map String Int
    , tablePhraseTags :: M.Map String (M.Map String Int)
    , tableWordTags :: M.Map String (M.Map String Int)
    }



empty :: Table
empty =
  FrequencyTable
    { tableAllTags = M.empty
    , tablePhraseTags = M.empty
    , tableWordTags = M.empty
    }



addPhrase :: String -> String -> Table -> Table
addPhrase phrase tag tbl =
  let
    incr k = M.insertWith (+) k 1
    allTags' = incr tag $ tableAllTags tbl
    addTagForKey k =
      M.insertWith (M.unionWith (+)) k (M.singleton tag 1)
    phraseTags' = addTagForKey phrase $ tablePhraseTags tbl
    phraseWords = L.words phrase
    wordTags' =
      L.foldr addTagForKey (tableWordTags tbl) phraseWords
  in
    FrequencyTable
      { tableAllTags = allTags'
      , tablePhraseTags = phraseTags'
      , tableWordTags = wordTags'
      }



tagsByFrequency :: M.Map String Int -> [String]
tagsByFrequency frequencies =
  let
    xs = M.toList frequencies
  in
  L.map snd $ L.sort $ L.map (\(k,c) -> (-c,k)) xs



frequenciesForPhrase :: String -> Table -> Maybe (M.Map String Int)
frequenciesForPhrase phrase tbl =
  M.lookup phrase $ tablePhraseTags tbl



frequenciesForWords
  :: String -> Table -> M.Map String Int
frequenciesForWords phrase tbl =
  let
    phraseWords = words phrase
    wordsSet = S.fromList phraseWords
    wordTagFrequencies = M.restrictKeys (tableWordTags tbl) wordsSet
  in
  if M.null wordTagFrequencies then
    -- No words intersect; so use tags from all words
    tableAllTags tbl
  else
    M.foldr (M.unionWith (+)) M.empty wordTagFrequencies



tagsForExactPhrase :: String -> Table -> Maybe [String]
tagsForExactPhrase phrase tbl =
  tagsByFrequency <$> frequenciesForPhrase phrase tbl



tagsByWords :: String -> Table -> [String]
tagsByWords phrase tbl =
  tagsByFrequency $ frequenciesForWords phrase tbl
