module Main where

import Control.Monad (void)
import qualified Brick.Main as M

import UI

{-
  type Term = String
  type Count = Natural
  type Probability = Rational

  data Model category = ...
  emptyModel    :: Model category
  termsOfString :: String -> [Term]
  addEntry      :: (Eq category, Hashable category) => Model category -> (String, category) -> Model category
  freqOfWord    :: Model category -> Term -> Count
  categoryProbabilities :: Model category -> Term -> [(category, Probability)]
  probableCategories    :: (Eq category, Hashable category) =>
                           Model category -> String -> [(category, Probability)]
-}



-- CategorisePrompt :: (String, [[String]])



-- samplePrompt :: CategorisePrompt
-- samplePrompt = ( "Spent 10 SGD remark"
--                , [ ["item1", "item2", "item3", "item4", "item5"]
--                  , ["itemA", "itemB", "itemC", "itemD", "itemE"]
--                  ]
--                )
--
--
--
-- nextSamplePrompt :: CategorisePrompt -> [String] -> IO (CategorisePrompt, [Maybe String])
-- nextSamplePrompt p res = do
--   putStrLn "Outputs:"
--   forM_ res putStrLn
--   putStrLn ""
--   -- Doesn't matter, just do like this.
--   return (samplePrompt, [ Just "init1", Just "init2" ])

{-
 So ... we want to do...
 load CSV as list of entries,
   for each entry: (& its categories)
     if cat specified, build on that model,
     if cat unspecified,
       prompt for input from the UI.
       save to the CSV file.
-}

initPrompt :: CategorisePrompt
initPrompt = ( "Spent 10 SGD remark"
               , [ (Just "initA", ["item1", "item2", "item3", "item4", "item5"])
                 , (Just "initB", ["itemA", "itemB", "itemC", "itemD", "itemE"])
                 ]
               )



-- `m`, some model for computing the results,
-- `res`, the values of edit.
nextPrompt :: m -> [String] -> IO (m, CategorisePrompt)
nextPrompt m res = do
  putStrLn "Got result"
  -- Doesn't matter, just do like this.
  return (m, initPrompt)




main :: IO ()
main =
  -- initialState :: CategorisePrompt -> UpdateFn -> [ Maybe String ]
  let initState = initialState initPrompt nextPrompt ()
  in  void $ M.defaultMain theApp initState
