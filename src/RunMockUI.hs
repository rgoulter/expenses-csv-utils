module Main where

import Control.Monad (void, forM_)
import qualified Brick.Main as M

import UI.Types (CategorisePrompt)
import UI (initialState, theApp, St(..))



samplePrompt :: CategorisePrompt
samplePrompt = ( "Spent 10 SGD remark"
               , [ (Just "initA", ["item1", "item2", "item3", "item4", "item5"])
                 , (Just "initB", ["itemA", "itemB", "itemC"])
                 ]
               )



-- `m`, some model for computing the results,
-- `res`, the values of edit.
nextSamplePrompt :: () -> [String] -> IO ((), CategorisePrompt)
nextSamplePrompt () res = do
  putStrLn "Outputs:"
  forM_ res putStrLn
  putStrLn ""
  -- Doesn't matter, just do like this.
  return ((), samplePrompt)



main :: IO ()
main =
  -- initialState :: CategorisePrompt -> UpdateFn -> [ Maybe String ]
  let initState = initialState samplePrompt nextSamplePrompt ()
  in  void $ M.defaultMain theApp initState

