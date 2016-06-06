module Main where

import Control.Monad (void)
import qualified Brick.Main as M

import UI

main :: IO ()
main =
  let initState = initialState samplePrompt nextSamplePrompt [ Just "initA", Just "initB" ]
  in  void $ M.defaultMain theApp initState
