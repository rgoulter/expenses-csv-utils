module Data.Expenses.Query where

import Text.Printf (printf)

import Data.Expenses.Types (Entry(..), QueryAttribute(..))


attr :: String -> Maybe QueryAttribute
attr "earliest" = Just Earliest
attr "latest" = Just Latest
attr _ = Nothing



queryDirectives :: QueryAttribute -> [Entry] -> String
queryDirectives Earliest entries =
  printf "%4d-%02d-%02d" y m d
    where
      (y, m, d) = entryDate $ head entries
queryDirectives Latest entries =
  printf "%4d-%02d-%02d" y m d
    where
      (y, m, d) = entryDate $ last entries
