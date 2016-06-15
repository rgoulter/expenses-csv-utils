module Expenses.Merge.Constructive where

import Data.Function (on)
import Data.List (find, groupBy, concatMap)

import Entry



data EntryCmp = Different -- different (date,price,remark)
              | Matched Entry -- Just take the latest/most specific
              | Conflicted Entry Entry



compareEntries :: Entry -> Entry -> EntryCmp
compareEntries freshEntry oldEntry =
  let key e = (entryDate e, entryPrice e, entryRemark e)
      isAllUncat e = all (Uncategorised ==) (entryCategories e)
  in
    if key freshEntry == key oldEntry then
      -- Roughly, if freshEntry is all-Uncat, then
      -- just return the oldEntry.
      if isAllUncat freshEntry then
        Matched oldEntry
      -- Otherwise, if oldEntry is all-Uncat, great.
      --            otherwise, conflict!
      else if isAllUncat oldEntry then
        Matched freshEntry
      else
        Conflicted freshEntry oldEntry
    else
      Different



-- TODO: I'd prefer if this were more maintainable
--
-- `mergeEntryLists` is analogous to DropBox syncing.
--
-- It only handles cases where the (date,price,remark) are unchanged,
-- and only the Categorisation has been changed.
-- i.e. can't handle cases where remark is changed, or entries are
-- removed.
mergeEntryLists :: [Entry] -> [Entry] -> [Entry]
mergeEntryLists freshEntries oldEntries =
  let key e = (entryDate e, entryPrice e, entryRemark e)
      -- Helper assumes the two input lists have the same date.
      helper freshEntries oldEntries result =
        case (freshEntries, oldEntries) of
          ([], xs) -> result ++ xs
          (xs, []) -> result ++ xs
          (x:xs, y:ys) ->
            let -- if item exists in list, return Just of combination of both,
                -- otherwise, return Nothing
                aux it ls =
                  case find (\e -> key e == key it) ls of
                    Just it' ->
                      -- gotta combine `it`, `it'`.
                      case compareEntries it it' of
                        Matched res        -> Just res
                        -- `Conflicted` shouldn't happen,
                        -- but resolve arbitrarily.
                        Conflicted lhs rhs -> Just lhs
                        Different          -> Nothing
                    Nothing  -> Nothing
            in
              case (aux x (y:ys), aux y (x:xs)) of
                (Nothing, _) ->
                  -- Head of LHS doesn't appear in RHS; prefer that.
                  helper xs (y:ys) (result ++ [x])
                (Just x, Nothing) ->
                  -- Head of RHS doesn't appear in LHS; prefer that.
                  helper (x:xs) ys (result ++ [y])
                (Just x, Just y) ->
                  -- Both heads occur in other side,
                  -- if x,y the same, then output both,
                  if key x == key y then
                    helper xs ys (result ++ [x])
                  else
                    -- Could take either LHS or RHS; just pick LHS
                    -- (Need to filter first x from ys)
                    let filterFirst [] = []
                        filterFirst (y:ys) | key x == key y = ys
                        filterFirst ls = ls
                    in
                      helper xs (y:filterFirst ys) (result ++ [x])

      -- Group [Entry] into [(Date,[Entry])],
      -- then 'merge' using the above helper function when both dates equal
      -- (or concat the [Entry] from the earlier Date)
      keyedEntryGroups :: [Entry] -> [((Int, Int, Int), [Entry])]
      keyedEntryGroups entries = map (\es -> (entryDate $ head es, es))
                                     $ groupBy ((==) `on` entryDate) entries
      keyedFreshEntries = keyedEntryGroups freshEntries
      keyedOldEntries   = keyedEntryGroups oldEntries

      mergeAux :: Ord date => [(date, [Entry])] -> [(date, [Entry])] -> [Entry]
      mergeAux freshEntries oldEntries =
        case (freshEntries, oldEntries) of
          (xs, []) -> concatMap snd xs
          ([], xs) -> concatMap snd xs
          ((d1,es1):xs, (d2,es2):ys) | d1 < d2 ->
            es1 ++ mergeAux xs oldEntries
          ((d1,es1):xs, (d2,es2):ys) | d1 > d2 ->
            es2 ++ mergeAux freshEntries ys
          ((d1,es1):xs, (d2,es2):ys) ->
            -- Both the same date; merge using helper
            helper es1 es2 [] ++ mergeAux xs ys
  in
    mergeAux keyedFreshEntries keyedOldEntries
