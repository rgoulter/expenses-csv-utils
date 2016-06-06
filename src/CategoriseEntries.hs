module CategoriseEntries where

import Text.CSV (CSV, printCSV, parseCSVFromFile)

import Control.Monad (void)

import qualified Categorise as M
import qualified ParseExpensesDoc as D

import UI.Types (CategorisePrompt)



type ProcessModel = ([D.Entry], [D.Entry], [M.Model D.Category])



-- (Purely) go through the entries, updating the model
nextModel :: ProcessModel -> ProcessModel
nextModel (toProcess, processed, models) =
  case toProcess of
    []   -> ([], processed, models)

    e:es ->
      if D.Uncategorised `elem` (D.entryCategories e) then
        -- Found an entry which hasn't been (fully) categorised,
        -- can't further add to models.
        (toProcess, processed, models)
      else
        -- Add to each of the models.
        -- ASSUMPTION that len(models) >= len(E.categories)
        -- Also note that no `Uncategorised`.
        let models' = map (\(c, m) ->
                             let remark = (D.entryRemark e)
                             in  M.addEntry m (remark, c))
                          (zip (D.entryCategories e) models)
        in  nextModel (es, e:processed, models')



emptyPrompt :: CategorisePrompt
emptyPrompt = ("", [(Nothing, []), (Nothing, [])])



promptFromEntry :: D.Entry -> [M.Model D.Category] -> CategorisePrompt
promptFromEntry e models =
  let remark = D.entryRemark e
      suggestionsFromCategory (c, model) =
        let probable = M.probableCategories model remark
            -- MAGIC assumption that UI has only 5.
            sug = take 5 $ map (\(c,_) -> D.stringFromCategory c) probable
            mt =
              case c of
                D.Uncategorised -> Nothing
                D.Category category -> Just category
        in  (mt, sug)
      suggestions =
        map suggestionsFromCategory (zip (D.entryCategories e) models)
  in (remark, suggestions)



promptFromModel :: ProcessModel -> CategorisePrompt
promptFromModel (todo,_,models) =
  case todo of
    []  -> emptyPrompt
    e:_ -> promptFromEntry e models



initFromCSV :: CSV -> (ProcessModel, CategorisePrompt)
initFromCSV csv =
  let entries = D.entriesFromCSV csv
      -- ASSUMPTION: 2 categories
      model = nextModel (entries, [], [M.emptyModel, M.emptyModel])
      prompt = promptFromModel model
  in
    (model, prompt)



-- `m`, some model for computing the results,
-- `res`, the values of edit.
nextPrompt :: ProcessModel -> [String] -> IO (ProcessModel, CategorisePrompt)
nextPrompt ([],  done,m) newCategories = return (([],done,m), emptyPrompt)
nextPrompt (e:es,done,m) newCategories =
  -- call nextModel, updating the head of "to-process" entries w/ the cats.,
  let e' = e { D.entryCategories = map D.categoryFromString newCategories }
      model' = nextModel (e':es,done,m)
  in do
    -- XXX write the whole CSV to file. (or wait till exit?).

    return (model', promptFromModel model')

