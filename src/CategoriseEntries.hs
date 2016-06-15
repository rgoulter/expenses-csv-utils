{-# LANGUAGE FlexibleInstances #-}

module CategoriseEntries where

import System.IO (FilePath)

import Text.CSV (CSV, printCSV, parseCSVFromFile)
import Text.Printf (printf)

import Control.Monad (void)

import qualified Categorise as M
import qualified ParseExpensesDoc as D

import UI.Types (CategorisePrompt, Suggestion(..))
-- import qualified UI.Types as T



type ProcessModel = ([D.Entry], [D.Entry], [M.Model D.Category])

-- type Suggestion = (D.Category, M.Probability)
type Sug = (D.Category, M.Probability)

instance Suggestion Sug where
  -- displaySuggestion :: Int -> Sug -> String
  displaySuggestion width (cat, prob) =
    let precision = 2
        fmt = printf "%%-%ds(%%.%df)" (width - precision - 4) precision
        s = D.stringFromCategory cat
        p = fromRational prob :: Double
    in  printf fmt s p

  -- contentOfSuggestion :: Suggestion -> String
  contentOfSuggestion (cat,_) = D.stringFromCategory cat



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



emptyPrompt :: CategorisePrompt Sug
emptyPrompt = ("", [(Nothing, []), (Nothing, [])])



promptFromEntry :: D.Entry -> [M.Model D.Category] -> CategorisePrompt Sug
promptFromEntry e models =
  let remark = D.entryRemark e
      suggestionsFromCategory (c, model) =
        let probable = M.probableCategories model remark
            -- MAGIC assumption that UI has only 5.
            sug = take 5 probable
            mt =
              case c of
                D.Uncategorised -> Nothing
                D.Category category -> Just category
        in  (mt, sug)
      suggestions =
        map suggestionsFromCategory (zip (D.entryCategories e) models)
  in (remark, suggestions)



promptFromModel :: ProcessModel -> CategorisePrompt Sug
promptFromModel (todo,_,models) =
  case todo of
    []  -> emptyPrompt
    e:_ -> promptFromEntry e models



initFromCSV :: CSV -> (ProcessModel, CategorisePrompt Sug)
initFromCSV csv =
  let entries = D.entriesFromCSV csv
      -- ASSUMPTION: 2 categories
      model = nextModel (entries, [], [M.emptyModel, M.emptyModel])
      prompt = promptFromModel model
  in
    (model, prompt)



updateModelWith :: ProcessModel -> [String] -> ProcessModel
updateModelWith (e:es,done,m) newCategories =
  let e' = e { D.entryCategories = map D.categoryFromString newCategories }
  in  nextModel (e':es,done,m)



writeModelToFile :: FilePath -> ProcessModel -> IO ()
writeModelToFile filename (todo,done,m) = do
  -- Need to reverse Done,
  putStrLn "Save to file..."
  let entries = reverse done ++ todo
      records = map D.recordFromEntry entries
      outp = printCSV records
  writeFile filename outp



-- `m`, some model for computing the results,
-- `res`, the values of edit.
nextPrompt :: ProcessModel -> [String] -> IO (ProcessModel, CategorisePrompt Sug)
nextPrompt ([],  done,m) newCategories = return (([],done,m), emptyPrompt)
nextPrompt model newCategories =
  -- call nextModel, updating the head of "to-process" entries w/ the cats.,
  let model' = updateModelWith model newCategories
  in do
    -- XXX write the whole CSV to file. (or wait till exit?).

    return (model', promptFromModel model')

