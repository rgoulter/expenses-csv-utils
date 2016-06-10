module UI.Types where



-- INTERFACE TYPE



-- Because I want to show probabilities with suggestions
-- (& perhaps other things),
-- use a typeclass to model this.
class Suggestion s where
  -- Render suggestion as a string, trying to keep
  -- within a given width.
  displaySuggestion :: Int -> s -> String

  -- What gets put in the Editor field when
  -- selected.
  contentOfSuggestion :: s -> String



{-
In order to be useful, we need:
  * Prompt
  * List-of-suggestions. (i.e. [[String]] or whatever),
    in this case, n=2
  * some (IO?) function with which to
    - output [String] (n=2, the values we had),
      & get new (prompt,[suggestions]) back.
      (IO, b/c presumably this func. writes to file).
-}

type CategorisePrompt s = (String, [(Maybe String, [s])])

