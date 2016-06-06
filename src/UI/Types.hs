module UI.Types where



-- INTERFACE TYPE
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

type CategorisePrompt = (String, [(Maybe String, [String])])

