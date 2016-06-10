module UI.ModalState where

-- Use this for keeping track of modes/focus
data NamedBool n = NamedB n n Bool
                 | NamedN n

type ModalState n = (Int, [NamedBool n])

clearNamedBools :: [NamedBool n] -> [NamedBool n]
clearNamedBools =
  map (\nb ->
         case nb of
           NamedB n1 n2 _ -> NamedB n1 n2 False
           NamedN n -> NamedN n)

enableIdx :: Int -> [NamedBool n] -> [NamedBool n]
enableIdx idx namedBools =
  map (\(b, nb) ->
         case nb of
           NamedB n1 n2 _ -> NamedB n1 n2 b
           NamedN n -> NamedN n)
      (zip (map (idx ==) [0..]) -- Sequence of [True at idx, False otherwise]
           namedBools)

incrModalState :: ModalState n -> ModalState n
incrModalState (idx, namedBools) =
  ((idx + 1) `mod` length namedBools,
   clearNamedBools namedBools)


decrModalState :: ModalState n -> ModalState n
decrModalState (idx, namedBools) =
  ((idx - 1) `mod` length namedBools,
   clearNamedBools namedBools)

editModalState :: ModalState n -> ModalState n
editModalState (idx, namedBools) =
  (idx, enableIdx idx namedBools)

isEditing :: ModalState n -> Bool
isEditing (idx, namedBools) =
  case namedBools !! idx of
    NamedB _ _ b -> b
    NamedN _ -> False

currentName :: ModalState n -> n
currentName (idx, namedBools) =
  case namedBools !! idx of
    NamedB n _ False -> n
    NamedB _ n True  -> n
    NamedN n -> n
