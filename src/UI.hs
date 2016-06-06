{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module UI where

import Data.Monoid ((<>))
import Data.Maybe (fromMaybe, maybeToList, listToMaybe)
import qualified Data.Text.Zipper as Z

import Control.Monad.IO.Class (liftIO)
import Lens.Micro
import Lens.Micro.TH
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Widgets.Core
  ( (<+>)
  , (<=>)
  , withAttr
  , hLimit
  , vLimit
  , str
  , padRight
  , padAll
  , Named(..)
  )
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import Brick.Util (on, fg)
import Brick.Markup (markup, (@?))



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



-- BRICK/VTY DECLARATIONS



expenseStrAttr  :: A.AttrName
expenseStrAttr  = "myApp" <> "expenseStr"
listFocusAttr   :: A.AttrName
listFocusAttr   = "myApp" <> "listFocus"
listUnfocusAttr :: A.AttrName
listUnfocusAttr = "myApp" <> "listUnfocus"
helpBarAttr     :: A.AttrName
helpBarAttr     = "myApp" <> "helpBar"

data Name = Edit1
          | List1
          | Edit2
          | List2
          | ConfirmBtn
          deriving (Ord, Show, Eq)

-- Use this for keeping track of modes/focus
data NamedBool = NamedB Name (Maybe Bool)

type ModalState = (Int, [NamedBool])

clearNamedBools :: [NamedBool] -> [NamedBool]
clearNamedBools =
  map (\(NamedB n b) ->
         let b' =
               case b of
                 Nothing -> Nothing
                 Just _ -> Just False
         in NamedB n b')

enableIdx :: Int -> [NamedBool] -> [NamedBool]
enableIdx idx namedBools =
  map (\(b, NamedB n mb) ->
         let mb' =
              case mb of
                Nothing -> Nothing
                Just bool -> Just b
         in NamedB n mb')
      (zip (map (idx ==) [0..]) -- Sequence of [True at idx, False otherwise]
           namedBools)

incrModalState :: ModalState -> ModalState
incrModalState (idx, namedBools) =
  ((idx + 1) `mod` length namedBools,
   clearNamedBools namedBools)


decrModalState :: ModalState -> ModalState
decrModalState (idx, namedBools) =
  ((idx - 1) `mod` length namedBools,
   clearNamedBools namedBools)

editModalState :: ModalState -> ModalState
editModalState (idx, namedBools) =
  (idx, enableIdx idx namedBools)

isEditing :: ModalState -> Bool
isEditing (idx, namedBools) =
  let (NamedB _ m) = namedBools !! idx
  in fromMaybe False m

-- "Maybe Name",
currentName :: ModalState -> Maybe Name
currentName (idx, namedBools) =
  let NamedB n _ = namedBools !! idx
  in  Just n

-- Don't need much more than a dumb list.
data MyList = List Name [String]

data St m =
  St { _modalState :: ModalState
     , _edit1 :: E.Editor Name
     , _list1 :: MyList
     , _edit2 :: E.Editor Name
     , _list2 :: MyList
     , _prompt :: CategorisePrompt
     , _updatePrompt :: m -> [String] -> IO (m, CategorisePrompt)
     , _promptState :: m
     }

makeLenses ''St



initialState :: CategorisePrompt
             -> (m -> [String] -> IO (m, CategorisePrompt))
             -> m
             -> St m
initialState prompt@(s,(mt1,sg1):(mt2,sg2):_) updateFn initState =
     -- I'm thinking this mightn't be the right model for our modal focus,
     -- don't need to know name of ConfirmBtn;
     -- & want to know to switch between Edit1/List1, etc.
  St { _modalState = (0,
                      [ NamedB Edit1 (Just False)
                      , NamedB Edit2 (Just False)
                      , NamedB ConfirmBtn Nothing
                      ])
     -- ASSUMPTION: For now, assume `prompt` contains at-least 2 suggestions
     , _edit1      = E.editor Edit1 (str . unlines) (Just 1) (fromMaybe "" mt1)
     , _list1      = List List1 sg1
     , _edit2      = E.editor Edit2 (str . unlines) (Just 1) (fromMaybe "" mt2)
     , _list2      = List List2 sg2
     , _prompt     = prompt
     , _updatePrompt = updateFn
     , _promptState = initState
     }



theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
  [ (E.editAttr,        V.white `on` V.blue)
  , (E.editFocusedAttr, V.black `on` V.yellow)
  , (expenseStrAttr,    fg V.cyan)
  , (listFocusAttr,     fg V.yellow)
  , (listUnfocusAttr,   fg V.blue)
  , (helpBarAttr,       V.rgbColor 144 144 144 `on` V.rgbColor 64 64 64)
  ]



-- <=> is "put on top",
-- <+> is "put beside"
drawUI :: St m -> [T.Widget Name]
drawUI st = [ui]
  where
    -- TODO: This pattern-match is dangerous and a kludge.
    (modalIdx, NamedB _ (Just b1):NamedB _ (Just b2):_) = _modalState st

    (promptStr, _) = _prompt st

    -- renderEditor  :: Bool -> Editor n -> Widget n
    e1  = E.renderEditor (modalIdx == 0 && b1)     (_edit1 st)
    l1  = renderList     (modalIdx == 0 && not b1) (_list1 st)
    e2  = E.renderEditor (modalIdx == 1 && b2)     (_edit2 st)
    l2  = renderList     (modalIdx == 1 && not b2) (_list2 st)

    cfm = renderConfirm (modalIdx == 2)

    -- Render our list widgets
    renderList :: Bool -> MyList -> T.Widget Name
    renderList isFocused (List _ ls) =
      let items =
            map (\(i,s) -> show i ++ ". " ++ s)
                (zip [1..] $ take 5 ls)
          attr = if isFocused then listFocusAttr else listUnfocusAttr
      in  withAttr attr $
          str $ unlines items

    renderConfirm :: Bool -> T.Widget Name
    renderConfirm isFocused =
      let attr = if isFocused then listFocusAttr else listUnfocusAttr
      in
        (padAll 2 $ C.hCenter $ withAttr attr $
            str "Confirm")

    ui =
      (str "Categorise" <=>
       (withAttr expenseStrAttr $ str promptStr))
      <=>
      B.hBorder
      <=>
      ((padAll 1 $
        str "Category 1:" <=>
        hLimit 30 (vLimit 1 e1) <=>
        l1)
       <+>
       (padAll 1 $
        str "Category 2: " <=>
        hLimit 30 (vLimit 1 e2) <=>
        l2)
       <+>
       cfm)
      <=>
      B.hBorder
      <=>
      (withAttr helpBarAttr $ padRight T.Max $
         str "Press Tab to switch between editors, Esc to quit.")



appEvent :: St m -> V.Event -> T.EventM Name (T.Next (St m))
appEvent st ev =
  let modalSt@(modalIdx,_) = _modalState st
      isEdit = isEditing modalSt

      (_,suggestions) = _prompt st

      -- XXX Strictly, this should only work for if the list has than idx..
      acceptsHotkey :: Char -> Bool
      acceptsHotkey c =
        c `elem` ['1'..'5']
      stringForHotkey :: Char -> Maybe String
      stringForHotkey c =
        -- Unsafe assumption that |suggestions| > |modalIdx|
        lookup c $ zip ['1'..] (snd (suggestions !! modalIdx))
  in case ev of
    -- Esc Quits the App
    V.EvKey V.KEsc         [] -> M.halt st


    -- Let user edit
    -- If user accidentally presses 'i', they have to type what they want anyway.
    -- ie. no way to un-edit other than to press ENTER.
    V.EvKey (V.KChar 'i') [] | not isEdit ->
      M.continue $ st & modalState %~ editModalState


    -- Cycle between "Focus Rings" (Col1, Col2, Cfm)
    V.EvKey V.KEnter      [] | modalIdx == 2 ->
      -- ASSUMPTION only 3x modal states; coupled that incrMS touches modalIdx
      -- If we're going back to 1st, need to:
      --   - clear the Edits,
      --   - refresh the suggestions
      let getFirstLine ed = fromMaybe "" . listToMaybe $ E.getEditContents ed
          txt1 = getFirstLine $ st ^. edit1
          txt2 = getFirstLine $ st ^. edit2
      in M.suspendAndResume $ do
        -- Filthy pattern match, ASSUMPTION of size 2
        (m', prompt'@(_,(initText1,_):(initText2,_):_)) <- _updatePrompt st (_promptState st) [txt1, txt2]
        let setTextZipper ms =
              Z.stringZipper (maybeToList ms) (Just 1)
            st' = st { _modalState  = incrModalState (_modalState st)
                     , _edit1       = E.applyEdit (\z -> setTextZipper initText1) (_edit1 st)
                     , _edit2       = E.applyEdit (\z -> setTextZipper initText2) (_edit2 st)
                     , _prompt      = prompt'
                     , _promptState = m'
                     }
        return $ st'

    V.EvKey V.KEnter      [] ->
      M.continue $ st & modalState %~ incrModalState

    V.EvKey (V.KChar n)   [] | not isEdit && acceptsHotkey n -> do
      -- TODO:LENS: I don't understand lenses enough to know the idiomatic case here
      let str  = fromMaybe "" $ stringForHotkey n
          edit = if modalIdx == 0 then edit1 else edit2
          setTextZipper =
            Z.stringZipper [str] (Just 1)
          st'  = st & edit %~ E.applyEdit (\z -> setTextZipper)
      M.continue $ st' & modalState %~ incrModalState

    V.EvKey (V.KChar 'e') [] | not (isEditing $ _modalState st) ->
      M.continue $ st & modalState %~ decrModalState


    _ -> M.continue =<< case _modalState st of
           (0, _) | isEdit ->
             T.handleEventLensed st edit1 E.handleEditorEvent ev
           (1, _) | isEdit ->
             T.handleEventLensed st edit2 E.handleEditorEvent ev
           _ -> return st



appCursor :: St m -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
appCursor st locs =
  let mn = currentName (_modalState st)
  -- in case mn of
  --      Nothing -> Nothing
  --      Just n ->
  in listToMaybe $ filter (\cl -> mn == T.cursorLocationName cl) locs



theApp :: M.App (St m) V.Event Name
theApp =
  M.App { M.appDraw = drawUI
        , M.appChooseCursor = appCursor
        , M.appHandleEvent = appEvent
        , M.appStartEvent = return
        , M.appAttrMap = const theMap
        , M.appLiftVtyEvent = id
        }


