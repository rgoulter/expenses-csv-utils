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

import UI.Types
import qualified UI.ModalState as MS



-- BRICK/VTY DECLARATIONS



expenseStrAttr  :: A.AttrName
expenseStrAttr  = "myApp" <> "expenseStr"
listFocusAttr   :: A.AttrName
listFocusAttr   = "myApp" <> "listFocus"
listUnfocusAttr :: A.AttrName
listUnfocusAttr = "myApp" <> "listUnfocus"
helpBarAttr     :: A.AttrName
helpBarAttr     = "myApp" <> "helpBar"

data Name = Edit Int
          | List Int
          | ConfirmBtn
          deriving (Ord, Show, Eq)

type NamedBool = MS.NamedBool Name

type ModalState = MS.ModalState Name



-- Would this benefit from being a record, lens access?
data CategoriseComponent s =
  Category { _edit :: E.Editor Name
           , _list :: Name
           , _suggestions :: [s]
           }

makeLenses ''CategoriseComponent



data St s m =
  St { _modalState :: ModalState
     , _categorisers :: [CategoriseComponent s]
     , _promptStr :: String
     , _updatePrompt :: m -> [String] -> IO (m, CategorisePrompt s)
     , _promptState :: m
     }

makeLenses ''St



initialState :: CategorisePrompt s
             -> (m -> [String] -> IO (m, CategorisePrompt s))
             -> m
             -> St s m
initialState prompt updateFn initState =
  let initModalStateFor :: [(Name, Name)] -> ModalState
      initModalStateFor names =
        let modes =
              foldr (\(listName,editName) nbs ->
                          MS.NamedB listName editName False : nbs)
                       [MS.NamedN ConfirmBtn]
                       names
        in (0, modes)
      st =
        St { _modalState = initModalStateFor [(List 1, Edit 1), (List 2, Edit 2)]
           , _categorisers =
               [ Category { _edit = E.editor (Edit 1) (str . unlines) (Just 1) ""
                          , _list = List 1
                          , _suggestions = []
                          }
               , Category { _edit = E.editor (Edit 2) (str . unlines) (Just 1) ""
                          , _list = List 2
                          , _suggestions = []
                          }
               ]
           , _promptStr    = ""
           , _updatePrompt = updateFn
           , _promptState  = initState
           }
  in stateWithPrompt st prompt



stateWithPrompt :: St s m -> CategorisePrompt s -> St s m
stateWithPrompt st (nPromptStr,catSuggestions) =
   let setTextZipper ms =
         Z.stringZipper (maybeToList ms) (Just 1)
       sugCompWith (initText, sg) catComp =
         catComp & edit . E.editContentsL .~ setTextZipper initText
                 & suggestions .~ sg
   in
     st & categorisers %~ zipWith sugCompWith catSuggestions
        & promptStr .~ nPromptStr



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
drawUI :: Suggestion s => St s m -> [T.Widget Name]
drawUI st = [ui]
  where
    focusName = st ^. modalState . to MS.currentName

    cfm = renderConfirm (focusName == ConfirmBtn)

    -- Combine the categorisers,
    mid = foldr (\(cat, idx) accum ->
                   renderCategory cat idx <+> accum)
                cfm
                (zip (st ^. categorisers) [1..])

    renderCategory :: Suggestion s => CategoriseComponent s -> Int -> T.Widget Name
    renderCategory c idx =
      let ed = E.renderEditor (focusName == getName (c ^. edit)) (c ^. edit)
          ls = renderList (focusName == (c ^. list))
                          (c ^.. suggestions . traverse . to (displaySuggestion 27))
      in padAll 1 $
        str ("Category " ++ show idx ++ ":") <=>
        hLimit 30 (vLimit 1 ed) <=>
        ls

    -- Render our list widgets
    renderList :: Bool -> [String] -> T.Widget Name
    renderList isFocused ls =
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
       withAttr expenseStrAttr (str (st ^. promptStr)))
      <=>
      B.hBorder
      <=>
      mid
      <=>
      B.hBorder
      <=>
      withAttr helpBarAttr (padRight T.Max $
         str "Press Tab to switch between editors, Esc to quit.")



appEvent :: Suggestion s => St s m -> V.Event -> T.EventM Name (T.Next (St s m))
appEvent st ev =
  let modalSt@(modalIdx,modes) = st ^. modalState
      isEdit = MS.isEditing modalSt

      -- sugs :: [[s]]
      sugs = st ^.. (categorisers . traverse . suggestions)

      -- XXX Strictly, this should only work for if the list has than idx..
      acceptsHotkey :: Char -> Bool
      acceptsHotkey c =
        c `elem` ['1'..'5']
      stringForHotkey :: Char -> Maybe String -- XXX Suggestion
      stringForHotkey c =
        -- Unsafe assumption that |sugs| > |modalIdx|
        lookup c $ zip ['1'..] (map contentOfSuggestion $ sugs !! modalIdx)
  in case ev of
    -- Esc Quits the App
    V.EvKey V.KEsc         [] -> M.halt st


    -- Let user edit
    -- If user accidentally presses 'i', they have to type what they want anyway.
    -- ie. no way to un-edit other than to press ENTER.
    V.EvKey (V.KChar 'i') [] | not isEdit ->
      M.continue $ st & modalState %~ MS.editModalState


    -- Cycle between "Focus Rings" (Col1, Col2, Cfm)
    V.EvKey V.KEnter      [] | modalIdx == length modes - 1 ->
      -- If we're going back to 1st, need to:
      --   - clear the Edits,
      --   - refresh the suggestions
      let getFirstLine ed = fromMaybe "" . listToMaybe $ E.getEditContents ed
          txt :: [String]
          txt = map getFirstLine $ st ^.. (categorisers . traverse . edit)
      in M.suspendAndResume $ do
        (m', prompt') <- (st ^. updatePrompt) (st ^. promptState) txt
        let st' = st & modalState %~ MS.incrModalState
                     & promptState .~ m'
        return $ stateWithPrompt st' prompt'

    V.EvKey V.KEnter      [] ->
      M.continue $ st & modalState %~ MS.incrModalState

    V.EvKey (V.KChar n)   [] | not isEdit && acceptsHotkey n ->
      let str = fromMaybe "" $ stringForHotkey n
          ed = categorisers . ix modalIdx . edit
          setTextZipper =
            Z.stringZipper [str] (Just 1)
      in
        M.continue $ st & ed . E.editContentsL .~ setTextZipper
                        & modalState %~ MS.incrModalState

    V.EvKey (V.KChar 'e') [] | not isEdit ->
      M.continue $ st & modalState %~ MS.decrModalState


    _ -> M.continue =<< case st ^. modalState of
           (idx, _) | isEdit &&
                      idx < length (st ^. categorisers) ->
             -- Can't use T.handleEventLensed out-of-the-box with the
             -- following lens.
             let ed :: Applicative f => (E.Editor Name -> f (E.Editor Name)) -> St s m -> f (St s m)
                 ed =  categorisers . ix idx . edit
                 edLens :: Lens' (St s m) (E.Editor Name)
                 edLens =
                   lens (^?! ed)
                        (\st newVal -> st & ed .~ newVal)
             in
               T.handleEventLensed st edLens E.handleEditorEvent ev
           _ ->
             return st



appCursor :: St s m -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
appCursor st locs =
  let mn = st ^. modalState . to MS.currentName . to Just
  in listToMaybe $ filter (\cl -> mn == T.cursorLocationName cl) locs



theApp :: Suggestion s => M.App (St s m) V.Event Name
theApp =
  M.App { M.appDraw = drawUI
        , M.appChooseCursor = appCursor
        , M.appHandleEvent = appEvent
        , M.appStartEvent = return
        , M.appAttrMap = const theMap
        , M.appLiftVtyEvent = id
        }


