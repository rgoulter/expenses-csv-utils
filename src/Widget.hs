import Control.Monad (forM_)

import Data.Bits ((.&.), (.|.), complement)

import Data.Char (isPrint)
import qualified Data.List as L

import System.Win32.Types
import System.Win32.Console
  ( eNABLE_VIRTUAL_TERMINAL_PROCESSING
  , eNABLE_VIRTUAL_TERMINAL_INPUT
  , eNABLE_ECHO_INPUT
  , eNABLE_LINE_INPUT
  , eNABLE_PROCESSED_INPUT
  , eNABLE_WINDOW_INPUT
  , getConsoleMode
  , setConsoleMode
  )
import Graphics.Win32.Misc (sTD_INPUT_HANDLE, getStdHandle)

import System.IO
import Control.Monad (when)

import qualified System.Console.ANSI as ANSI

data Direction
  = UpA | DownA | RightA | LeftA

data Input
  = ArrowKey Direction
  | Del
  | Enter
  | Esc
  | Printable Char

data WidgetConfig = WidgetConfig
  { widgetContext :: String
  , widgetOptions :: [String]
  }

data WidgetState = WidgetState
  { widgetConfig :: WidgetConfig
  , widgetSelected :: Int
  , widgetValue :: String
  }

data WidgetMsg
  = Cancelled
  | Submitted String
  | Updated WidgetState


getKey :: IO String
getKey =
  let
    getKey' chars = do
      char <- getChar
      more <- hWaitForInput stdin 10
      (if more then getKey' else return) (char:chars)
  in reverse <$> getKey' ""

widgetInput :: IO Input
widgetInput = do
  key <- getKey
  return $ case key of
    "\ESC[A" -> ArrowKey UpA
    "\ESC[B" -> ArrowKey DownA
    "\ESC[C" -> ArrowKey RightA
    "\ESC[D" -> ArrowKey LeftA
    "\DEL"   -> Del
    "\n"     -> Enter
    "\r"     -> Enter
    "\ESC"   -> Esc
    c:_      -> Printable c   -- Ignores input if typed too quickly


updateWidget :: WidgetState -> Input -> WidgetMsg
updateWidget ws Esc = Cancelled
updateWidget ws@(WidgetState { widgetSelected = idx, widgetValue = s, widgetConfig = WidgetConfig { widgetOptions = opts } }) Enter
  | idx == 0 = Submitted s
  | otherwise = Submitted $ opts !! (idx - 1)
updateWidget ws (ArrowKey direction) = case direction of
  UpA   -> Updated (ws { widgetSelected = max 0 $ (widgetSelected ws) - 1 })
  DownA -> Updated (ws { widgetSelected = min (length $ widgetOptions $ widgetConfig ws) ((widgetSelected ws) + 1) })
  _    -> Updated ws
updateWidget ws@(WidgetState { widgetSelected = idx, widgetValue = s }) Del
  | idx == 0 && not (null s) = Updated ws { widgetValue = take (length s - 1) s }
  | otherwise = Updated ws
updateWidget ws@(WidgetState { widgetSelected = idx, widgetValue = s }) (Printable c)
  | idx == 0 && isPrint c = Updated ws { widgetValue = s ++ (c:"") }
  | otherwise = Updated ws


-- data WidgetConfig = WidgetConfig
--   { widgetContext :: String
--   , widgetOptions :: [String]
--   }

-- data WidgetState = WidgetState
--   { widgetConfig :: WidgetConfig
--   , widgetSelected :: Int
--   , widgetInput :: String
--   }
renderWidget :: WidgetState -> IO ()
renderWidget ws@WidgetState { widgetValue = widgetVal, widgetSelected = widgetIdx } =
  let renderPrompt =
        putStrLn "Input value for:"
      indent i s =
        let pad = replicate i ' '
        in unlines $ (pad ++) <$> lines s
      renderContext = do
        let ctx = widgetContext $ widgetConfig ws
            indentedCtx = indent 2 ctx
        ANSI.setSGR [ ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.White
                    ]
        putStrLn indentedCtx
        ANSI.setSGR []
      renderInput = do
        putStr "0. "
        -- if widgetIdx == 0
        --   then ANSI.showCursor
        --   else ANSI.hideCursor
        if 0 == widgetIdx
          then ANSI.setSGR [ ANSI.SetUnderlining ANSI.SingleUnderline
                           , ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Cyan
                           ]
          else ANSI.setSGR [ ANSI.SetUnderlining ANSI.SingleUnderline ]
        putStr $ take 20 $ widgetVal ++ L.repeat ' '
        ANSI.setSGR []
        putStrLn ""
      renderSuggestion (i, s) = do
        putStr $ show i ++ ". "
        if i == widgetIdx
          then ANSI.setSGR [ ANSI.SetUnderlining ANSI.SingleUnderline
                           , ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Cyan
                           ]
          else ANSI.setSGR []
        putStr s
        ANSI.setSGR []
        putStrLn ""
      renderSuggestions =
        forM_ (zip [1 ..] (widgetOptions $ widgetConfig ws)) renderSuggestion
  in do
    ANSI.hideCursor
    renderPrompt
    renderContext
    renderInput
    renderSuggestions

initState :: WidgetConfig -> WidgetState
initState cfg = WidgetState
  { widgetConfig = cfg
  , widgetSelected = 0
  , widgetValue = ""
  }

runWidget :: WidgetConfig -> IO (Maybe String)
runWidget widgetConfig =
  let loop ws = do
        ANSI.saveCursor
        -- ANSI.clearFromCursorToScreenEnd
        renderWidget ws
        i <- widgetInput
        let msg = updateWidget ws i
        case msg of
          Cancelled -> return Nothing
          Submitted s -> return (Just s)
          Updated ws' -> do
            ANSI.restoreCursor
            loop ws'
  in loop $ initState widgetConfig


-- Win32
withC :: (() -> IO ()) -> IO ()
withC cb = do
  stdinHdl <- getStdHandle sTD_INPUT_HANDLE
  oldConsoleMode <- getConsoleMode stdinHdl
  let newConsoleMode = (oldConsoleMode .&.
                       complement (eNABLE_ECHO_INPUT
                               .|. eNABLE_LINE_INPUT
                               .|. eNABLE_PROCESSED_INPUT
                               )) .|. eNABLE_VIRTUAL_TERMINAL_INPUT
  setConsoleMode stdinHdl newConsoleMode

  cb ()

  setConsoleMode stdinHdl oldConsoleMode


main :: IO ()
main = do
  hSetNewlineMode stdin noNewlineTranslation
  hSetBuffering stdout NoBuffering
  withC (\() -> do
    let cfg = WidgetConfig
              { widgetContext = "Spent 5 SGD\non McDonalds"
              , widgetOptions = ["Expenses:Food", "Expenses:Drinks", "Expenses:Dessert"]
              }
    maybeRes <- runWidget cfg
    print maybeRes)
