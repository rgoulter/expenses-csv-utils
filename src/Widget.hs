import Data.Bits ((.&.), (.|.), complement)

import Data.Char (isPrint)

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

data Direction
  = Up | Down | Right | Left

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
  , widgetInput :: String
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
      more <- hReady stdin
      more <- hWaitForInput stdin 2
      (if more then getKey' else return) (char:chars)
  in reverse <$> getKey' ""

widgetInput :: IO Input
widgetInput = do
  key <- getKey
  return $ case key of
    "\ESC[A" -> ArrowKey Up
    "\ESC[B" -> ArrowKey Down
    "\ESC[C" -> ArrowKey Right
    "\ESC[D" -> ArrowKey Left
    "\DEL"   -> Del
    "\n"     -> Enter
    "\r"     -> Enter
    "\ESC"   -> Esc
    c:_      -> Printable c   -- Ignores input if typed too quickly


updateWidget :: WidgetState -> Input -> WidgetMsg
updateWidget ws input =
  -- XXX
  Updated ws


renderWidget :: WidgetState -> IO ()
renderWidget ws =
  -- XXX
  return ()

initState :: WidgetConfig -> WidgetState
initState cfg = WidgetState
  { widgetConfig = cfg
  , widgetSelected = 0
  , widgetInput = ""
  }

runWidget :: WidgetConfig -> IO (Maybe String)
runWidget widgetConfig =
  let loop ws = do
        -- XXX save cursor
        renderWidget ws
        i <- widgetInput
        msg <- updateWidget ws i
        case msg of
          Cancelled -> return Nothing
          Submitted s -> return (Just s)
          Updated ws' -> do
            -- XXX restore cursor
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
    putStrLn "running (win32, loop):"

    loop)
