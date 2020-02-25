import Control.Monad (forM_)

import Data.Bits ((.&.), (.|.), complement)

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

import qualified System.Console.ANSI as ANSI
import qualified System.Console.ANSI.Types as SGR

import System.IO


data Input
  -- = IChar Char
  = Up      -- \ESC [ A
  | Down    -- \ESC [ B
  | Enter


data Model = Model
  { selectPrompt :: String
  , selectOptions :: [String]
  , selectIndex :: Int
  }


withC :: (() -> IO String) -> IO String
withC cb = do
  stdinHdl <- getStdHandle sTD_INPUT_HANDLE
  oldConsoleMode <- getConsoleMode stdinHdl
  let newConsoleMode = (oldConsoleMode .&.
                       complement (
                                eNABLE_ECHO_INPUT
                               .|. eNABLE_LINE_INPUT
                               -- .|. eNABLE_PROCESSED_INPUT
                               )) -- .|. eNABLE_VIRTUAL_TERMINAL_INPUT
  setConsoleMode stdinHdl newConsoleMode

  result <- cb ()

  setConsoleMode stdinHdl oldConsoleMode

  return result


renderSelect :: Model -> IO ()
renderSelect model@(Model { selectPrompt = prompt
                          , selectOptions = options
                          , selectIndex = index
                          }) = do
  ANSI.hideCursor

  putStrLn prompt

  let renderOption (option, idx) = do
        putStr $ show idx
        putStr ". "
        let sgr =
              if index == idx then
                SGR.SetColor SGR.Foreground SGR.Vivid SGR.Cyan
              else
                SGR.SetColor SGR.Foreground SGR.Dull SGR.White
        ANSI.setSGR [sgr]
        putStrLn option
        ANSI.setSGR [SGR.Reset]

  forM_ (zip options [0..]) renderOption

  -- put the cursor back where it was
  ANSI.cursorUpLine 4 -- MAGIC
  return ()


handleSelectInput :: IO Input
handleSelectInput =
  let
    helper acc = do
      c <- getChar
      ANSI.cursorUpLine 1
      putStrLn $ "gotChar: " ++ (show c)
      case (c:acc) of
        'A':'[':'\ESC':_ -> return Up
        'B':'[':'\ESC':_ -> return Down
        '\n':_ -> return Enter
        '\r':_ -> return Enter
        _ -> helper (c:acc)
  in
    helper []


update :: Input -> Model -> (Model, Maybe String)
update input model@(Model {selectIndex = idx}) =
  case input of
    Up ->
      let
        idx' = if idx > 0 then idx - 1 else idx
      in
        (model { selectIndex = idx' }, Nothing)

    Down ->
      let
        idx' = if idx < 2 then idx + 1 else idx -- MAGIC
      in
        (model { selectIndex = idx' }, Nothing)

    Enter ->
      (model, Just "res")


select :: String -> [String] -> IO String
select prompt options =
  let
    initModel = Model { selectPrompt = prompt
                  , selectOptions = options
                  , selectIndex = 0
                  }
    select' model = do
      renderSelect model
      input <- handleSelectInput
      let (model', selected) = update input model
      case selected of
        Nothing -> select' model'
        Just result -> return result
  in do
    res <- select' initModel
    ANSI.cursorDownLine 4 -- MAGIC
    ANSI.showCursor
    return res

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where getKey' chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)

helper :: IO String
helper =
  let
    h acc = do

      c <- getChar
      putStrLn $ "gotChar: " ++ (show c)
      rdy <- hReady stdin
      putStrLn $ "  ready? " ++ (show rdy)
      case c of
        'q' -> return ()
        _ -> h (c:acc)
  in do
    h []
    return "X"

helperK :: IO String
helperK =
  let
    h = do
      k <- getKey
      case k of
        "\r\n" -> putStrLn "NL"
        "\ESC[A" -> putStrLn "UP"
        "\ESC[B" -> putStrLn "DOWN"
        "q" -> return ()
        _ -> do
          putStrLn $ "k is: " ++ (show k)
          h
  in do
    h
    return "X"


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  result <- withC (\() ->
    -- select "Choose:" ["Apple", "Banana", "Carrot"]
    helper
    )
  putStrLn $ "Selected: " ++ result
