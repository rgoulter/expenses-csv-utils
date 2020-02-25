{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign
import Foreign.C.Types

foreign import ccall unsafe "add1"
     c_add1 :: CInt -> CInt


main :: IO ()
main = do
  putStrLn "hello world"
  let x = c_add1 5
  putStrLn $ "x is " ++ (show x)
