module Data.Expenses.Ledger.Process
  ( xmlOf
  , xmlOfString
  ) where

import qualified Data.ByteString.Lazy.Char8 as LBS8

import qualified System.Process.Typed as TP



xmlOf :: FilePath -> IO String
xmlOf journal = do
  let ledgerConfig = TP.setStdin TP.nullStream
                   $ TP.setStdout TP.byteStringOutput
                   $ TP.proc "ledger" ["xml", "-f", journal]
  (_, stdout, _) <- TP.readProcess ledgerConfig
  return $ LBS8.unpack stdout



xmlOfString :: String -> IO String
xmlOfString journal = do
  let ledgerConfig = TP.setStdin (TP.byteStringInput $ LBS8.pack journal)
                   $ TP.setStdout TP.byteStringOutput
                   $ TP.proc "ledger" ["xml", "-f", "-"]
  (_, stdout, _) <- TP.readProcess ledgerConfig
  return $ LBS8.unpack stdout
