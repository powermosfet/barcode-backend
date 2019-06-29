module Lib
  ( barcodeBackend
  ) where

import qualified Api
import qualified Config
import Network.Wai.Handler.Warp (run)
import System.Environment (getEnvironment)

barcodeBackend :: IO ()
barcodeBackend = do
  env <- getEnvironment
  case Config.fromEnvironment env of
    Left err -> putStrLn err
    Right (port, dbConfig) -> do
      conn <- Config.connectDb dbConfig
      run port $ Api.app conn
