module Lib
  ( barcodeBackend
  ) where

import qualified Api
import qualified Config
import Network.Wai.Handler.Warp
import System.Environment

barcodeBackend :: IO ()
barcodeBackend = do
  env <- getEnvironment
  case Config.fromEnvironment env of
    Left err -> putStrLn err
    Right config -> do
      let port = Config.configPort config
      conn <- Config.connectDb config
      run port $ Api.app (config, conn)
