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
            putStrLn "Database config:"
            print dbConfig
            putStr "\nStarting barcode api server on port "
            print port
            run port $ Api.app dbConfig
