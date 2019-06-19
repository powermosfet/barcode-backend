module Lib
    ( barcodeBackend
    ) where

import Network.Wai.Handler.Warp
import qualified Api

barcodeBackend :: IO ()
barcodeBackend = run 8080 Api.app
