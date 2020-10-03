module App
    ( AppM
    , runAppM
    ) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Reader (runReaderT)
import Servant (Handler)
import Config (DbConfig)

type AppM = ReaderT DbConfig Handler

runAppM :: DbConfig -> AppM a -> Handler a
runAppM config x = runReaderT x config
