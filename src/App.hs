module App
    ( DbConn
    , AppM
    , runAppM
    ) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Reader (runReaderT)
import Database.HDBC.PostgreSQL (Connection)
import Servant (Handler)

type DbConn = Connection

type AppM = ReaderT DbConn Handler

runAppM :: DbConn -> AppM a -> Handler a
runAppM conn x = runReaderT x conn
