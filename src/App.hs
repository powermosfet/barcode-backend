module App where

import Config (Config)
import Control.Monad.Reader (ReaderT)
import Database.HDBC.PostgreSQL (Connection)
import Servant (Handler)

type Context = (Config, Connection)

type AppM = ReaderT Context Handler
