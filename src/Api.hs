{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import qualified Api.Product as Product
import App (AppM, DbConn, runAppM)
import Servant ((:>), Application, Proxy(..), ServerT, hoistServer, serve)

type Api = "api" :> ("product" :> Product.Api)

api :: Proxy Api
api = Proxy

server :: ServerT Api AppM
server = Product.server

app :: DbConn -> Application
app conn = serve api $ hoistServer api (runAppM conn) server
