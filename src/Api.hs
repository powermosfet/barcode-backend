{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import qualified Api.Product as Product
import App (AppM, runAppM)
import Servant ((:>), Application, Proxy(..), ServerT, hoistServer, serve)
import qualified Config

type Api = "api" :> ("product" :> Product.Api)

api :: Proxy Api
api = Proxy

server :: ServerT Api AppM
server = Product.server

app :: Config.DbConfig -> Application
app config = serve api $ hoistServer api (runAppM config) server
