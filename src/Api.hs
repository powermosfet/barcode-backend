{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import qualified Api.Product
import Servant (ServerT, Application, Handler, Proxy(..), (:>), serve, hoistServer)
import App (AppM, Context)
import Control.Monad.Trans.Reader (runReaderT)

type Api = "api" :>
    ( "product" :> Api.Product.Api
    )

api :: Proxy Api
api = Proxy

server :: ServerT Api AppM
server = Api.Product.server

nt :: Context -> AppM a -> Handler a
nt context x = runReaderT x context

app :: Context -> Application
app context = serve api $ hoistServer api (nt context) server

