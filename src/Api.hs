{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Model (Product(..))
import Servant

type Api = "product" :> Get '[JSON] [Product]
      :<|> "product" :> ReqBody '[JSON] Product :> Post '[JSON] Product

server :: Server Api
server = return []
    :<|> (\_ -> return (Product "000000" "Ost"))

api :: Proxy Api
api = Proxy

app :: Application
app = serve api server
