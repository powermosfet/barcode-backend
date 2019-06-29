{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Api.Product where

import App (AppM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Aeson
import Database.HDBC
import GHC.Generics
import Servant

newtype Barcode =
  Barcode String
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Product =
  Product
    { productBarcode :: Barcode
    , productDescription :: String
    }
  deriving (Eq, Show, Generic)

instance ToJSON Product

instance FromJSON Product

productFromSql :: [SqlValue] -> AppM Product
productFromSql [SqlString barcode, SqlString description] =
  return $ Product (Barcode barcode) description
productFromSql _ = throwError err500

type ListApi = Get '[ JSON] [Product]

type PostApi = ReqBody '[ JSON] Product :> Post '[ JSON] Product

type PutApi
   = Capture "id" Int :> ReqBody '[ JSON] Product :> Put '[ JSON] Product

type Api = ListApi :<|> PostApi :<|> PutApi

server :: ServerT Api AppM
server =
  getList :<|> post :<|> (\_ _ -> return (Product (Barcode "000000") "Ost"))

post :: Product -> AppM Product
post _ = return (Product (Barcode "000000") "Ost")

put :: Barcode -> Product -> AppM Product
put _ _ = return (Product (Barcode "000000") "Ost")

getList :: AppM [Product]
getList = do
  (_, conn) <- ask
  results <- liftIO $ quickQuery' conn "SELECT * FROM product;" []
  mapM productFromSql results
