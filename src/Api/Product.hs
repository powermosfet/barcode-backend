{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Api.Product where

import App (AppM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Aeson (FromJSON, ToJSON)
import Database.HDBC (SqlValue(SqlString), fromSql, quickQuery', run)
import GHC.Generics (Generic)
import Servant
  ( (:<|>)(..)
  , (:>)
  , Capture
  , Get
  , JSON
  , Post
  , Put
  , ReqBody
  , ServantErr
  , ServerT
  , err404
  , err500
  , throwError
  )

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

productFromSql :: ServantErr -> [SqlValue] -> AppM Product
productFromSql _ [barcode, description] =
  return $ Product (Barcode (fromSql barcode)) (fromSql description)
productFromSql err _ = throwError err

type ListApi = Get '[ JSON] [Product]

type SingleApi = Capture "barcode" String :> Get '[ JSON] Product

type PostApi = ReqBody '[ JSON] Product :> Post '[ JSON] Product

type PutApi
   = Capture "barcode" String :> ReqBody '[ JSON] Product :> Put '[ JSON] Product

type Api = ListApi :<|> SingleApi :<|> PostApi :<|> PutApi

server :: ServerT Api AppM
server = getList :<|> getSingle :<|> post :<|> put

getList :: AppM [Product]
getList = do
  conn <- ask
  results <- liftIO $ quickQuery' conn "SELECT * FROM product;" []
  mapM (productFromSql err500) results

getSingle :: String -> AppM Product
getSingle barcode = do
  conn <- ask
  results <-
    liftIO $
    quickQuery'
      conn
      "SELECT * FROM product where barcode = ? LIMIT 1;"
      [SqlString barcode]
  productFromSql err404 (concat results)

post :: Product -> AppM Product
post prod@(Product (Barcode barcode) description) = do
  conn <- ask
  result <-
    liftIO $
    run
      conn
      "INSERT INTO product VALUES (?, ?);"
      [SqlString barcode, SqlString description]
  if result == 1
    then return prod
    else throwError err500

put :: String -> Product -> AppM Product
put barcode (Product _ description) = do
  conn <- ask
  result <-
    liftIO $
    run
      conn
      "UPDATE product SET description=? WHERE barcode=?;"
      [SqlString description, SqlString barcode]
  if result == 1
    then return (Product (Barcode barcode) description)
    else throwError err404
