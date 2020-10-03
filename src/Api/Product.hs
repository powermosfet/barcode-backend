{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Api.Product where

import App (AppM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Aeson
    ( FromJSON
    , Options
    , ToJSON
    , defaultOptions
    , fieldLabelModifier
    , genericParseJSON
    , genericToJSON
    , parseJSON
    , toJSON
    )
import Data.Char (toLower)
import Database.HDBC (SqlValue(SqlString), fromSql, quickQuery', run, commit)
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
import qualified Config 

newtype Barcode =
    Barcode String
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Product =
    Product
        { productBarcode :: Barcode
        , productDescription :: String
        }
    deriving (Eq, Show, Generic)

aesonOptions :: Options
aesonOptions = defaultOptions {fieldLabelModifier = map toLower . drop 7}

instance ToJSON Product where
    toJSON = genericToJSON aesonOptions

instance FromJSON Product where
    parseJSON = genericParseJSON aesonOptions

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
    dbConfig <- ask
    let query = "SELECT * FROM product;"
    results <- liftIO $ Config.withDb dbConfig (\conn -> quickQuery' conn query [])
    mapM (productFromSql err500) results

getSingle :: String -> AppM Product
getSingle barcode = do
    dbConfig <- ask
    let query = "SELECT * FROM product where barcode = ? LIMIT 1;"
    results <- liftIO $ Config.withDb dbConfig (\conn -> quickQuery' conn query [SqlString barcode])
    productFromSql err404 (concat results)

post :: Product -> AppM Product
post prod@(Product (Barcode barcode) description) = do
    dbConfig <- ask
    let query = "INSERT INTO product VALUES (?, ?);"
    result <- liftIO $ Config.withDb dbConfig (\conn -> do
        result <- run conn query [SqlString barcode, SqlString description]
        commit conn
        return result
        )
    if result == 1
        then return prod
        else throwError err500

put :: String -> Product -> AppM Product
put barcode (Product _ description) = do
    dbConfig <- ask
    let query = "UPDATE product SET description=? WHERE barcode=?;"
    result <- liftIO $ Config.withDb dbConfig (\conn -> do
        result <- run conn query [SqlString description, SqlString barcode]
        commit conn
        return result
        )
    if result == 1
        then return (Product (Barcode barcode) description)
        else throwError err404
