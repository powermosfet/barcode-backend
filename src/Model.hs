{-# LANGUAGE DeriveGeneric #-}

module Model where

import Data.Aeson
import GHC.Generics

data Product = Product
    { barcode :: String
    , description :: String
    }
    deriving (Show, Eq, Generic)

instance ToJSON Product
instance FromJSON Product
