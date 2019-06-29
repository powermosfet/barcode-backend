module Config where

import Database.HDBC.PostgreSQL (Connection, connectPostgreSQL)
import Text.Read (readMaybe)

data Config =
  Config
    { configPort :: Int
    , configDbHost :: String
    , configDbUser :: String
    , configDbPw :: String
    , configDbPort :: String
    , configDbName :: String
    }

fromEnvironment :: [(String, String)] -> Either String Config
fromEnvironment env =
  Config <$> maybe (missingEnv "PORT") Right (lookup "PORT" env >>= readMaybe) <*>
  maybe (missingEnv "DB_HOST") Right (lookup "DB_HOST" env) <*>
  maybe (missingEnv "DB_USER") Right (lookup "DB_USER" env) <*>
  maybe (missingEnv "DB_PW") Right (lookup "DB_PW" env) <*>
  maybe (missingEnv "DB_PORT") Right (lookup "DB_PORT" env) <*>
  maybe (missingEnv "DB_NAME") Right (lookup "DB_NAME" env)

missingEnv :: String -> Either String a
missingEnv var = Left ("Could not find environment variable " ++ var)

makeConnectionString :: Config -> String
makeConnectionString config =
  "user=" ++
  configDbUser config ++
  " password=" ++
  configDbPw config ++
  " hostaddr=" ++
  configDbHost config ++
  " port=" ++ configDbPort config ++ " dbname=" ++ configDbName config

connectDb :: Config -> IO Connection
connectDb config = connectPostgreSQL (makeConnectionString config)
