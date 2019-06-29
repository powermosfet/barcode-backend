module Config where

import Database.HDBC.PostgreSQL (Connection, connectPostgreSQL)
import Text.Read (readMaybe)

data DbConfig =
  DbConfig
    { configDbHost :: String
    , configDbUser :: String
    , configDbPw :: String
    , configDbPort :: String
    , configDbName :: String
    }
  deriving (Show)

fromEnvironment :: [(String, String)] -> Either String (Int, DbConfig)
fromEnvironment env =
  let getEnv convert name =
        maybe (missingEnv name) Right (lookup name env >>= convert)
      getS = getEnv Just
      getI = getEnv readMaybe
      config =
        DbConfig <$> getS "DB_HOST" <*> getS "DB_USER" <*> getS "DB_PW" <*>
        getS "DB_PORT" <*>
        getS "DB_NAME"
      port = getI "PORT"
   in (,) <$> port <*> config

missingEnv :: String -> Either String a
missingEnv var = Left ("Could not find environment variable " ++ var)

makeConnectionString :: DbConfig -> String
makeConnectionString config =
  "user=" ++
  configDbUser config ++
  " password=" ++
  configDbPw config ++
  " host=" ++
  configDbHost config ++
  " port=" ++ configDbPort config ++ " dbname=" ++ configDbName config

connectDb :: DbConfig -> IO Connection
connectDb config = connectPostgreSQL (makeConnectionString config)
