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
  let getEnv convert name =
        maybe (missingEnv name) Right (lookup name env >>= convert)
      getS = getEnv Just
      getI = getEnv readMaybe
   in Config <$> getI "PORT" <*> getS "DB_HOST" <*> getS "DB_USER" <*>
      getS "DB_PW" <*>
      getS "DB_PORT" <*>
      getS "DB_NAME"

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
