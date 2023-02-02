module Lib where

import Data.Text qualified as Text
import Database.Redis qualified as Redis
import Handler (Env (..))
import Handler qualified
import Handler.Forecast
import Relude
import System.Environment qualified as Environment
import Web.Scotty (ScottyM)
import Web.Scotty qualified as Scotty

seedData :: [(ByteString, ByteString)]
seedData =
    [ ("Bethesda", "lat=38.9816047&lon=-77.1535962")
    ]

handler :: Env -> ScottyM ()
handler env = do
    Scotty.get "/forecast" $
        Handler.runHandler
            env
            Handler.Forecast.getForecast
            Handler.Forecast.perform
    pure ()

server :: IO ()
server = do
    redisConn <- Redis.connect Redis.defaultConnectInfo
    apiKey <- getEnv "API_KEY"
    Scotty.scotty 9966 $ handler (Env{apiKey, redisConn})

getEnv :: String -> IO Text
getEnv name = do
    search <- Environment.lookupEnv name
    case search of
        Nothing -> error $ "Missing environment variable: " <> Text.pack name
        Just value -> pure $ Text.pack value