module Server (server) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Database.Redis qualified as Redis
import Handler (Env (..))
import Handler qualified
import Handler.Forecast qualified as Forecast
import Handler.Locations qualified as Locations
import Network.Wai (Application, Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Static (Policy)
import Network.Wai.Middleware.Static qualified as Static
import Relude
import System.Environment qualified as Environment
import Web.Scotty (ScottyM)
import Web.Scotty qualified as Scotty

staticMiddleware :: Middleware
staticMiddleware =
    Static.staticPolicy $
        serveClient <> Static.addBase "public" <> Static.noDots <> Static.addBase "../client"

serveClient :: Policy
serveClient = Static.policy defaultIndex
  where
    defaultIndex "" = Just "index.html"
    defaultIndex s = Just s

handler :: Env -> ScottyM ()
handler env = do
    Scotty.middleware staticMiddleware
    Scotty.get "/locations" $
        Handler.runHandler
            env
            Locations.getLocations
            Locations.perform

    Scotty.get "/locations/:city/weather" $ do
        city <- Scotty.param "city"
        Handler.runHandler
            env
            (Forecast.getForecast city)
            Forecast.perform
    pure ()

server :: IO ()
server = do
    redisConn <- Redis.connect Redis.defaultConnectInfo
    runSeedData redisConn
    apiKey <- getEnv "API_KEY"
    Scotty.scotty 9966 $ handler (Env{apiKey = apiKey, redisConn = redisConn})

runSeedData :: Redis.Connection -> IO ()
runSeedData redisConn = do
    let cities = Text.Encoding.decodeUtf8 . fst <$> seedData

    seedLatLon <- Redis.runRedis redisConn $ Redis.mset seedData
    seedCities <- Redis.runRedis redisConn $ Redis.set "cities" (LBS.toStrict $ Aeson.encode cities)

    case (seedLatLon, seedCities) of
        (Right _, Right _) -> pure ()
        (Left err, _) -> error $ "Failed to seed LatLong data: " <> show err
        (_, Left err) -> error $ "Failed to seed City data: " <> show err

getEnv :: String -> IO Text
getEnv name = do
    search <- Environment.lookupEnv name
    case search of
        Nothing -> error $ "Missing environment variable: " <> Text.pack name
        Just value -> pure $ Text.pack value

seedData :: [(ByteString, ByteString)]
seedData =
    [ ("Bethesda", LBS.toStrict . Aeson.encode $ Forecast.LatLon 38.9816047 (-77.1535962))
    , ("Washington", LBS.toStrict . Aeson.encode $ Forecast.LatLon 38.9071923 (-77.0368707))
    , ("New York", LBS.toStrict . Aeson.encode $ Forecast.LatLon 40.7127281 (-74.0060152))
    , ("London", LBS.toStrict . Aeson.encode $ Forecast.LatLon 51.5073219 (-0.1276474))
    , ("Paris", LBS.toStrict . Aeson.encode $ Forecast.LatLon 48.8566969 2.3514616)
    , ("Tokyo", LBS.toStrict . Aeson.encode $ Forecast.LatLon 35.6894875 139.6917064)
    , ("Sydney", LBS.toStrict . Aeson.encode $ Forecast.LatLon (-33.868820) 151.209296)
    , ("Moscow", LBS.toStrict . Aeson.encode $ Forecast.LatLon 55.755826 37.6173)
    , ("Beijing", LBS.toStrict . Aeson.encode $ Forecast.LatLon 39.9041999 116.4073963)
    , ("Cairo", LBS.toStrict . Aeson.encode $ Forecast.LatLon 30.0444196 31.2357116)
    , ("Rio de Janeiro", LBS.toStrict . Aeson.encode $ Forecast.LatLon (-22.9068467) (-43.1728965))
    , ("Santiago", LBS.toStrict . Aeson.encode $ Forecast.LatLon (-33.4488897) (-70.6692655))
    , ("Buenos Aires", LBS.toStrict . Aeson.encode $ Forecast.LatLon (-34.6036844) (-58.3815591))
    , ("Lima", LBS.toStrict . Aeson.encode $ Forecast.LatLon (-12.046374) (-77.0427934))
    , ("Mexico City", LBS.toStrict . Aeson.encode $ Forecast.LatLon 19.4326077 (-99.133208))
    , ("Bogota", LBS.toStrict . Aeson.encode $ Forecast.LatLon 4.7109886 (-74.072092))
    , ("Sao Paulo", LBS.toStrict . Aeson.encode $ Forecast.LatLon (-23.5506507) (-46.6333824))
    , ("Madrid", LBS.toStrict . Aeson.encode $ Forecast.LatLon 40.4167754 (-3.7037902))
    , ("Rome", LBS.toStrict . Aeson.encode $ Forecast.LatLon 41.9027835 12.4963655)
    , ("Berlin", LBS.toStrict . Aeson.encode $ Forecast.LatLon 52.5200066 13.4049539)
    , ("Amsterdam", LBS.toStrict . Aeson.encode $ Forecast.LatLon 52.3702157 4.8951679)
    , ("Barcelona", LBS.toStrict . Aeson.encode $ Forecast.LatLon 41.3850639 2.1734035)
    , ("Milan", LBS.toStrict . Aeson.encode $ Forecast.LatLon 45.4642035 9.189982)
    , ("Toronto", LBS.toStrict . Aeson.encode $ Forecast.LatLon 43.653226 (-79.3831843))
    , ("Montreal", LBS.toStrict . Aeson.encode $ Forecast.LatLon 45.5016889 (-73.567255))
    , ("Vancouver", LBS.toStrict . Aeson.encode $ Forecast.LatLon 49.2827291 (-123.1207375))
    , ("Calgary", LBS.toStrict . Aeson.encode $ Forecast.LatLon 51.0486151 (-114.0708459))
    , ("Edmonton", LBS.toStrict . Aeson.encode $ Forecast.LatLon 53.544389 (-113.4909267))
    , ("Ottawa", LBS.toStrict . Aeson.encode $ Forecast.LatLon 45.421106 (-75.690308))
    , ("Quebec City", LBS.toStrict . Aeson.encode $ Forecast.LatLon 46.8138783 (-71.2079803))
    , ("Winnipeg", LBS.toStrict . Aeson.encode $ Forecast.LatLon 49.895136 (-97.138374))
    , ("Halifax", LBS.toStrict . Aeson.encode $ Forecast.LatLon 44.6487635 (-63.5752387))
    , ("St. John's", LBS.toStrict . Aeson.encode $ Forecast.LatLon 47.5615234 (-52.7126312))
    , ("San Francisco", LBS.toStrict . Aeson.encode $ Forecast.LatLon 37.7749295 (-122.4194155))
    , ("Los Angeles", LBS.toStrict . Aeson.encode $ Forecast.LatLon 34.0522342 (-118.2436849))
    , ("San Diego", LBS.toStrict . Aeson.encode $ Forecast.LatLon 32.715738 (-117.1610838))
    , ("San Jose", LBS.toStrict . Aeson.encode $ Forecast.LatLon 37.3382082 (-121.8863286))
    , ("San Francisco", LBS.toStrict . Aeson.encode $ Forecast.LatLon 37.7749295 (-122.4194155))
    , ("Fresno", LBS.toStrict . Aeson.encode $ Forecast.LatLon 36.7468422 (-119.7725868))
    , ("Sacramento", LBS.toStrict . Aeson.encode $ Forecast.LatLon 38.5815719 (-121.4943996))
    , ("Long Beach", LBS.toStrict . Aeson.encode $ Forecast.LatLon 33.7700504 (-118.1937395))
    , ("Oakland", LBS.toStrict . Aeson.encode $ Forecast.LatLon 37.8043637 (-122.2711137))
    , ("Bakersfield", LBS.toStrict . Aeson.encode $ Forecast.LatLon 35.3732921 (-119.0187125))
    , ("Anaheim", LBS.toStrict . Aeson.encode $ Forecast.LatLon 33.8352932 (-117.9145036))
    ]