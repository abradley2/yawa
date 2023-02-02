module Handler.Forecast (perform, Effect (..), getForecast, LatLon (..)) where

import Control.Monad.Free (Free (..), liftF)
import Data.Aeson (FromJSON, ToJSON, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Database.Redis qualified as Redis
import Handler (Env (..), HandlerM)
import Network.HTTP.Simple qualified as HTTP
import Network.HTTP.Types.Status (Status)
import Network.HTTP.Types.Status qualified as Status
import Relude
import Web.Scotty (ActionM)
import Web.Scotty qualified as Scotty

data LatLon = LatLon {lat :: Float, lon :: Float}

latLonKey :: LatLon -> ByteString
latLonKey (LatLon lat lon) =
    mconcat ["lat=", show lat, "&lon=", show lon]

instance ToJSON LatLon where
    toJSON (LatLon lat lon) = Aeson.object ["lat" .= lat, "lon" .= lon]

instance FromJSON LatLon where
    parseJSON = Aeson.withObject "LatLon" $ \o -> do
        LatLon
            <$> (o .: "lat")
            <*> (o .: "lon")

data Effect next
    = SendResponse Status LBS.ByteString next
    | CheckCache Env LatLon (Maybe LBS.ByteString -> next)
    | QueryForecast Env LatLon (LBS.ByteString -> next)
    | WriteCache Env LatLon LBS.ByteString next
    deriving (Functor)

perform :: (Free Effect) action -> ActionM action
perform (Pure a) = pure a
perform (Free (SendResponse status response next)) = do
    Scotty.setHeader "Content-Type" "application/json"
    Scotty.status status
    Scotty.raw response
    perform next
perform (Free (CheckCache env latLon next)) = do
    let redisConn = env.redisConn
    result <-
        lift . Redis.runRedis redisConn $
            Redis.get (latLonKey latLon)
    case result of
        Right (Just forecast) -> perform (next $ Just $ LBS.fromStrict forecast)
        _ -> perform (next Nothing)
perform (Free (QueryForecast env latLon next)) = do
    req <-
        HTTP.parseRequest
            ( mconcat
                [ "GET https://api.openweathermap.org/data/3.0/onecall"
                , "?lat="
                , show latLon.lat
                , "&lon="
                , show latLon.lon
                , "&appid="
                , "316ee27609c090ca70313f1d9eae61cd" -- Text.unpack env.apiKey
                ]
            )
    res <- HTTP.httpLBS req
    perform (next $ HTTP.getResponseBody res)
perform (Free (WriteCache env latLon forecast next)) = do
    let redisConn = env.redisConn
    _ <-
        lift . Redis.runRedis redisConn $
            Redis.set (latLonKey latLon) (LBS.toStrict forecast)
    perform next

sendResponse :: Status -> LBS.ByteString -> Free Effect ()
sendResponse status response = liftF $ SendResponse status response ()

checkCache :: Env -> LatLon -> Free Effect (Maybe LBS.ByteString)
checkCache env latLon = liftF $ CheckCache env latLon id

queryForecast :: Env -> LatLon -> Free Effect LBS.ByteString
queryForecast env latLon = liftF $ QueryForecast env latLon id

writeCache :: Env -> LatLon -> LBS.ByteString -> Free Effect ()
writeCache env latLon forecast = liftF $ WriteCache env latLon forecast ()

getForecast :: HandlerM Effect Text ()
getForecast = do
    let latLon = LatLon{lat = 0, lon = 0}
    env <- ask
    cacheHit <- lift $ checkCache env latLon
    case cacheHit of
        Just forecast -> do
            lift $ sendResponse Status.status200 forecast
        Nothing -> do
            forecast <- lift $ queryForecast env latLon
            lift $ writeCache env latLon forecast
            lift $ sendResponse Status.status200 forecast
