module Handler.Forecast (Effect (..), runEffects, onError, getForecast, handleGetForecast, LatLon (..)) where

import Control.Monad.Free (Free (..), liftF)
import Data.Aeson (FromJSON, ToJSON, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.Lazy qualified
import Database.Redis qualified as Redis
import Handler (Env (..), Handler (..), liftEffect, liftEither)
import Network.HTTP.Simple qualified as HTTP
import Network.HTTP.Types.Status (Status)
import Network.HTTP.Types.Status qualified as Status
import Relude
import Web.Scotty.Trans (ActionT)
import Web.Scotty.Trans qualified as ScottyT

type LazyText = Data.Text.Lazy.Text

data LatLon = LatLon {lat :: Float, lon :: Float}

data Error = Error Status Text

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
    = SendResponse Status LazyByteString next
    | GetCityParam (Text -> next)
    | CheckCache LatLon (Either Error (Maybe LazyByteString) -> next)
    | QueryForecast LatLon (LazyByteString -> next)
    | QueryLatLon Text (Either Error ByteString -> next)
    | WriteCache LatLon LazyByteString next
    deriving (Functor)

runEffects :: (Free Effect) action -> ActionT LazyText (ReaderT Env IO) action
runEffects (Pure done) =
    pure done
runEffects (Free (GetCityParam next)) = do
    city <- ScottyT.param "city"
    runEffects . next $ city
runEffects (Free (QueryLatLon city next)) = do
    env <- ask

    result <-
        liftIO . Redis.runRedis env.redisConn $
            Redis.get (Text.Encoding.encodeUtf8 city)

    runEffects . next $ case result of
        Left redisErr ->
            Left $ Error Status.status500 (show redisErr)
        Right (Just latLonRaw) ->
            Right latLonRaw
        Right Nothing ->
            Left $ Error Status.status404 "City not found"
runEffects (Free (SendResponse status response next)) = do
    ScottyT.setHeader "Content-Type" "application/json"
    ScottyT.status status
    ScottyT.raw response
    runEffects next
runEffects (Free (CheckCache latLon next)) = do
    env <- ask
    result <-
        liftIO . Redis.runRedis env.redisConn $
            Redis.get (latLonKey latLon)
    runEffects . next $ case result of
        Right (Just forecast) -> Right . Just $ ByteString.Lazy.fromStrict forecast
        Right Nothing -> Right Nothing
        Left redisErr -> Left $ Error Status.status500 (show redisErr)
runEffects (Free (QueryForecast latLon next)) = do
    env <- ask
    req <-
        HTTP.parseRequest
            ( mconcat
                [ "GET https://api.openweathermap.org/data/3.0/onecall"
                , "?lat="
                , show latLon.lat
                , "&lon="
                , show latLon.lon
                , "&appid="
                , Text.unpack env.apiKey
                , "&units=imperial"
                ]
            )
    res <- HTTP.httpLBS req
    runEffects (next $ HTTP.getResponseBody res)
runEffects (Free (WriteCache latLon forecast next)) = do
    env <- ask
    _ <-
        liftIO . Redis.runRedis env.redisConn $
            Redis.set (latLonKey latLon) (ByteString.Lazy.toStrict forecast)
    runEffects next

onError :: Error -> (Free Effect) ()
onError (Error status message) = liftF (SendResponse status (Aeson.encode message) ())

handleGetForecast :: ExceptT Error (Free Effect) ()
handleGetForecast = do
    city <- liftEffect (GetCityParam id)

    latLonRaw <- liftEffect (QueryLatLon city id) >>= liftEither

    latLon <- liftEither $ decodeLatLon latLonRaw

    cachedForecast <- liftEffect (CheckCache latLon id) >>= liftEither

    case cachedForecast of
        Just forecast ->
            liftEffect (SendResponse Status.status200 forecast ())
        Nothing -> do
            forecast <- liftEffect (QueryForecast latLon id)
            liftEffect (WriteCache latLon forecast ())
            liftEffect (SendResponse Status.status200 forecast ())
  where
    decodeLatLon :: ByteString -> Either Error LatLon
    decodeLatLon = first (Error Status.status500 . Text.pack) . Aeson.eitherDecodeStrict

getForecast :: Handler Effect Error ()
getForecast =
    Handler
        { handle = handleGetForecast
        , onError = onError
        , runEffects = runEffects
        }