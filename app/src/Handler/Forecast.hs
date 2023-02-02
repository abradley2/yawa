module Handler.Forecast (perform, Effect (..), getForecast) where

import Control.Monad.Free (Free (..), liftF)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import Database.Redis qualified as Redis
import Handler (Env (..), HandlerM)
import Network.HTTP.Simple qualified as HTTP
import Network.HTTP.Types.Status (Status)
import Network.HTTP.Types.Status qualified as Status
import Relude
import Web.Scotty (ActionM)
import Web.Scotty qualified as Scotty

data Effect next
    = SendResponse Status LBS.ByteString next
    | CheckCache Env Float Float (Maybe LBS.ByteString -> next)
    | QueryForecast Env Float Float (LBS.ByteString -> next)
    | WriteCache Env Float Float LBS.ByteString next
    deriving (Functor)

perform :: (Free Effect) action -> ActionM action
perform (Pure a) = pure a
perform (Free (SendResponse status response next)) = do
    Scotty.setHeader "Content-Type" "application/json"
    Scotty.status status
    Scotty.raw response
    perform next
perform (Free (CheckCache env lat lon next)) = do
    let redisConn = env.redisConn
    result <-
        lift . Redis.runRedis redisConn $
            Redis.get (mconcat ["lat=", show lat, "&lon=", show lon])
    case result of
        Right (Just forecast) -> perform (next $ Just $ LBS.fromStrict forecast)
        _ -> perform (next Nothing)
perform (Free (QueryForecast env lat lon next)) = do
    req <-
        HTTP.parseRequest
            ( mconcat
                [ "GET https://api.openweathermap.org/data/3.0/onecall"
                , "?lat="
                , show lat
                , "&lon="
                , show lon
                , "&appId"
                , Text.unpack env.apiKey
                ]
            )
    res <- HTTP.httpLBS req
    perform (next $ HTTP.getResponseBody res)
perform (Free (WriteCache env lat lon forecast next)) = do
    let redisConn = env.redisConn
    _ <-
        lift . Redis.runRedis redisConn $
            Redis.set (mconcat ["lat=", show lat, "&lon=", show lon]) (LBS.toStrict forecast)
    perform next

sendResponse :: Status -> LBS.ByteString -> Free Effect ()
sendResponse status response = liftF $ SendResponse status response ()

checkCache :: Env -> Float -> Float -> Free Effect (Maybe LBS.ByteString)
checkCache env lat lon = liftF $ CheckCache env lat lon id

queryForecast :: Env -> Float -> Float -> Free Effect LBS.ByteString
queryForecast env lat lon = liftF $ QueryForecast env lat lon id

writeCache :: Env -> Float -> Float -> LBS.ByteString -> Free Effect ()
writeCache env lat lon forecast = liftF $ WriteCache env lat lon forecast ()

getForecast :: HandlerM Effect Text ()
getForecast = do
    env <- ask
    cacheHit <- lift $ checkCache env 0 0
    case cacheHit of
        Just forecast -> do
            lift $ sendResponse Status.status200 forecast
        Nothing -> do
            forecast <- lift $ queryForecast env 0 0
            lift $ writeCache env 0 0 forecast
            lift $ sendResponse Status.status200 forecast
