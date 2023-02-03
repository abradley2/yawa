module Spec where

import Relude

import Control.Exception (assert)
import Control.Monad.Free (Free (..))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.HashMap.Lazy qualified as HashMap
import Handler (Handler (..))
import Handler qualified
import Handler.Forecast (Effect, LatLon (..))
import Handler.Forecast qualified as Forecast
import Network.HTTP.Types.Status (Status)
import Network.HTTP.Types.Status qualified as Status

data TestState = TestState
    { redisCache :: HashMap LazyByteString LazyByteString
    , httpResponse :: Maybe (Status, LazyByteString)
    }

testState :: TestState
testState =
    TestState
        { redisCache = HashMap.empty
        , httpResponse = Nothing
        }

runHandler :: (Free Effect () -> Maybe (State TestState ())) -> StateT TestState Identity ()
runHandler override =
    Handler.runHandler
        ( Handler
            { runEffects = \effect -> fromMaybe (runEffects effect) (override effect)
            , onError = Forecast.onError
            , handle = Forecast.handleGetForecast
            }
        )

runEffects :: (Free Effect) action -> State TestState action
runEffects effect =
    case effect of
        Pure done ->
            pure done
        Free (Forecast.SendResponse status body next) -> do
            modify
                ( \s ->
                    s{httpResponse = Just (status, body)}
                )
            runEffects next
        Free (Forecast.GetCityParam next) ->
            runEffects . next $ "Bethesda"
        Free (Forecast.CheckCache latLon next) -> do
            redisCache <- (\s -> s.redisCache) <$> get
            runEffects . next . Right $ HashMap.lookup (Aeson.encode latLon) redisCache
        Free (Forecast.QueryForecast _ _) ->
            error "QueryForecast not implemented"
        Free (Forecast.QueryLatLon _ next) ->
            runEffects . next $ Right $ ByteString.Lazy.toStrict $ Aeson.encode $ LatLon 1 1
        Free (Forecast.WriteCache latLon body next) -> do
            modify
                ( \s ->
                    s{redisCache = HashMap.insert (Aeson.encode latLon) body s.redisCache}
                )
            runEffects next

-- verify that we return the api response to the frontend just as we receive it from the API
returnsApiResponse :: ()
returnsApiResponse =
    runHandler overrides
        & (`execState` testState)
        & (\s -> assert (s.httpResponse == Just (Status.status200, apiResponse)) ())
  where
    apiResponse = "It's going to be cloudy"

    overrides effect =
        runEffects <$> case effect of
            Free (Forecast.QueryForecast _ next) ->
                Just . next $ apiResponse
            _ ->
                Nothing

-- verify that we return any cache hits to the frontend and DO NOT query the API in those situations
returnsCacheHit :: ()
returnsCacheHit =
    runHandler overrides
        & (`execState` testState)
        & (\s -> assert (s.httpResponse == Just (Status.status200, cachedResponse)) ())
  where
    cachedResponse = "It's going to be sunny"

    overrides effect =
        runEffects <$> case effect of
            Free (Forecast.CheckCache _ next) ->
                Just . next . Right $ Just cachedResponse
            Free (Forecast.QueryForecast _ _) ->
                error "QueryForecast should not be called"
            _ ->
                Nothing

-- verify that whatever the api gives us is written to cache
writesToCache :: ()
writesToCache =
    runHandler overrides
        & (`execState` testState)
        & ( \s ->
                assert (HashMap.lookup latLonResponse s.redisCache == Just apiResponse) ()
          )
  where
    apiResponse = "It's going to be cloudy"

    latLonResponse = Aeson.encode $ LatLon 1 1

    overrides effect =
        runEffects <$> case effect of
            Free (Forecast.QueryForecast _ next) ->
                Just . next $ apiResponse
            Free (Forecast.QueryLatLon _ next) ->
                Just . next $ Right $ ByteString.Lazy.toStrict latLonResponse
            _ ->
                Nothing