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
import Handler.Forecast (LatLon (..))
import Handler.Forecast qualified as Forecast
import Network.HTTP.Types.Status (Status)
import Network.HTTP.Types.Status qualified as Status

data TestState = TestState
    { redisCache :: HashMap LazyByteString LazyByteString
    , httpResponse :: Maybe (Status, LazyByteString)
    }

testState :: TestState
testState = TestState{redisCache = HashMap.empty, httpResponse = Nothing}

runEffectsWith ::
    ((Free Forecast.Effect) action -> Maybe (State TestState action)) ->
    (Free Forecast.Effect) action ->
    State TestState action
runEffectsWith override effect = fromMaybe (runEffects effect) (override effect)

runEffects :: (Free Forecast.Effect) action -> State TestState action
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
        Free (Forecast.QueryForecast _ next) ->
            runEffects . next $ ""
        Free (Forecast.QueryLatLon _ next) ->
            runEffects . next $ Right $ ByteString.Lazy.toStrict $ Aeson.encode $ LatLon 1 1
        Free (Forecast.WriteCache latLon body next) -> do
            modify
                ( \s ->
                    s{redisCache = HashMap.insert (Aeson.encode latLon) body s.redisCache}
                )
            runEffects next

returnsApiResponse :: ()
returnsApiResponse =
    Handler.runHandler
        ( Handler
            { runEffects = runEffectsWith overrides
            , onError = Forecast.onError
            , handle = Forecast.handleGetForecast
            }
        )
        & (`execState` testState)
        & (\s -> assert (s.httpResponse == Just (Status.status200, apiResponse)) ())
  where
    apiResponse = "It's going to be cloudy"

    overrides effect = case effect of
        Free (Forecast.QueryForecast _ next) ->
            Just $ do
                modify
                    ( \s ->
                        s{httpResponse = Just (Status.status200, apiResponse)}
                    )
                runEffects . next $ apiResponse
        _ ->
            Nothing