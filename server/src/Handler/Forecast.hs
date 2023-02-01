{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Forecast where

import Relude 
import Web.Scotty (ActionM)
import Control.Monad.Free (Free(..), liftF)
import Handler (HandlerM)
import Data.Aeson (ToJSON)
import Web.Scotty qualified as Scotty 

data Effect next
  = SendGetForecastResponse GetForecastResponse next
  | CheckCache Float Float (Maybe () -> next)
  | QueryForecast Float Float (() -> next)
  | WriteCache Float Float () next
  | Log String next
  deriving (Functor)

perform :: (Free Effect) action -> ActionM action
perform (Pure a) = pure a
perform (Free (SendGetForecastResponse response next)) = do
  Scotty.json $ GetForecastResponse "Hello, from the handler!"
  perform next
perform (Free (CheckCache lat lon next)) = do
  putStrLn "Checking cache"
  perform (next Nothing)
perform (Free (QueryForecast lat lon next)) = do
  putStrLn "Querying weather"
  perform (next ())
perform (Free (WriteCache lat lon forecast next)) = do
  putStrLn "Writing cache"
  perform next
perform (Free (Log msg next)) = do
  putStrLn msg
  perform next

sendGetForecastResponse :: GetForecastResponse -> Free Effect ()
sendGetForecastResponse response = liftF $ SendGetForecastResponse response ()

checkCache :: Float -> Float -> Free Effect (Maybe ())
checkCache lat lon = liftF $ CheckCache lat lon id

queryForecast :: Float -> Float -> Free Effect ()
queryForecast lat lon = liftF $ QueryForecast lat lon id

writeCache :: Float -> Float -> () -> Free Effect ()
writeCache lat lon forecast = liftF $ WriteCache lat lon forecast ()

newtype GetForecastResponse = GetForecastResponse Text deriving (ToJSON)

getForecast :: HandlerM Effect Text ()
getForecast = do
  cacheHit <- lift $ checkCache 0 0
  case cacheHit of
    Just cacheValue -> do
      lift $ sendGetForecastResponse $ GetForecastResponse "Hello, from the handler!"
      pure ()
    Nothing -> do
      forecast <- lift $ queryForecast 0 0
      lift $ writeCache 0 0 forecast
      lift $ sendGetForecastResponse $ GetForecastResponse "Hello, from the handler!"
      pure ()