{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Lib where

import Handler (Env (..))
import qualified Handler 
import Handler.Forecast
import Relude
import Web.Scotty (ScottyM)
import qualified Web.Scotty as Scotty

handler :: Env -> ScottyM ()
handler env = do
  Scotty.get "/forecast" $
    Handler.runHandler
      env
      Handler.Forecast.getForecast
      Handler.Forecast.perform
  pure ()

someFunc :: IO ()
someFunc = Scotty.scotty 9966 $ handler (Env {})
