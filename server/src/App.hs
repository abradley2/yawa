{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App where

import Handler qualified
import Handler (Env(..))
import Relude
import Web.Scotty (ScottyM)
import Web.Scotty qualified as Scotty
import Handler.Forecast qualified

handler :: Env -> ScottyM ()
handler env = do
  Scotty.get "/forecast" $ Handler.runHandler env 
    Handler.Forecast.getForecast
    Handler.Forecast.perform
  pure ()

server :: IO ()
server = Scotty.scotty 9966 $ handler (Env {})
