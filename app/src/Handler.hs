module Handler (HandlerM, Env (..), runHandler) where

import Control.Monad.Free (Free)
import Database.Redis qualified as Redis
import Relude
import Web.Scotty (ActionM)

data Env = Env {apiKey :: Text, redisConn :: Redis.Connection}

type Perform effect error action = (Free effect) action -> ActionM ()

type HandlerM effect error action = ReaderT Env (Free effect) action

runHandler :: Env -> HandlerM effect error action -> Perform effect error action -> ActionM ()
runHandler env handler perform =
    runReaderT handler env
        & perform
