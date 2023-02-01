{-# LANGUAGE NoImplicitPrelude #-}

module Handler (HandlerM, Env (..), runHandler) where

import Control.Monad.Free (Free)
import Relude
import Web.Scotty (ActionM)

data Env = Env {}

type Perform effect error action = (Free effect) action -> ActionM ()

type HandlerM effect error action = ReaderT Env (Free effect) action

runHandler :: Env -> HandlerM effect error action -> Perform effect error action -> ActionM ()
runHandler env handler perform =
  runReaderT handler env
    & perform
