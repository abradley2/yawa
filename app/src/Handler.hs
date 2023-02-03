module Handler (ActionHandler, Handler (..), Env (..), runHandler, liftEffect, liftEither) where

import Control.Monad.Free (Free)
import Control.Monad.Free qualified as Free
import Control.Monad.Trans.Except qualified as Except
import Data.Text.Lazy qualified
import Database.Redis qualified as Redis
import Relude
import Web.Scotty.Trans (ActionT)

type LazyText = Data.Text.Lazy.Text

data Env = Env {apiKey :: Text, redisConn :: Redis.Connection}

type ActionHandler effect error action = Handler effect error action (ActionT LazyText (ReaderT Env IO))

data Handler effect error action m = Handler
    { handle :: ExceptT error (Free effect) action
    , onError :: error -> (Free effect) action
    , runEffects :: (Free effect) action -> m action
    }

liftEffect :: Functor effect => effect action -> ExceptT error (Free effect) action
liftEffect = ExceptT . fmap Right . Free.liftF

liftEither :: Functor effect => Either error action -> ExceptT error (Free effect) action
liftEither = Except.except

runHandler :: Functor effect => Handler effect error action m -> m action
runHandler handler =
    runExceptT handler.handle
        & (>>= either handler.onError pure)
        & handler.runEffects
