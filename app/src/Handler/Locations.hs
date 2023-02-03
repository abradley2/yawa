module Handler.Locations (getLocations, runEffects) where

import Control.Monad.Free (Free (..), liftF)
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.Lazy qualified
import Database.Redis qualified as Redis
import Handler (ActionHandler, Env (..), Handler (..), liftEffect, liftEither)
import Network.HTTP.Types.Status (Status)
import Network.HTTP.Types.Status qualified as Status
import Relude
import Web.Scotty.Internal.Types (ActionT)
import Web.Scotty.Trans qualified as ScottyT

type LazyText = Data.Text.Lazy.Text

data Effect next
    = QueryLocations (Either Error LazyByteString -> next)
    | SendResponse Status LazyByteString next
    deriving (Functor)

data Error = Error Status Text

runEffects :: (Free Effect) action -> ActionT LazyText (ReaderT Env IO) action
runEffects (Pure done) =
    pure done
runEffects (Free (SendResponse status response next)) = do
    ScottyT.setHeader "Content-Type" "application/json"
    ScottyT.status status
    ScottyT.raw response
    runEffects next
runEffects (Free (QueryLocations next)) = do
    env <- ask
    results <- liftIO $ Redis.runRedis env.redisConn $ Redis.get "cities"

    case results of
        Right (Just cities) -> runEffects $ next (Right $ ByteString.Lazy.fromStrict cities)
        Right Nothing -> runEffects $ next (Left $ Error Status.status404 "No cities found")
        Left redisErr -> runEffects $ next (Left $ Error Status.status500 (show redisErr))

onError :: Error -> (Free Effect) ()
onError (Error status err) =
    liftF $
        SendResponse status (ByteString.Lazy.fromStrict $ Text.Encoding.encodeUtf8 err) ()

handleGetLocations :: ExceptT Error (Free Effect) ()
handleGetLocations = do
    locationsRaw <- liftEffect (QueryLocations id) >>= liftEither

    liftEffect $ SendResponse Status.status200 locationsRaw ()

getLocations :: ActionHandler Effect Error ()
getLocations =
    Handler
        { handle = handleGetLocations
        , onError = onError
        , runEffects = runEffects
        }
