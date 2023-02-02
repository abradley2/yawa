module Handler.Locations (getLocations, perform) where

import Control.Monad.Free (Free (..), liftF)
import Data.ByteString.Lazy qualified as LBS
import Database.Redis qualified as Redis
import Handler (Env (..), HandlerM)
import Network.HTTP.Types.Status (Status)
import Network.HTTP.Types.Status qualified as Status
import Relude
import Web.Scotty (ActionM)
import Web.Scotty qualified as Scotty

data Effect next
    = QueryLocations Env (Maybe LBS.ByteString -> next)
    | SendResponse Status LBS.ByteString next
    deriving (Functor)

perform :: (Free Effect) action -> ActionM action
perform (Pure a) = pure a
perform (Free (SendResponse status response next)) = do
    Scotty.setHeader "Content-Type" "application/json"
    Scotty.status status
    Scotty.raw response
    perform next
perform (Free (QueryLocations env next)) = do
    let redisConn = env.redisConn
    results <-
        lift . Redis.runRedis redisConn $
            Redis.get "cities"

    case results of
        Right (Just cities) -> perform $ next (Just $ LBS.fromStrict cities)
        _ -> perform $ next Nothing

queryLocations :: Env -> Free Effect (Maybe LBS.ByteString)
queryLocations env = liftF $ QueryLocations env id

sendResponse :: Status -> LBS.ByteString -> Free Effect ()
sendResponse status response = liftF $ SendResponse status response ()

getLocations :: HandlerM Effect Text ()
getLocations = do
    env <- ask
    locations <- lift $ queryLocations env
    lift $
        maybe
            (sendResponse Status.status404 mempty)
            (sendResponse Status.status200)
            locations
