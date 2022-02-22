module Main where

import qualified Api
import qualified Api.Movie as Movie
import qualified Api.User as User
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Function ((&))
import GHC.IO.Device (SeekMode (AbsoluteSeek))
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.API.Generic (ToServantApi, fromServant, genericApi, toServant)
import Servant.Client
  ( AsClientT,
    BaseUrl (BaseUrl),
    ClientM,
    Scheme (Http),
    client,
    mkClientEnv,
    runClientM,
    (//),
    (/:),
  )

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings
  let clientEnv = mkClientEnv manager (BaseUrl Http "localhost" 8080 "")
  res <- runClientM someCallsNotNested clientEnv
  case res of
    Left ce -> print ce
    Right any -> pure any
  res <- runClientM someCallsNested clientEnv
  case res of
    Left ce -> print ce
    Right any -> pure any

-- Easy
someCallsNotNested :: ClientM ()
someCallsNotNested = do
  let apiClient :: Api.Routes (AsClientT ClientM)
      apiClient = client Api.api
  readiness <- Api.getReadiness apiClient
  liftIO . print $ readiness
  liveness <- Api.getLiveness apiClient
  liftIO . print $ liveness
  metrics <- Api.getMetrics apiClient
  liftIO . print $ metrics

-- Hard
someCallsNested :: ClientM ()
someCallsNested = do
  -- Quite a bit of boiler plate. Need to create a client object for each
  -- sub-api.  Cumbersome. That's why it's easier to just have one top-level
  -- routes like we have in Feeld codebase
  let apiClient :: Api.Routes (AsClientT ClientM)
      apiClient = client Api.api

  movies <- Movie.list (Api.movies apiClient) Nothing
  liftIO . print $ movies

  --  GET /movies/:genre
  movies <- apiClient // Api.movies // Movie.list /: Just Movie.Thriller
  user <-
    User.create (Api.users apiClient)
      User.CreateUser {User.createUserName = "Joel", User.createUserEmail = "blah@blah.com"}
  liftIO . print $ movies

  user <-
    User.create
      (Api.users apiClient)
      User.CreateUser
        { User.createUserName = "arian",
          User.createUserEmail = "arian@feeld.co"
        }
  liftIO . print $ user