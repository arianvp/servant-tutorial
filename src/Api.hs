{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import qualified Api.Movie as Movie
import qualified Api.User as User
import GHC.Generics (Generic)
import Servant
  ( Application,
    Get,
    Handler,
    PlainText,
    Proxy (..),
    serve,
    type (:>), NamedRoutes
  )
import Servant.API.Generic (GenericMode (type (:-)), ToServantApi, fromServant, genericApi, toServant)
import Servant.Client (AsClientT, ClientM, client)
import Servant.Client.Generic (AsClientT, genericClient)
import Servant.Server.Generic (AsServerT)

data Routes mode = Routes
  { getMetrics :: mode :- "metrics" :> Get '[PlainText] String,
    getReadiness :: mode :- "readiness" :> Get '[PlainText] String,
    getLiveness :: mode :- "liveness" :> Get '[PlainText] String,
    users :: mode :- "users" :> NamedRoutes User.Routes,
    movies :: mode :- "movies" :> NamedRoutes Movie.Routes 
  }
  deriving stock (Generic)

type Api = NamedRoutes Routes

api :: Proxy Api
api = Proxy :: Proxy Api

livenessCheck :: Handler String
livenessCheck = pure "I am alive"

readinessCheck :: Handler String
readinessCheck = pure "I am ready"

prometheusMetrics :: Handler String
prometheusMetrics = pure "awesomeness_count 1"

-- a server is a record of handlers
server :: Routes (AsServerT Handler)
server =
  Routes
    { getMetrics = prometheusMetrics,
      getReadiness = readinessCheck,
      getLiveness = livenessCheck,
        users = User.server,
        movies = Movie.server 
    }

app :: Application
app = serve api server
