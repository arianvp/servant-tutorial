{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.Movie where

import Data.Aeson.Types (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Servant
  ( Capture,
    FromHttpApiData,
    Get,
    Handler,
    JSON,
    NamedRoutes,
    Proxy (Proxy),
    QueryParam,
    Server,
    ToHttpApiData (toUrlPiece),
    parseUrlPiece,
    type (:>),
  )
import Servant.API.Generic (GenericMode (type (:-)), ToServantApi, fromServant, genericApi)
import Servant.Client (AsClientT, client)
import Servant.Client.Internal.HttpClient (ClientM)
import Servant.Server.Generic (AsServerT)

data Routes r = Routes
  { list :: r :- QueryParam "genre" Genre :> Get '[JSON] [Movie],
    get :: r :- Capture "id" MovieId :> Get '[JSON] Movie
  }
  deriving stock (Generic)

data Genre = Horror | Thriller
  deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

type MovieId = String

data Movie = Movie
  {movieTitle :: String, movieGenre :: Genre}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

instance ToHttpApiData Genre where
  toUrlPiece Horror = "horror"
  toUrlPiece Thriller = "thriller"

instance FromHttpApiData Genre where
  parseUrlPiece "horror" = Right Horror
  parseUrlPiece "thriller" = Right Thriller
  parseUrlPiece _ = Left "unknown"

server :: Routes (AsServerT Handler)
server =
  Routes
    { list = listMovies,
      get = getMovie
    }

getMovie :: MovieId -> Handler Movie
getMovie x = pure $ Movie {movieGenre = Thriller, movieTitle = "A new hope"}

listMovies :: Maybe Genre -> Handler [Movie]
listMovies x = do
  let movies =
        [ Movie {movieGenre = Thriller, movieTitle = "A new Hope"},
          Movie {movieGenre = Horror, movieTitle = "Lol scary"}
        ]
  case x of
    Nothing -> pure movies
    Just ge -> pure $ filter (\x -> movieGenre x == ge) movies
