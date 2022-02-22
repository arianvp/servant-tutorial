{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Api.User where

import Data.Aeson (DotNetTime (DotNetTime), ToJSON)
import Data.Aeson.Types (FromJSON)
import GHC.Generics (Generic)
import Servant
  ( Capture,
    DeleteNoContent,
    Get,
    Handler,
    JSON,
    NamedRoutes,
    NoContent,
    Post,
    Put,
    QueryParam,
    ReqBody,
    Server,
    type (:>),
  )
import Servant.API.Generic (ToServant, ToServantApi, toServant, type (:-))
import Servant.Server.Generic (AsServerT)

data Routes r = Routes
  { -- | Create user
    create :: r :- ReqBody '[JSON] CreateUser :> Post '[JSON] User,
    -- | Update user
    update :: r :- Capture "userid" UserId :> ReqBody '[JSON] UpdateUser :> Put '[JSON] User,
    -- | Get user by user id
    get :: r :- Capture "userid" UserId :> Get '[JSON] User,
    -- | Delete user by id
    delete :: r :- Capture "userid" UserId :> DeleteNoContent,
    -- | Finds user by email. Multiple users can have the same email
    findByEmail :: r :- QueryParam "email" String :> Get '[JSON] [User]
  }
  deriving stock (Generic)

type UserId = String

data User = User
  { userId :: UserId,
    userName :: String,
    userEmail :: String
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

data CreateUser = CreateUser
  { createUserName :: String,
    createUserEmail :: String
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

data UpdateUser = UpdateUser
  { updateUserEmail :: Maybe String,
    updateUserName :: Maybe String
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

server :: Routes (AsServerT Handler)
server =
  Routes
    { create = createUser,
      update = updateUser,
      get = getUser,
      findByEmail = findUserByEmail,
      delete = deleteUser
    }

findUserByEmail :: Maybe [Char] -> Handler [User]
findUserByEmail = error "not implemented"

deleteUser :: [Char] -> Handler NoContent
deleteUser = error "not implemented"

getUser :: [Char] -> Handler User
getUser = error "not implemented"

updateUser :: [Char] -> UpdateUser -> Handler User
updateUser = error "not implemented"

createUser :: CreateUser -> Handler User
createUser CreateUser {..} = do
  let userEmail = createUserEmail
  let userName = createUserName
  pure $ User {userId = "0", ..}
