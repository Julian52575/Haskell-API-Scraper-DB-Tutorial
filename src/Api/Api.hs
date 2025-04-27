{-- Copyright julian.bottiglione@epitech.eu
-- API Type
--}

{-# LANGUAGE TypeOperators #-}
-- ^ Needed for :<|>
{-# LANGUAGE DataKinds #-}
-- ^ Needed for JSON

module Api.Api where

import Servant ((:<|>), JSON, (:>))
import Servant.Auth (Auth)

import Api.JWTPayload
import qualified Api.Routes.HelloWorld as Routes (HelloWorld)
import qualified Api.Routes.Login as Routes (Login)
import qualified Api.Routes.Secret as Routes (Secret)
import qualified Api.Routes.Secret2 as Routes (Secret2)

type PublicRoutes = 
        Routes.HelloWorld
    :<|> Routes.Login
type PublicApi = PublicRoutes

type ProtectedRoutes = Routes.Secret :<|> Routes.Secret2
type ProtectedApi authFormat = Auth authFormat JWTPayload :> ProtectedRoutes

type Api authFormat = ProtectedApi authFormat :<|> PublicApi