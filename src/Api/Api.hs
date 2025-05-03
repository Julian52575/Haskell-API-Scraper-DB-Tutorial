{-- Copyright julian.bottiglione@epitech.eu
-- API Type
--}

{-# LANGUAGE TypeOperators #-}
-- ^ Needed for :<|>
{-# LANGUAGE DataKinds #-}
-- ^ Needed for JSON
{-# LANGUAGE TypeFamilies #-}
-- ^ For AuthProtect "cookie-auth"
module Api.Api where

import Servant ((:<|>), JSON, (:>), AuthProtect)
import Servant.Auth (Auth)

import Api.JWTPayload
import qualified Api.Routes.HelloWorld as Routes (HelloWorld)
import qualified Api.Routes.Login as Routes (Login)
import qualified Api.Routes.Secret as Routes (Secret)
import qualified Api.Routes.Secret2 as Routes (Secret2)
import Servant.Server.Experimental.Auth (AuthServerData)

type PublicRoutes = 
        Routes.HelloWorld
    :<|> Routes.Login
type PublicApi = PublicRoutes

type instance AuthServerData (AuthProtect "cookie-auth") = JWTPayload
type ProtectedRoutes = Routes.Secret :<|> Routes.Secret2
type ProtectedApi = AuthProtect "cookie-auth" :> ProtectedRoutes

type Api = ProtectedApi :<|> PublicApi