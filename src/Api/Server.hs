{-- Copyright julian.bottiglione@epitech.eu
-- Server
--}

{-# LANGUAGE DataKinds #-}
-- ^ For the api auth format

module Api.Server where

import Data.Proxy 
import Servant (Server, serve, (:<|>)(..), Context((:.)), Context(EmptyContext), err401, serveWithContext)
import Servant.Auth.Server (JWT, JWTSettings (JWTSettings), generateKey, defaultJWTSettings, defaultCookieSettings, AuthResult (Authenticated), ThrowAll (throwAll), Cookie)
import Network.Wai (Application)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Api.Api (Api, ProtectedApi, ProtectedRoutes, PublicRoutes, PublicApi)
import Api.Routes.HelloWorld as Routes (helloWorldFunction)
import Api.Routes.Login as Routes (loginFunction)
import Api.Routes.Secret as Routes (secretFunction)
import Api.Routes.Secret2 as Routes (secretFunction2)
import Api.JWTPayload (JWTPayload)

protectedServer :: AuthResult JWTPayload -> Server ProtectedRoutes
protectedServer (Authenticated payload) = Routes.secretFunction payload :<|> Routes.secretFunction2
protectedServer _ = throwAll err401

server :: JWTSettings -> Server PublicRoutes
server jwt = Routes.helloWorldFunction
    :<|> Routes.loginFunction jwt

appM :: (MonadIO m) => m Application 
appM = do
    -- Generate JWT settings
    myKey <- liftIO generateKey -- random key at runtime
    let jwtCfg = defaultJWTSettings myKey
        cookieCfg = defaultCookieSettings -- unused but required by some auth functions
        context = cookieCfg :. jwtCfg :. EmptyContext
    pure $ serveWithContext (Proxy :: Proxy (Api '[JWT, Cookie])) context (protectedServer :<|> server jwtCfg)