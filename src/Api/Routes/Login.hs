{-- Copyright julian.bottiglione@epitech.eu
-- login route
--}

{-# LANGUAGE DeriveGeneric #-}
-- ^ For TokenResponse.deriving Generic
{-# LANGUAGE NamedFieldPuns #-}
-- ^ For HelloWorldResponse{message}
{-# LANGUAGE RecordWildCards #-}
-- ^ For HelloWorldResponse{..}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
-- ^ For type HelloWorld = "hello-world" :>
{-# LANGUAGE OverloadedStrings #-}
-- ^ For o .: String

module Api.Routes.Login where

import Data.Text (Text)
import Data.ByteString (toStrict)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Aeson as Aeson
import GHC.Generics
import Control.Monad.IO.Class (MonadIO, liftIO)

import Servant ((:>), JSON, ReqBody, Post, throwError, err500, Handler, Header, Headers, err401, NoContent(..))
import Servant.Auth.Server (makeJWT, JWTSettings, AuthResult, SetCookie, acceptLogin, defaultCookieSettings)

import Api.JWTPayload

type Login = "login"
            :> ReqBody '[JSON] LoginBody
            :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)

data TokenResponse = TokenResponse {
    token :: Text
} deriving (Generic)
instance Aeson.ToJSON TokenResponse

data LoginBody = LoginBody {
    loginName :: Text,
    loginPassword :: Text
} deriving (Generic)
instance Aeson.FromJSON LoginBody

loginFunction :: JWTSettings -> LoginBody -> Handler (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
loginFunction jwtCfg (LoginBody name _password) = do
  let payload = JWTPayload { userName = name }
  loginAcceptMaybe <- liftIO $ acceptLogin defaultCookieSettings jwtCfg payload
  case loginAcceptMaybe of
    Nothing -> throwError err401
    Just responseToCookieFun -> do
      let response = NoContent
      return $ responseToCookieFun response