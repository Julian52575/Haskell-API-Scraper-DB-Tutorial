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

import Servant ((:>), JSON, ReqBody, Post, throwError, err500, Handler)
import Servant.Auth.Server (makeJWT, JWTSettings, AuthResult)

import Api.JWTPayload

type Login = "login" :> ReqBody '[JSON] LoginBody :> Post '[JSON] TokenResponse

data TokenResponse = TokenResponse {
    token :: Text
} deriving (Generic)
instance Aeson.ToJSON TokenResponse

data LoginBody = LoginBody {
    loginName :: Text,
    loginPassword :: Text
} deriving (Generic)
instance Aeson.FromJSON LoginBody

loginFunction :: JWTSettings -> LoginBody -> Handler TokenResponse
loginFunction jwtCfg (LoginBody name _password) = do
  let user = JWTPayload { userName = name }
  mToken <- liftIO $ makeJWT user jwtCfg Nothing
  case mToken of
    Left _ -> throwError err500
    Right tokenBS -> return $ TokenResponse (decodeUtf8 $ toStrict tokenBS)
