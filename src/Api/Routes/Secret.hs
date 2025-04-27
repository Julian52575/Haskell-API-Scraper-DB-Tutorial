{-- Copyright julian.bottiglione@epitech.eu
-- Secret route
--}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE NamedFieldPuns #-}
-- ^ Needed for JWTPayload{userName}

module Api.Routes.Secret where

import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString (toStrict)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Control.Monad.IO.Class (liftIO)

import Servant ((:>), ReqBody, JSON, Get, Handler, throwError, err500)
import Servant.Auth.Server (JWTSettings, makeJWT)

import Api.JWTPayload (JWTPayload(..))

type Secret = "secret" :> Get '[JSON] SecretResponse 

data SecretResponse = SecretResponse {
    secret :: Text
} deriving (Generic)
instance ToJSON SecretResponse

secretFunction :: JWTPayload -> Handler SecretResponse
secretFunction JWTPayload{userName} = pure $ SecretResponse{secret = pack $ "Congratulation " ++ unpack userName ++ " on accessing the secret route!"}