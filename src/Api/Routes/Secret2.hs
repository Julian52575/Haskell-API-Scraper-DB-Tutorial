{-- Copyright julian.bottiglione@epitech.eu
-- Secret route
--}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE NamedFieldPuns #-}
-- ^ Needed for JWTPayload{userName}

module Api.Routes.Secret2 where

import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString (toStrict)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Control.Monad.IO.Class (liftIO)

import Servant ((:>), ReqBody, JSON, Get, Handler, throwError, err500)
import Servant.Auth.Server (JWTSettings, makeJWT)

import Api.JWTPayload (JWTPayload(..))

type Secret2 = "secret2" :> Get '[JSON] SecretResponse2

data SecretResponse2 = SecretResponse2 {
    secret :: Text
} deriving (Generic)
instance ToJSON SecretResponse2

secretFunction2 :: Handler SecretResponse2
secretFunction2 = pure $ SecretResponse2{secret = pack "Congratulation on accessing the 2nd secret route!"}