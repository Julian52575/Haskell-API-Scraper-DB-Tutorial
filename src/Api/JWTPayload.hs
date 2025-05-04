{-- Copyright Julian Bottiglione
-- Json Web Token Payload
--}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Api.JWTPayload where

import Data.Text (Text)
import GHC.Generics
import qualified Data.Aeson as Aeson (FromJSON, ToJSON)
import qualified Servant.Auth.Server as Servant.Auth
import Data.ByteString (ByteString)

data JWTPayload = JWTPayload {
    userName :: Text
} deriving (Eq, Show, Generic)

instance Aeson.FromJSON         JWTPayload
instance Aeson.ToJSON           JWTPayload
instance Servant.Auth.FromJWT   JWTPayload
instance Servant.Auth.ToJWT     JWTPayload

jwtPayloadIsValid :: JWTPayload -> Bool
jwtPayloadIsValid JWTPayload{..} = True -- We will add database interaction later
