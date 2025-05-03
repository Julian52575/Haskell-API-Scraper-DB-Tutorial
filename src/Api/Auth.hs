{-- Copyright Julian Bottiglione
-- Authentication handling
--}

module Api.Auth where

import Data.Text

import Servant.Auth.Server (JWTSettings)
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import Network.Wai (Request)

import Api.JWTPayload (JWTPayload(..))

cookieAuthHandler :: JWTSettings -> AuthHandler Request JWTPayload
cookieAuthHandler _ = mkAuthHandler $ \req -> pure JWTPayload {userName = pack "lol"}

