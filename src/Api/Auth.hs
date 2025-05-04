{-- Copyright Julian Bottiglione
-- Authentication handling
--}

{-# LANGUAGE FlexibleContexts #-}

module Api.Auth where

import Data.Maybe (maybe, fromJust)
import Data.Text as T
import Data.ByteString.Char8 as BS
import Web.Cookie (parseCookies)
import Network.HTTP.Types.Header (Header, hCookie)
import GHC.List (lookup)
import Control.Monad.IO.Class (MonadIO (liftIO))

import Servant (Handler, throwError, err400, err401)
import Servant.Auth.Server (JWTSettings, verifyJWT)
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import Network.Wai (Request, requestHeaders)

import Api.JWTPayload (JWTPayload(..), jwtPayloadIsValid)

import Control.Monad.Trans.Class (lift)
import Control.Monad.RWS (MonadReader (ask))
import Control.Monad.Reader (ReaderT (runReaderT))


requestToJwt :: e -> JWTSettings -> Request -> Handler JWTPayload
requestToJwt env sett req = do
    case lookup hCookie (requestHeaders req) of
        Nothing -> throwError err401 -- No Cookies
        Just cookiesRaw -> case lookup (BS.pack "JWT-Cookie") (parseCookies cookiesRaw) of
            Nothing -> throwError err401 -- No JWT-Cookie
            Just jwtRaw -> do
                result <- liftIO $ verifyJWT sett jwtRaw
                case result of
                    Nothing -> throwError err401 --Invalid JWT
                    Just payload ->
                        if jwtPayloadIsValid payload
                        then pure payload
                        else throwError err401

cookieAuthHandler :: e -> JWTSettings -> AuthHandler Request JWTPayload
cookieAuthHandler env sett = mkAuthHandler $ requestToJwt env sett