{-- Copyright julian.bottiglione@epitech.eu
-- Server
--}

module Api.Server where

import Data.Proxy 
import Servant (Server, serve, (:<|>)(..), Context((:.)), Context(EmptyContext))
import Servant.Auth.Server (JWTSettings (JWTSettings), generateKey, defaultJWTSettings, defaultCookieSettings)
import Network.Wai (Application)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Api.Api (Api)
import Api.Routes.HelloWorld as Routes (helloWorldFunction)
import Api.Routes.Login as Routes (loginFunction)

server :: (MonadIO m) => JWTSettings -> m (Server Api)
server jwt = return $ Routes.helloWorldFunction :<|> \idk -> Routes.loginFunction jwt idk

appM :: (MonadIO m) => m Application 
appM = do
    -- Generate JWT settings
    myKey <- liftIO generateKey -- random key at runtime
    let jwtCfg = defaultJWTSettings myKey
        cookieCfg = defaultCookieSettings -- unused but required by some auth functions
        cfg = cookieCfg :. jwtCfg :. EmptyContext
    Servant.serve (Proxy :: Proxy Api) <$> server jwtCfg