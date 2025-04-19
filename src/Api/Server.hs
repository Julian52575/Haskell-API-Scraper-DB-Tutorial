{-- Copyright julian.bottiglione@epitech.eu
-- Server
--}

module Api.Server where

import Data.Proxy 
import qualified Servant (Server, serve)
import qualified Network.Wai as Wai (Application)
import Control.Monad.IO.Class (MonadIO)

import Api.Api (Api)
import qualified Api.Routes.HelloWorld as Routes (helloWorldFunction)

server :: (MonadIO m) => m (Servant.Server Api)
server = return Routes.helloWorldFunction

appM :: (MonadIO m) => m Wai.Application 
appM = Servant.serve (Proxy :: Proxy Api) <$> server