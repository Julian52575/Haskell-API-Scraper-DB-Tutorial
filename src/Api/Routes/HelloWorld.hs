{-- Copyright julian.bottiglione@epitech.eu
-- hello-world route
--}

{-# LANGUAGE NamedFieldPuns #-}
-- ^ For HelloWorldResponse{message}
{-# LANGUAGE RecordWildCards #-}
-- ^ For HelloWorldResponse{..}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
-- ^ For type HelloWorld = "hello-world" :>
{-# LANGUAGE OverloadedStrings #-}
-- ^ For o .: String

module Api.Routes.HelloWorld where

import Data.Aeson
import Data.UnixTime (UnixTime (utSeconds), getUnixTime)
import Servant ((:>), Get, JSON)
import Control.Monad.IO.Class (MonadIO, liftIO)

type HelloWorld = "hello-world" :> Get '[JSON] HelloWorldResponse 

data HelloWorldResponse = HelloWorldResponse {
    message :: String,
    time :: UnixTime
} deriving (Show)

instance ToJSON HelloWorldResponse where
    toJSON HelloWorldResponse{..} = object
        [
            "message" .= message,
            "time" .= utSeconds time
        ]

helloWorldFunction :: (MonadIO m) => m HelloWorldResponse
helloWorldFunction = do
    let message = "Hello World !"
    time <- liftIO getUnixTime
    pure HelloWorldResponse{..}