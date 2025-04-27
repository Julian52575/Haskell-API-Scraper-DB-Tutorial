{-- Copyright julian.bottiglione@epitech.eu
-- API Type
--}

{-# LANGUAGE TypeOperators #-}
-- ^ Needed for :<|>

module Api.Api where

import Servant ((:<|>))

import qualified Api.Routes.HelloWorld as Routes (HelloWorld)
import qualified Api.Routes.Login as Routes (Login)

type Api = 
        Routes.HelloWorld
    :<|> Routes.Login