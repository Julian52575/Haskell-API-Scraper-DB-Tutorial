{-- Copyright julian.bottiglione@epitech.eu
-- Main
--}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FlexibleContexts #-}  -- For MonadReader Env
--{-# LANGUAGE ScopedTypeVariables #-}  -- Typed variables
module Main where

import Control.Monad.Trans.Reader (runReaderT)
import Network.Wai.Handler.Warp (run)

import Env (initEnv)
import Api.Server (appM)

main :: IO ()
main = do
  env <- initEnv
  app <- runReaderT appM env
  run 8080 app