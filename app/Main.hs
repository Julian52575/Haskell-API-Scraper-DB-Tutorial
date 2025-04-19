{-- Copyright julian.bottiglione@epitech.eu
-- Main
--}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FlexibleContexts #-}  -- For MonadReader Env
--{-# LANGUAGE ScopedTypeVariables #-}  -- Typed variables
module Main where

import Data.Text (unpack)
import Control.Monad.Reader.Class (MonadReader, asks)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import Network.Wai.Handler.Warp (run)

import Env (Env, startingMessage, initEnv)
import Api.Server (appM)


printMsgFromEnv :: (MonadReader Env m, MonadIO m) => m ()
printMsgFromEnv = do
  msgRaw <- asks startingMessage
  liftIO $ putStrLn (unpack msgRaw)

main :: IO ()
main = do
  env <- initEnv
  app <- appM
  runReaderT printMsgFromEnv env
  run 8080 app