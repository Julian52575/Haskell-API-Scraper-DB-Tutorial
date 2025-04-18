{-- Copyright julian.bottiglione@epitech.eu
-- Main
--}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FlexibleContexts #-}  -- For MonadReader Env
--{-# LANGUAGE ScopedTypeVariables #-}  -- Typed variables
module Main where

import Data.Text (Text, unpack)
import Data.Functor (void)
import Control.Monad.Reader.Class (MonadReader, asks)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (runReaderT, ReaderT)
import Control.Monad.Trans (lift)

import Env (Env, startingMessage, initEnvFromDotEnv)

printMsgFromEnv :: (MonadReader Env m, MonadIO m) => m ()
printMsgFromEnv = do
  msgRaw <- asks startingMessage
  liftIO $ putStrLn (unpack msgRaw)

main :: IO ()
main = do
  env <- initEnvFromDotEnv
  runReaderT printMsgFromEnv env
