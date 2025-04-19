{-- Copyright julian.bottiglione@epitech.eu
-- Env data record
--}

{-# LANGUAGE RecordWildCards #-}
-- ^ Allow .. to initalize data record members

module Env where

import Data.Text (Text, pack)
import Data.IP (IPv4)

import System.Environment (getEnv)

data Env = Env {
    startingMessage :: Text
    , apiPort :: Int
    , databaseIp :: IPv4
    , databaseTable :: Text
} deriving (Show)

initEnv :: IO Env
initEnv = do
    startingMessage <- pack <$> getEnv "STARTING_MESSAGE"
    apiPort <- read <$> getEnv "API_PORT"
    databaseIp <- read <$> getEnv "DATABASE_IP"
    databaseTable <- pack <$> getEnv "DATABASE_TABLE"
    pure Env{..}