{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Env 
    (
        App(..),
        Env(..),
        Config(..),
        load
    )
where

import qualified Data.ByteString.Lazy as B
import Control.Monad
import Data.Text
import Control.Applicative
import Data.Aeson
import GHC.Generics
import Control.Monad.Reader
import Control.Monad.IO.Class
import HBot.Core.Logger
import System.Directory
import System.FilePath


type App = ReaderT Env IO

data Env = Env { 
    config :: Config
} deriving Show

data Config = Config {
    logLvl :: LogLvl
    , logFile :: !Text
} deriving (Show, Generic)


initEnv :: Reader Config Env
initEnv = do 
    config <- ask
    let env = Env { config = config }

    pure env

getEnv :: Config -> Env
getEnv = runReader initEnv

load :: MonadIO m => m Env
load = do
    currentDir <- liftIO $ getCurrentDirectory
    let fileName = "config.json"
    let byPath = currentDir </> fileName
    gotConfig <- liftIO $ ((eitherDecodeFileStrict byPath) :: IO (Either String Config))
    let env = case gotConfig of Left e -> error e
                                Right config -> getEnv config

    pure env

instance FromJSON Config
instance ToJSON Config

--TODO: move to core module
instance FromJSON LogLvl
instance ToJSON LogLvl