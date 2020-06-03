{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module HBot.Env
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
import Data.Typeable
import Data.Aeson
import GHC.Generics
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Monad.Catch
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

newtype DecodeError = DecodeError String deriving Show

instance Exception DecodeError


initEnv :: Reader Config Env
initEnv = do 
    config <- ask
    let env = Env { config = config }

    pure env

getEnv :: Config -> Env
getEnv = runReader initEnv

load :: (MonadIO m, MonadThrow m) => m Env
load = do
    currentDir <- liftIO $ getCurrentDirectory
    let fileName = "config.json"
    let byPath = currentDir </> fileName
    let decodedConfig = (eitherDecodeFileStrict byPath) :: IO (Either String Config)
    config <- decodeError =<< liftIO decodedConfig
    let env = getEnv config

    pure env

decodeError :: MonadThrow m => Either String a -> m a
decodeError = either (throwM . DecodeError) pure

instance FromJSON Config
instance ToJSON Config

--TODO: move to core module
instance FromJSON LogLvl
instance ToJSON LogLvl