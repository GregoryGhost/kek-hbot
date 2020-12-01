{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module HBot.Env
  ( App (..),
    Env (..),
    Config (..),
    load,
    telegramToken
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Text (unpack, Text)
import Data.Typeable
import GHC.Generics
import HBot.Core.Logger
import System.Directory
import System.FilePath

type App = ReaderT Env IO

data Env
  = Env
      { config :: Config
      }
  deriving (Show)

data LogSettings
  = LogSettings
      { logLvl :: LogLvl,
        logFile :: !Text
      }
  deriving (Show, Generic)

data Config
  = Config
      { log :: LogSettings,
        telegram :: TelegramSettings
      }
  deriving (Show, Generic)

newtype DecodeError = DecodeError String deriving (Show)

instance Exception DecodeError

data TelegramSettings
  = TelegramSettings
      { token :: !Text
      }
  deriving (Show, Generic)

telegramToken :: IO String
telegramToken = do
  env <- load
  let tgToken = unpack $ token $ telegram $ config env
  pure tgToken

initEnv :: Reader Config Env
initEnv = do
  config <- ask
  let env = Env {config = config}
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

instance FromJSON TelegramSettings

instance ToJSON TelegramSettings

instance FromJSON LogSettings

instance ToJSON LogSettings

--TODO: move to core module
instance FromJSON LogLvl

instance ToJSON LogLvl
