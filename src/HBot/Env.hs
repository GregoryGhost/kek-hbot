module HBot.Env
  ( App (..),
    Env (..),
    Config (..),
    load,
    telegramToken,
  )
where

import Control.Applicative
import Control.Lens
import Control.Lens.TH
import Control.Lens.TH
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.Lens
import Data.Aeson.Parser
import Data.Aeson.TH
import Data.Aeson.Types as AesonT
import qualified Data.ByteString.Lazy as B
import Data.Text (Text, unpack)
import Data.Typeable
import Deriving.Aeson
import GHC.Generics
import HBot.Core.Logger
import HBot.Settings
import System.Directory
import System.FilePath

--TODO: move to core module
instance FromJSON LogLvl

instance ToJSON LogLvl

data TelegramSettings
  = TelegramSettings
      { _token :: !Text
      }
  deriving stock (Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via JsonSettings TelegramSettings

makeFieldsNoPrefix ''TelegramSettings

data LogSettings
  = LogSettings
      { _logLvl :: LogLvl,
        _logFile :: !Text
      }
  deriving stock (Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via JsonSettings LogSettings

makeFieldsNoPrefix ''LogSettings

data Config
  = Config
      { _log :: LogSettings,
        _telegram :: TelegramSettings
      }
  deriving stock (Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via JsonSettings Config

makeFieldsNoPrefix ''Config

data Env
  = Env
      { _config :: Config
      }
  deriving stock (Show, Generic)

makeFieldsNoPrefix ''Env

type App = ReaderT Env IO

newtype DecodeError = DecodeError String deriving (Show)

instance Exception DecodeError

telegramToken :: IO String
telegramToken = do
  env <- load
  let tgToken = unpack $ env ^. config ^. telegram ^. token
  pure tgToken

initEnv :: Reader Config Env
initEnv = do
  config <- ask
  let env = Env config
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
