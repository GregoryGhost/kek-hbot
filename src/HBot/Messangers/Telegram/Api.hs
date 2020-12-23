module HBot.Messangers.Telegram.Api
  ( checkTelegramAuth,
  )
where

--TODO: функции по получению данных от телеги не должны напрямую обращать к API телеги
--TODO: должен быть общий механизм по обработке запросов на основе контракта
--TODO: функции должны просто подготавливать данные, а обработчик должен быть в другом месте

import Control.Lens
import Control.Lens.TH
import Data.Aeson as Aeson
import Data.Aeson.Encode.Pretty as Pretty
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Typeable
import HBot.Messangers.Telegram.Types
import HBot.Messangers.Telegram.Requests
import Control.Concurrent.Async
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.Conduit (($$))
import Data.Conduit.Attoparsec (sinkParser)
import Network.HTTP.Client as HClient
import Network.HTTP.Client.Conduit (bodyReaderSource)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Simple as HttpS
import Network.HTTP.Types.Status (statusCode)



data TelegramError = TelegramError 
  {
    _description :: !String
  }
  deriving Show

instance Exception TelegramError

makeFieldsNoPrefix ''TelegramError


checkTelegramAuth :: (MonadIO m, MonadThrow m) =>  String -> m Bool
checkTelegramAuth token = do
  me <- getMe token
  pure $ me ^. ok

getMe :: (MonadIO m, MonadThrow m) => String -> m GetMeResult
getMe token = do
  manager <- liftIO $ newManager tlsManagerSettings
  request <- parseRequest formattedRequest
  response <- HttpS.httpLbs request
  case (getMeResult response) of
    Just v -> pure v
    Nothing -> throwM $ TelegramError "Can't deserialize GetMe json"
  where
    formattedRequest = "https://api.telegram.org/bot" ++ token ++ "/getMe"

getMeResult :: Response L8.ByteString -> Maybe GetMeResult
getMeResult response = decode $ HClient.responseBody response
