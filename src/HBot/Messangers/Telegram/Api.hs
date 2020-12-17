module HBot.Messangers.Telegram.Api
  ( getStatusResponse,
    checkTelegramAuth,
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
import Control.Concurrent.Async
import Control.Monad.Except
import Data.Conduit (($$))
import Data.Conduit.Attoparsec (sinkParser)
import Network.HTTP.Client as HClient
import Network.HTTP.Client.Conduit (bodyReaderSource)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Simple as HttpS
import Network.HTTP.Types.Status (statusCode)

getStatusResponse :: L8.ByteString -> IO Bool
getStatusResponse sourceJson = do
  putStrLn $ "Source JSON: " ++ (L8.unpack sourceJson)
  case decoded of
    Just v -> do
      let prettyJson = L8.unpack $ Pretty.encodePretty v
      putStrLn $ "Pretty JSON: " ++ prettyJson
      pure $ v ^. ok
    _ -> do
      print "cant decoded"
      pure False
  where
    decoded = (decode sourceJson) :: Maybe GetMeResult

checkTelegramAuth :: String -> IO Bool
checkTelegramAuth token = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest formattedRequest
  response <- HttpS.httpLbs request
  getStatusResponse $ responseBody response
  where
    formattedRequest = "https://api.telegram.org/bot" ++ token ++ "/getMe"
