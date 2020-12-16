module Main where

-- main :: IO ()
-- main = do
--     env <- Env.load
--     print env
--     runBot env `catchError` handle
--     where
--         runBot env = putStrLn "TODO"
--         handle = undefined

import Control.Concurrent.Async
import Control.Lens
import Control.Lens.TH
import Control.Monad.Except
import Data.Aeson as Aeson
import Data.Aeson.Casing
import Data.Aeson.Encode.Pretty as Pretty
import Data.Aeson.Lens
import Data.Aeson.Parser
import Data.Aeson.TH
import Data.Aeson.Types as AesonT
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Conduit (($$))
import Data.Conduit.Attoparsec (sinkParser)
import Data.Typeable
import Deriving.Aeson
import GHC.Generics
import HBot.Env as Env
import HBot.Env (telegramToken)
import Network.HTTP.Client as HClient
import Network.HTTP.Client.Conduit (bodyReaderSource)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Simple as HttpS
import Network.HTTP.Types.Status (statusCode)
import Control.Applicative


type JsonSettings = CustomJSON '[OmitNothingFields, FieldLabelModifier (StripPrefix "_", CamelToSnake)]

data User
  = User
      { _id :: Int,
        _isBot :: Bool,
        _firstName :: String,
        _username :: String,
        _canJoinGroups :: Bool,
        _canReadAllGroupMessages :: Bool,
        _supportsInlineQueries :: Bool
      }
  deriving stock (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via JsonSettings User

makeFieldsNoPrefix ''User

data ResultTelegram a
  = ResultTelegram
      { _ok :: Bool,
        _result :: a
      }
  deriving stock (Show, Generic)
  deriving
    (FromJSON, ToJSON)
    via JsonSettings (ResultTelegram a)

makeFieldsNoPrefix ''ResultTelegram

type GetMeResult = ResultTelegram User

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

main :: IO ()
main = do
  token <- telegramToken
  isTelegramAuthed <- checkTelegramAuth token
  if isTelegramAuthed
    then print "Telegram auth OK"
    else print "Telegram auth FAILED"
