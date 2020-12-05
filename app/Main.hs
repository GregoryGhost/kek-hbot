{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

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
import Control.Monad.Except
import Data.Aeson.Parser
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Conduit (($$))
import Data.Conduit.Attoparsec (sinkParser)
import HBot.Env as Env
import HBot.Env (telegramToken)
import Network.HTTP.Client as HClient
import Network.HTTP.Client.Conduit (bodyReaderSource)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Simple as HttpS
import Network.HTTP.Types.Status (statusCode)
import Data.Aeson as Aeson
import Data.Aeson.Types as AesonT
import Data.Aeson.Casing
import Data.Aeson.Encode.Pretty as Pretty
import Data.Typeable
import GHC.Generics


data User
  = User
      {
        id :: Int
        , isBot :: Bool
        , firstName :: String
        , username :: String
        , canJoinGroups :: Bool
        , canReadAllGroupMessages :: Bool
        , supportsInlineQueries :: Bool
      } deriving (Show, Generic)

data ResultTelegram a
  = ResultTelegram
      {
        ok :: Bool
        , result :: a
      } deriving (Show, Generic)

type GetMeResult = ResultTelegram User

instance FromJSON User where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }
instance ToJSON User where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance FromJSON GetMeResult where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }
instance ToJSON GetMeResult where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

getStatusResponse :: L8.ByteString -> IO Bool
getStatusResponse sourceJson = do
  putStrLn $ "Source JSON: " ++ (L8.unpack sourceJson)
  case decoded of
    Just v -> do
      let prettyJson = L8.unpack $ Pretty.encodePretty v
      putStrLn $ "Pretty JSON: " ++ prettyJson
      pure $ ok v
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
  
