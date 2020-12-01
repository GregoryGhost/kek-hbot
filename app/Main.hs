{-# LANGUAGE OverloadedStrings #-}

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
import Data.Aeson.Parser (json)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Conduit (($$))
import Data.Conduit.Attoparsec (sinkParser)
import HBot.Env as Env
import Network.HTTP.Client as HClient
import Network.HTTP.Client.Conduit (bodyReaderSource)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Simple
import Network.HTTP.Types.Status (statusCode)
import HBot.Env (telegramToken)


-- main :: IO ()
-- main = do
--     response <- httpLBS "http://httpbin.org/get"

--     putStrLn $ "The status code was: " ++
--                show (getResponseStatusCode response)
--     print $ getResponseHeader "Content-Type" response
--     L8.putStrLn $ getResponseBody response

checkTelegramAuth :: String -> IO Bool
checkTelegramAuth token = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest formattedRequest
  HClient.withResponse request manager $ \response -> do
    putStrLn $
      "The status code was: "
        ++ show (statusCode $ responseStatus response)
    value <-
      bodyReaderSource (responseBody response)
        $$ sinkParser json
    print value
  pure False
  where
    formattedRequest = "https://api.telegram.org/bot" ++ token ++ "/getMe"

main :: IO ()
main = do
  token <- telegramToken
  isTelegramAuthed <- checkTelegramAuth token
  if isTelegramAuthed
    then print "Telegram auth OK"
    else print "Telegram auth FAILED"
