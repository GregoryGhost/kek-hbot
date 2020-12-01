{-# LANGUAGE OverloadedStrings #-}

module Main where

import HBot.Env as Env
import Control.Monad.Except

-- main :: IO ()
-- main = do
--     env <- Env.load
--     print env
--     runBot env `catchError` handle
--     where
--         runBot env = putStrLn "TODO"
--         handle = undefined

import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Simple
import Control.Concurrent.Async

-- main :: IO ()
-- main = do
--     response <- httpLBS "http://httpbin.org/get"

--     putStrLn $ "The status code was: " ++
--                show (getResponseStatusCode response)
--     print $ getResponseHeader "Content-Type" response
--     L8.putStrLn $ getResponseBody response

main :: IO ()
main = do
  a1 <- async $ do httpLBS "http://www.haskell.org/"

  a2 <- async $ do httpLBS "http://www.duckduckgo.com/"

  page1 <- wait a1
  page2 <- (\x -> "kek" ++ show x) <$> wait a2

  print page1
  print page2