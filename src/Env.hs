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


type App = ReaderT Env IO

data Env = Env { 
    config :: Config
}

data Config = Config {
    logLvl :: LogLvl
    , fileLog :: !Text
} deriving (Show, Generic)


load :: (MonadIO m) => m App
load = do
    config <- (eitherDecode <$> getJSON) :: IO (Either String Config)
    let env = Env { config = config }
    app <- ask--TODO: ???
    pure app

instance FromJSON Config
instance ToJSON Config

--TODO: move to core module
instance FromJSON LogLvl
instance ToJSON LogLvl

getJSON :: IO B.ByteString
getJSON = B.readFile byPath
    where byPath = getCurrentDirectory ++ fileName
          fileName = "config.json"