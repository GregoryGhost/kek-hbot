{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module EchoSpec (
    KekCmd(..), BasicCmd(..), PureEval(..)
) where

import HBot.Core.Cmd
import HBot.Core.BasicCmd
import Data.Text as T
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Void
import Data.Typeable

data KekCmd = Echo deriving (Interpretable, Enum, Bounded, Helpeable)
data Messanger = Telegram | Slack | Vk

instance Cmd KekCmd String

instance Show KekCmd where
    show Echo = "The echo command returns the entered text."

instance PureEval KekCmd String String where
    eval BotCmd { cmd = c } = pure . show $ c

