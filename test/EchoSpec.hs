{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module EchoSpec (check) where

import HBot.Cmds.Echo as Echo
import HBot.Core.Cmd (Cmd(..), BotCmd(..))
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Instances
import Control.Monad.Catch

instance Arbitrary BotKekCmdStr where
    arbitrary = genBotKekCmd

genBotKekCmd :: Gen BotKekCmdStr
genBotKekCmd = do 
      args <- getPrintableString <$> arbitrary

      pure $ BotCmd Echo [args]

prop_evalEcho :: BotKekCmdStr -> Bool
prop_evalEcho cmd = case Echo.eval cmd of { 
    Left a -> False;
    Right a -> formatEcho == a; } where
    formatEcho = "Entered text: " ++ (show $ args cmd)


return []
check = $(quickCheckAll)