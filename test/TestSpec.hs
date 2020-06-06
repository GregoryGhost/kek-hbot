{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module TestSpec (check) where

import HBot.Cmds.Echo
import Test.QuickCheck
import Test.QuickCheck.All
import Test.QuickCheck.Instances

double :: Integer -> Integer
double n = 2 * n

prop_doubleEven :: Integer -> Bool
prop_doubleEven n = even (double n)

return []
check = $(quickCheckAll)