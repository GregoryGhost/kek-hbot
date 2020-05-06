module Main where

import Lib1

main :: IO ()
main = do
    env <- initEnv
    pure $runBot env `catchError` handle
    where
        handle = undefined
