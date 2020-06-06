module Main where

import HBot.Env as Env
import Control.Monad.Except

main :: IO ()
main = do
    env <- Env.load
    print env
    runBot env `catchError` handle
    where
        runBot env = putStrLn "TODO"
        handle = undefined
