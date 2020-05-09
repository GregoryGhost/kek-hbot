module Main where

import Env

main :: IO ()
main = do
    env <- Env.load
    putStrLn env
    pure $ runBot env `catchError` handle
    where
        runBot env = undefined
        handle = undefined
