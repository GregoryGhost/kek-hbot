module Main where

-- main :: IO ()
-- main = do
--     env <- Env.load
--     print env
--     runBot env `catchError` handle
--     where
--         runBot env = putStrLn "TODO"
--         handle = undefined

import HBot.Env as Env
import HBot.Env (telegramToken)
import HBot.Messangers.Telegram.Bot

main :: IO ()
main = do
  token <- telegramToken
  isTelegramAuthed <- checkTelegramAuth token
  if isTelegramAuthed
    then print "Telegram auth OK"
    else print "Telegram auth FAILED"
