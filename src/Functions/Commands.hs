module Functions.Commands ( 
    botSayHello,
    botSayCommands ) where

import BotTypes (
    OKThen (..),
    OKThen (..),
    Result (..),
    MessageInfo (..),
    From (..),
    Chat (..),
    Entities (..),
    Commands (..) )

import qualified Data.ByteString.Char8 as BS (
    readFile )

import qualified Data.Yaml as Y (
    decode )

import Data.DateTime ( 
    getCurrentTime,
    addSeconds )

import Functions.Global (
    botSender,
    writeLog )

-- helper of bot commands
botSayCommands :: String -> String -> OKThen -> [String] -> IO ()
botSayCommands url token botData list = mapM_ (botSender url token chatID) list
    where chatID = chatId $ chat $ message $ last $ result botData


-- to order say hello
botSayHello :: String -> String -> OKThen -> IO ()
botSayHello url token botData =
    let name = fromUsername $ from $ message $ last $ result botData
        mess = text $ message $ last $ result botData
        chatID = chatId $ chat $ message $ last $ result botData
    in botSender url token chatID ("Hi, " ++ name ++ "! Now I can talk with you. You have told me: " ++ mess)
