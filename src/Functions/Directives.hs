module Functions.Directives (
    botSayHello,
    botExecuteCommand ) where

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

import Functions.Commands ( 
    botSayHello,
    botSayCommands )


-- command structure: shaggy_command
-- command executer
botExecuteCommand :: String -> String -> OKThen -> IO ()
botExecuteCommand url token botData =
    getCurrentTime >>= (return . addSeconds 10800) >>= \time ->
    let name = fromUsername $ from $ message $ last $ result botData
        mess = text $ message $ last $ result botData
        chatID = chatId $ chat $ message $ last $ result botData
        --date' = date $ message $ last $ result botData
    in if take 7 mess == "shaggy_"
       then let command = drop 7 mess
            in case command of
               "" -> botSender url token chatID (name ++ " orders nothing in " ++ show time) >>
                     writeLog (name ++ " spams logs!!!")
               command -> botSender url token chatID (name ++ " orders " ++ command ++ " in " ++ show time) >>
                          writeLog (name ++ " chooses " ++ command) >>
                          commandChoose url token botData command
       else return ()
    where commandChoose :: String -> String -> OKThen -> String -> IO ()
          commandChoose url token botData command =
              BS.readFile "./src/commands.yaml" >>= \byte ->
              let parsed = Y.decode byte :: Maybe Commands
              in case parsed of
                 Nothing -> writeLog "no commands in commands.yaml" >>
                            botSender url 
                                      token 
                                      (chatId $ chat $ message $ last $ result botData) 
                                      "no commands, my lord"
                 (Just (Commands comList)) ->
                     if command `elem` ((head . words) <$> comList)
                     then 
                        case command of
                        "say_hello" -> botSayHello url token botData >>
                                       writeLog (command ++ " executed")
                        "help" -> botSayCommands url token botData comList >>
                                          writeLog (command ++ " executed")
                        _ -> botSender url 
                                        token 
                                        (chatId $ chat $ message $ last $ result botData) 
                                        "command is not finished yet, my lord" >>
                             writeLog (command ++ " rejected")
                     else writeLog (command ++ " rejected") >>
                          botSender url 
                                    token 
                                    (chatId $ chat $ message $ last $ result botData) 
                                    "command does not exist, my lord"
