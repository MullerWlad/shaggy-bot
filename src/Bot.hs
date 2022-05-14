{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Monad law, left identity" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Use if" #-}

module Bot (
    endPoint ) where

import qualified Data.ByteString.Char8 as BS (
    readFile )

import qualified Network.HTTP.Simple as HTTPS (
    httpLBS,
    parseRequest,
    getResponseBody,
    Response (..) )

import Control.Concurrent ( threadDelay )

import qualified Data.ByteString.Lazy.Char8 as L8 (
    putStrLn )

import qualified Data.Yaml as Y (
    decode )

import qualified Data.Aeson as JSON (
    FromJSON (..),
    Options (..),
    genericParseJSON,
    defaultOptions,
    fieldLabelModifier,
    decode )

import BotRead (
    botMessage )

import Functions.Directives ( 
    botExecuteCommand )

import Functions.Global (
    writeLog )

import BotTypes (
    Cred (..),
    OKThen (..),
    Result (..),
    MessageInfo (..),
    From (..),
    Chat (..),
    Entities (..) )


endPoint :: IO ()
endPoint = 
    writeLog "open bot, reading config.yaml" >>
    botMessage ("Hello, I'm test-bot-1.2.1! aka Shaggy." ++
                "Start reading config.yaml...") >>
    BS.readFile "./src/config.yaml" >>= \byte ->
    let parsed = Y.decode byte :: Maybe Cred
    in case parsed of
    Nothing -> 
        writeLog "could not parse config.yaml" >>
        botMessage "Could not parse config.yaml"
    (Just (Cred url_api token timeout)) -> 
        writeLog ("stored config: " ++
                  url_api ++ " " ++
                  show token ++ " " ++
                  show timeout) >>
        writeLog "fetching bot data" >>
        botMessage "Well red config.yaml! Start reading my own data..." >>
        HTTPS.parseRequest (url_api ++ token ++ "/getUpdates") >>=
        HTTPS.httpLBS >>= \updates ->
        let body = HTTPS.getResponseBody updates
            botDataMaybe = JSON.decode body :: Maybe OKThen
            rejust = \(Just content) -> content
        in case botDataMaybe of
        Nothing ->
            writeLog "empty bot data" >>
            botMessage "My data contains nothing!" >>
            threadDelay timeout >>
            endPoint
        justBotData ->
            (return $ rejust justBotData) >>= \botData ->
            case result botData of
            [] -> botMessage "No updates" >> 
                    writeLog "bot data responsed, empty updates" >>
                    roundInit parsed botData
            _ -> botMessage ("Oh, hello, " ++ 
                    (fromFirst_name $ from $ message $ head $ result botData) ++ 
                    " " ++
                    (fromLast_name $ from $ message $ head $ result botData) ++ 
                    ", my data well red, I'm ready to work with you! Listening...") >>
                    writeLog (show ((\res -> show (("update: " ++ (show $ update_id res)), 
                                        ("message: " ++ (show $ message_id $ message res)), 
                                        ("text: " ++ (show $ text $ message res)),
                                        ("from: " ++ (show $ fromUsername $ from $ message res) ++ " " ++ (show $ fromIs_bot $ from $ message res)),
                                        ("chat: " ++ (show $ chatId $ chat $ message res) ++ " " ++ (show $ chatType $ chat $ message res)))) <$> result botData)) >>
                    roundInit parsed botData


roundInit :: Maybe Cred -> OKThen ->  IO ()
roundInit parsed botData =
    generalRoundPol parsed botData botExecuteCommand


generalRoundPol :: Maybe Cred -> OKThen -> (String -> String -> OKThen -> IO ()) -> IO ()
generalRoundPol parsed@(Just (Cred url_api token timeout)) oldUp toDo =
    HTTPS.parseRequest (url_api ++ token ++ "/getUpdates") >>=
    HTTPS.httpLBS >>= \updates ->
    let body = HTTPS.getResponseBody updates
        botDataMaybe = JSON.decode body :: Maybe OKThen
        rejust = \(Just content) -> content
    in case botDataMaybe of
    Nothing ->
        putStrLn "My data contains nothing in RoundPol, Try reading config.yaml again..." >>
        threadDelay timeout >>
        generalRoundPol parsed oldUp toDo
    justBotData -> case oldUp == rejust justBotData of
                    True -> threadDelay timeout >>
                            generalRoundPol parsed oldUp toDo
                    False -> botMessage "New update!" >>
                             writeLog (show ((\res -> show (("update: " ++ (show $ update_id res)), 
                                        ("message: " ++ (show $ message_id $ message res)), 
                                        ("text: " ++ (show $ text $ message res)),
                                        ("from: " ++ (show $ fromUsername $ from $ message res) ++ " " ++ (show $ fromIs_bot $ from $ message res)),
                                        ("chat: " ++ (show $ chatId $ chat $ message res) ++ " " ++ (show $ chatType $ chat $ message res)))) <$> (result $ rejust justBotData))) >>
                             (toDo url_api token $ rejust justBotData) >>
                             threadDelay timeout >>
                             generalRoundPol parsed (rejust justBotData) toDo
