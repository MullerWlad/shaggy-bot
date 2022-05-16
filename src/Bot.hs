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
    decode,
    encodeFile )

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
    Entities (..),
    Sys (..) )


-- use only if code needs to write update in log
extraCherry :: String -> String -> IO ()
extraCherry url token =
    writeLog "extra cherry execute" >>
    BS.readFile "./src/system.yaml" >>= \byte ->
    let parsed = Y.decode byte :: Maybe Sys
    in case parsed of 
    Nothing -> botMessage "Zzzz..." >>
                writeLog "extra cherry rejected, bot sleeps"
    Just (Sys update) ->
        HTTPS.parseRequest (url ++ token ++ "/getUpdates?offset=" ++ (show $ succ update)) >>=
        HTTPS.httpLBS >>= \req ->
        return () >>
        writeLog "extra cherry well executed"


-- use only if code needs to write update in log
succUpId :: String -> String -> IO ()
succUpId url token =
    writeLog "succ update id execute" >>
    BS.readFile "./src/system.yaml" >>= \byte ->
    let parsed = Y.decode byte :: Maybe Sys
    in case parsed of 
    Nothing -> botMessage "Zzzz..." >>
                writeLog "succ update id rejected, bot sleeps"
    Just (Sys update) ->
        writeLog "succ update id well executed" >>
        Y.encodeFile "./src/system.yaml" (Sys $ succ update)


-- to open last update
lastUpdate :: String -> String -> IO ()
lastUpdate url token =
    HTTPS.parseRequest (url ++ token ++ "/getUpdates?offset=-1") >>=
    HTTPS.httpLBS >>= \req ->
    return ()


-- endpoint of application
endPoint :: IO ()
endPoint = 
    writeLog "open bot, reading config.yaml" >>
    botMessage ("Hello, I'm test-bot-1.2.1! aka Shaggy." ++
                "Start reading config.yaml...") >>
    BS.readFile "./src/config.yaml" >>= \byte ->
    let parsed = Y.decode byte :: Maybe Cred
    in case parsed of
    Nothing -> 
        botMessage "Could not parse config.yaml" >>
        endPoint
    (Just (Cred url_api token timeout)) -> 
        writeLog "config.yaml is well red" >>
        writeLog "fetching bot data" >>
        botMessage "Well red config.yaml! Start reading my own data..." >>
        HTTPS.parseRequest (url_api ++ token ++ "/getUpdates") >>=
        HTTPS.httpLBS >>= \updates ->
        let body = HTTPS.getResponseBody updates
            botDataMaybe = JSON.decode body :: Maybe OKThen
            rejust = \(Just content) -> content
        in case botDataMaybe of
        Nothing ->
            botMessage "My data contains nothing!" >>
            lastUpdate url_api token >>
            threadDelay timeout >>
            endPoint
        justBotData ->
            (return $ rejust justBotData) >>= \botData ->
            (case result botData of
                [] -> botMessage "No updates" >> 
                        writeLog "empty updates in endpoint" >>
                        roundInit parsed botData
                _ -> botMessage "Ok, listening api now..." >>
                        writeLog "updates are not empty in endpoint") >>
                        roundInit parsed botData
            

-- half cycle
roundInit :: Maybe Cred -> OKThen ->  IO ()
roundInit parsed botData =
    generalRoundPol parsed botData botExecuteCommand


-- general bot manager
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
        lastUpdate url_api token >>
        threadDelay timeout >>
        generalRoundPol parsed oldUp toDo
    justBotData -> case oldUp == rejust justBotData of
                    True -> threadDelay timeout >>
                            generalRoundPol parsed oldUp toDo
                    False -> botMessage "New update!" >>
                             (toDo url_api token $ rejust justBotData) >>
                             case result $ rejust justBotData of
                                [] -> botMessage "No updates" >> 
                                        writeLog "empty updates in general round pol" >>
                                        threadDelay timeout >>
                                        generalRoundPol parsed (rejust justBotData) toDo
                                _ -> botMessage "New update" >>
                                        writeLog "updates are not empty in general round pol " >>
                                        threadDelay timeout >>
                                        generalRoundPol parsed (rejust justBotData) toDo
