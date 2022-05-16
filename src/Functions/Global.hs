module Functions.Global (
    botSender,
    writeLog ) where

import qualified Network.HTTP.Simple as HTTPS (
    httpLBS,
    parseRequest,
    getResponseBody,
    Response (..) )

import BotRead (
    botMessage )

import Data.DateTime ( 
    getCurrentTime,
    addSeconds )


-- message bot sender
botSender :: String -> String -> Integer -> String -> IO ()
botSender url key chat message =
    HTTPS.parseRequest (url ++ key ++ "/sendMessage?chat_id=" ++ 
        show chat ++ "&" ++ "text=" ++ message) >>=
    HTTPS.httpLBS >>= botMessage


-- log writer
writeLog :: String -> IO ()
writeLog log =
    getCurrentTime >>= (return . addSeconds 10800) >>= \timeOpen ->
    appendFile "./src/log.txt" (show timeOpen ++ ": " ++ log ++ "\n")
