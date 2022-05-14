{-# LANGUAGE DeriveGeneric #-}
module BotTypes (
    Cred (..),
    OKThen (..),
    Result (..),
    MessageInfo (..),
    From (..),
    Chat (..),
    Entities (..),
    Commands (..) ) where

import qualified Data.Aeson as JSON (
    FromJSON (..),
    genericParseJSON,
    Options (..),
    defaultOptions,
    fieldLabelModifier )

import GHC.Generics ( 
    Generic )

import Data.Char (
    toLower )

import Control.Category ( (>>>) )


data Cred = Cred { 
    url_api :: String,
    token :: String,
    timeout :: Int
} deriving (Show, Generic, Eq)
instance JSON.FromJSON Cred

data Commands = Commands {
    commands :: [String]
} deriving (Show, Generic, Eq)
instance JSON.FromJSON Commands

data OKThen = OKThen {
    ok :: Bool,
    result :: [Result]
} deriving (Show, Generic, Eq)
instance JSON.FromJSON OKThen

data Result = Result {
    update_id :: Integer,
    message :: MessageInfo
} deriving (Show, Eq, Generic)
instance JSON.FromJSON Result

data MessageInfo = MessageInfo {
    message_id :: Integer,
    from :: From,
    chat :: Chat,
    date :: Integer,
    text :: String
    --entities :: [Entities]
} deriving (Show, Generic, Eq)
instance JSON.FromJSON MessageInfo

data From = From {
    fromId :: Integer,
    fromIs_bot :: Bool,
    fromFirst_name :: String,
    fromLast_name :: String,
    fromUsername :: String,
    fromLanguage_code :: String
} deriving (Show, Generic, Eq)
instance JSON.FromJSON From where
    parseJSON =
        JSON.genericParseJSON $ jsonOptions "from"

data Chat = Chat {
    chatId :: Integer,
    chatFirst_name :: String,
    chatLast_name :: String,
    chatUsername :: String,
    chatType :: String
} deriving (Show, Generic, Eq)
instance JSON.FromJSON Chat where
    parseJSON =
        JSON.genericParseJSON $ jsonOptions "chat"

data Entities = Entities {
    entOffset :: Integer,
    entLength :: Integer,
    entType :: String
} deriving (Show, Generic, Eq)
instance JSON.FromJSON Entities where
    parseJSON =
        JSON.genericParseJSON $ jsonOptions "ent"


jsonOptions :: String -> JSON.Options
jsonOptions prefix =
    let prefixLength = length prefix
        lowercaseFirstCharacter (c : rest) = toLower c : rest
        lowercaseFirstCharacter [] = []
    in JSON.defaultOptions {
        JSON.fieldLabelModifier = drop prefixLength >>> lowercaseFirstCharacter
    }
