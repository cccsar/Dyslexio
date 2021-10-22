module Tokens where

newtype Position = Pos (Int,Int) deriving Show

data Content = Bool Bool | Integer Int | Id String deriving Show

data ContextToken = CtxToken
    { position :: Position 
    , string :: String
    , stringContent :: Content
    , tk :: Token
    } deriving (Show)

data Token
    -- Reserved words
    = TkNum 
    | TkBool 

    -- Constants
    | TkInteger
    | TkTrue
    | TkFalse

    -- Ids
    | TkId
    deriving Show