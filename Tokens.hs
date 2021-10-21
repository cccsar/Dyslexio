module Tokens where

newtype Position = Pos (Int,Int) deriving Show

data ContextToken = CtxToken
    { pos :: Position 
    , content :: String
    , tk :: Token
    } deriving (Show)

data Token
    -- reserved words
    = TkNum 
    | TkBool 

    -- values
    | TkInteger
    | TkTrue
    | TkFalse

    -- ids
    | TkId

    deriving (Show)