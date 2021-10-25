module Tokens where

newtype Position = Pos (Int,Int) deriving Show

data Content = Bool Bool | Integer Int | Id String deriving Show

data ContextToken = CtxToken
    { position :: Position 
    , string :: String
    , stringContent :: Maybe Content
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

    -- Operators
    | TkOpenPar
    | TkClosePar
    | TkPower
    | TkPlus
    | TkMinus
    | TkNot
    | TkMult
    | TkMod
    | TkLT 
    | TkLE 
    | TkGT
    | TkGE 
    | TkEQ 
    | TkNE 
    | TkAnd
    | TkOr

    -- Symbols
    | TkQuote
    | TkComma
    | TkAssign
    | TkSemicolon
    | TkYields
    | TkRArrow
    | TkLArrow
    | TkOpenBracket
    | TkCloseBracket
    | TkOpenBrace
    | TkCloseBrace
    | TkDot
    | TkColon
    | TkColonColon
    | TkWhile
    | TkIf
    deriving Show