module Tokens where

newtype Position = Pos (Int,Int) 

instance Show Position where
    show (Pos (l,c)) = "line: " ++ show l ++ " - column: " ++ show c

data Content = Integer Int | Id String 

instance Show Content where
    show (Integer e) = show e
    show (Id e)      = show e

data ContextToken = CtxToken
    { position :: Position 
    , string :: String
    , stringContent :: Maybe Content
    , tk :: Token
    } 

instance Show ContextToken where
    show ctxTk = case stringContent ctxTk of
        Nothing  -> show (tk ctxTk)
        Just cnt -> show (tk ctxTk) ++ "(" ++ show cnt ++ ")"


data Token
    -- Reserved words
    = TkInt 
    | TkBool 
    | TkType
    | TkLazy

    -- Constants
    | TkNum
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