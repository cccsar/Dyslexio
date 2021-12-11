module Tokens 
( Content(..)
, ContextToken(..)
, Token (..)
) where

{-
 -  Module concerning token data definitions and their instances. 
 -}

data Content = Integer Int | Id String 

instance Show Content where
    show (Integer content) = show content
    show (Id idName)       = show idName

-- | Datatype for generalizing token components.
data ContextToken = CtxToken
    { position :: Int                 -- ^ Position of token in the input stream (a column).
    , string :: String                -- ^ String tokenized.
    , stringContent :: Maybe Content  -- ^ Content for tokens carrying info.
    , tk :: Token                     -- ^ Token
    } 

instance Show ContextToken where
    show ctxTk = case stringContent ctxTk of
        Nothing  -> show (tk ctxTk)
        Just cnt -> show (tk ctxTk) ++ "(" ++ show cnt ++ ")"

data Token
    -- | Reserved words
    = TkInt 
    | TkBool 
    | TkType
    | TkLazy

    -- | Constants
    | TkNum
    | TkTrue
    | TkFalse

    -- | Ids
    | TkId

    -- | Operators
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

    -- | Symbols
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