module BackEnd where

import qualified Tokens as Tk
import qualified Error as Err
import qualified Lexer as L

import Data.Either (partitionEithers)

data UserState = UState 
    { tks :: [Either Err.TokenError Tk.ContextToken ]
    }

{- Relevant functions Virtual Machine -}

lexer :: String -> [Either Err.TokenError Tk.ContextToken]
lexer = L.alexScanTokens

baseUserState = UState { tks = []}