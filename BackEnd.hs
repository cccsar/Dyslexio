module BackEnd where

import qualified Tokens as Tk
import qualified Error as Err
import qualified Lexer as L

data UserState = UState 
    { tks :: [Either Err.TokenError Tk.ContextToken ]
    } -- ###

{- Relevant functions Virtual Machine -}

lexer :: String -> [Either Err.TokenError Tk.ContextToken]
lexer = L.alexScanTokens

{- Constants -}
baseUserState = UState {tks = []}