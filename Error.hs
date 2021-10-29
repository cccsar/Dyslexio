module Error (TokenError(..)) where

{-
 - Module concerning error types.
 -}

import qualified Tokens as Tk (Position(..))


data TokenError = TkErr 
    { name :: String      -- ^ String in error.
    , pos :: Tk.Position  -- ^ Position of the error.
    } 

instance Show TokenError where
    show err = show (name err) ++ " at " ++ show (pos err)