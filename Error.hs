module Error (TokenError(..)) where

import qualified Tokens as Tk (Position(..))


data TokenError = TkErr 
    { name :: String      -- ^ String in error.
    , pos :: Tk.Position  -- ^ Position of the error.
    } 

instance Show TokenError where
    show err = show (name err) ++ " en " ++ show (pos err)