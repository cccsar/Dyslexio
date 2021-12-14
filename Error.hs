module Error 
( TokenError(..)
, ErrorMonad(..)
, bind
) where

{-
 - Module concerning error types.
 -}


data TokenError = TkErr 
    { name :: String      -- ^ String in error.
    , pos :: Int          -- ^ Position of the error (a column)
    } 

instance Show TokenError where
    show err = show (name err) ++ " at " ++ show (pos err)

{- Type for parse error representation -}

data ErrorMonad a = Ok a | Failed String

bind :: ErrorMonad a -> (a -> ErrorMonad b) -> ErrorMonad b
m `bind` k = 
   case m of 
        Ok a -> k a
        Failed e -> Failed e