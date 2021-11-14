module Error 
( TokenError(..)
, ParseError(..)
, E(..)
, thenE
, returnE
, failE
) where

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


data ParseError = Error

{- Experimenting -}

data E a = Ok a | Failed String

thenE :: E a -> (a -> E b) -> E b
m `thenE` k = 
   case m of 
        Ok a -> k a
        Failed e -> Failed e

returnE :: a -> E a
returnE a = Ok a

failE :: String -> E a
failE err = Failed err

-- Use this to try to induce descriptive error messages
catchE :: E a -> (String -> E a) -> E a
catchE m k = 
    case m of
        Ok a -> Ok a
        Failed e -> k e
