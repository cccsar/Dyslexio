module Interpreter () where

{-
 - Module containing expression interpretation for the AST
 - ### IDENTIFIER INTERPRETATION UNKNOWN
 -}

import AST

-- | This function assumes the type of an expression is known
evaluate :: Expr -> ConcreteType -> Either Int Bool
evaluate expr tp = undefined


-- | Interpretate arithmetical expressions
arithmetic :: Expr -> Maybe Int
arithmetic (IntExp e)  = Just e
arithmetic (Add l r)   = binOp (arithmetic l) (arithmetic r) (+)
arithmetic (Sub l r)   = binOp (arithmetic l) (arithmetic r) (-)
arithmetic (Minus el)  = arithmetic el >>= (\res -> Just $ res * (-1))
arithmetic (Mult l r)  = binOp (arithmetic l) (arithmetic r) (*)
arithmetic (Mod l r)   = binOp (arithmetic l) (arithmetic r) mod
arithmetic (Power l r) = binOp (arithmetic l) (arithmetic r) pow
arithmetic _           = Nothing

-- | Interpretate Boolean expression
boolean :: Expr -> Maybe Bool
boolean (BoolExp el) = Just el
boolean (And l r) = binOp (boolean l) (boolean r) (&&)
boolean (Or l r)  = binOp (boolean l) (boolean r) (||)
boolean (Not el)  = boolean el >>= (Just . not)
boolean _         = Nothing

-- | Interpretate Relatinal expressions
relational :: Expr -> Maybe Bool
relational (LessThan l r)         = binOp (arithmetic l) (arithmetic r) (<)
relational (LessEqualThan l r)    = binOp (arithmetic l) (arithmetic r) (<=)
relational (GreaterThan l r)      = binOp (arithmetic l) (arithmetic r) (>)
relational (GreaterEqualThan l r) = binOp (arithmetic l) (arithmetic r) (>=)
relational (Equal l r)            = binOp (arithmetic l) (arithmetic r) (==)
relational (NotEqual l r)         = binOp (arithmetic l) (arithmetic r) (<=)
relational exp                    = Nothing -- ### UNIFY

{- Helper Functions -}

-- | Binary operation over monads generalization
binOp :: Monad m => m a -> m b -> (a -> b -> c) -> m c
binOp l r op = do
    a <- l
    b <- r
    return (a `op` b)

-- | Power that goes from int x int -> int
pow :: Int -> Int -> Int
pow el 0 = el
pow el n
    | n < 0 = error "Invalid base"
    | otherwise = el * pow el (n-1)