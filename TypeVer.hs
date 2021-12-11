module TypeVer where

{-
 - Module for propper type validation
 -}

import SymTable

class ValidType a where
    checkType :: a -> Bool 