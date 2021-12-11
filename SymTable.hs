module SymTable 
( SymTable
, insertSymbolInfo
, reset
)
where

{-
 - Symbol Table implementation
 - 
 - As for now, the symbol table contains information about the type of a symbol or simply 
 - the name of a predefiend function
 -}

import qualified Data.Map as M
import AST (Type, Expr)

data SymbolContext = Context { symbolType :: Maybe Type}

type SymTable = M.Map String SymbolContext

{- Helper functions -}

insertSymbolInfo :: String -> Type -> SymTable -> SymTable
insertSymbolInfo = M.insert 

reset :: SymTable
reset = initialST

{- Constants -}

-- Known Symbols at all times. Those are the names of the predefined functions.
predefinedSymbols = [
    "if",
    "type",
    "ltype",
    "cvalue",
    "reset", 
    "irandom", 
    "fibo", 
    "gcd",
    "now"
]

-- Initial symbolTable to work with
initialST = M.fromList $ zip predefinedSymbols (replicate Nothing)