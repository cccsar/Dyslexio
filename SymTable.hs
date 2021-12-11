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
import qualified AST as A (Type, Expr) 

data Result 
    = BOOL Bool
    | INT Int
    | LAZY A.Expr -- tentative

data SymbolContext = Context { 
    symbolType :: Maybe A.Type,
    symbolContent :: Maybe Result
    }

type SymTable = M.Map String SymbolContext

{- Helper functions -}

insertSymbolInfo :: String -> SymbolContext  -> SymTable -> SymTable
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
    "now" ]

-- Initial symbolTable to work with
initialST = M.fromList $ zip predefinedSymbols (repeat (Context {symbolType = Nothing, symbolContent = Nothing}))