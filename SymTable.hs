module SymTable 
( SymTable
, SymbolContext (..)
, Result (..)
, insertSymbolInfo
, getSymbolContext
, getSymbolType
, getSymbolContent
, reset
, initialST
, prettySymT
)
where

{-
 - Symbol Table implementation
 - 
 - As for now, the symbol table contains information about the type of a symbol or simply 
 - the name of a predefiend function
 -}

import qualified Data.Map as M
import Data.Maybe (fromJust)

import qualified AST as A (Type, Expr, ConcreteType) 

data Result 
    -- Typed results
    = BOOL Bool
    | INT Int
    | LAZY A.Expr 
    -- Reflexive type results
    | REFXTYPE A.ConcreteType
    | LTYPE A.Type
    -- void result for functions
    | VOID 
    -- error
    | ERROR 


instance Show Result where
    show (BOOL True)    = "true"
    show (BOOL False)   = "false"
    show (INT a)     = show a
    show (LAZY expr) = show expr
    show (REFXTYPE tp) = show tp 
    show (LTYPE tp) = show tp
    show ERROR = "" 

data SymbolContext = Context { 
    symbolType :: Maybe A.Type,
    symbolContent :: Maybe Result
    }

type SymTable = M.Map String SymbolContext

prettySymT :: SymTable -> String
prettySymT symT = concatMap 
                    (\(name, info) -> name ++ "\n\tType: " ++ show (symbolType info) 
                                     ++ "\n\tContent: "++ show (symbolContent info) ++ "\n") 
                    asList
    where asList = M.toList symT 

{- Helper functions -}

insertSymbolInfo :: String -> SymbolContext  -> SymTable -> SymTable
insertSymbolInfo = M.insert 

getSymbolContext :: String -> SymTable -> Maybe SymbolContext
getSymbolContext = M.lookup

getSymbolType :: String -> SymTable -> Either String (Maybe A.Type)
getSymbolType id symT = case getSymbolContext id symT of
    Nothing      -> Left $ "Symbol '" ++ id ++ "' not in environment."
    Just context -> Right (symbolType context)

getSymbolContent :: String -> SymTable -> Either String Result
getSymbolContent id symT = case getSymbolContext id symT of
    Nothing      -> Left $ "Symbol '" ++ id ++ "' not in environment."
    Just context -> Right $ fromJust $ symbolContent context

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