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
 - Every correctly inserted symbol (one that passes type validation and does not incurr into runtime errors), 
 - has a type and a content associated with in the symbol table. 
 - 
 - Additionally, predefined functions exist on the symbol table containing 'Nothing' values for those fields, since
 - this was the semantic chosen to represent them.
 - 
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
    | REFXTYPE A.Type
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
    show VOID = "()" 

data SymbolContext = Context { 
    symbolType :: Maybe A.Type,
    symbolContent :: Maybe Result
    }

type SymTable = M.Map String SymbolContext

-- | Pretty display for symbol Table Debugging
prettySymT :: SymTable -> String
prettySymT symT = concatMap 
                    (\(name, info) -> name ++ "\n\tType: " ++ show (symbolType info) 
                                     ++ "\n\tContent: "++ show (symbolContent info) ++ "\n") 
                    asList
    where asList = M.toList symT 

{- Helper functions -}

-- | Inserts a symbol into the symbolTable
insertSymbolInfo :: String -> SymbolContext  -> SymTable -> SymTable
insertSymbolInfo = M.insert 

-- | Perfoms symbol lookup and brings the symbol context.
getSymbolContext :: String -> SymTable -> Maybe SymbolContext
getSymbolContext = M.lookup

-- | Performs symbol lookup and retrieves the type of a symbol from a symbol table.
getSymbolType :: String -> SymTable -> Either String (Maybe A.Type)
getSymbolType anId symT = case getSymbolContext anId symT of
    Nothing      -> Left $ "Symbol '" ++ anId ++ "' not in environment."
    Just context -> Right (symbolType context)

-- | Performs symbol lookup and retrieves the content of a symbol (its RVALUE) from a symbol table.
getSymbolContent :: String -> SymTable -> Either String Result
getSymbolContent anId symT = case getSymbolContext anId symT of
    Nothing      -> Left $ "Symbol '" ++ anId ++ "' not in environment."
    Just context -> Right $ fromJust $ symbolContent context

reset :: SymTable
reset = initialST

{- Constants -}

-- | Known Symbols at all times. Those are the names of the predefined functions.
predefinedSymbols :: [String]
predefinedSymbols = [
    "if",
    "type",
    "ltype",
    "cvalue",
    "reset", 
    "irandom", 
    "fibo", 
    "gcd",
    "max",
    "min",
    "lcm",
    "now" ]

-- | Initial symbolTable to work with
initialST :: M.Map String SymbolContext
initialST = M.fromList $ zip predefinedSymbols (repeat (Context {symbolType = Nothing, symbolContent = Nothing}))