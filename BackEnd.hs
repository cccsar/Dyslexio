module BackEnd
( UserState(..)
, GlobalState (..)
, insertDictionaryST
, insertSymbolST
, symbolDefinedST  
, getSymbolContextST 
, getSymbolTypeST
, numberedLines
, baseUserState
, lexer
, parse
)
where

{-
 - Module with supporting functions and data definitions for the REPL module
 -}

import Data.Char(isSpace)
import Data.Maybe (isJust)
import System.FilePath(FilePath)
import Control.Monad.State

import qualified Data.Map as M

import qualified AST as A
import qualified Error as Err
import qualified Lexer as L
import qualified Parser as P
import qualified SymTable as ST
import qualified Tokens as Tk


-- | Line number, and erorr string
type ErrorContext = (Int,String)
type Filename = String
-- | A dictionary is a word and a list of acceptions for that word.
type Dictionary a b = M.Map a [b]

data UserState = UState
    { errorDictionary :: Dictionary Filename ErrorContext,
      nextLine :: Int,
      currentOpenFile :: Maybe Filename,
      pathName :: FilePath,
      symT :: ST.SymTable
    }

type GlobalState a = StateT UserState IO a


-- | Statefull insertion of a symbol into GlobalState symT 
insertSymbolST :: String -> ST.SymbolContext -> GlobalState ()
insertSymbolST id context = do
    ustate <- get

    let newST = ST.insertSymbolInfo id context (symT ustate)

    put ustate { symT = newST } 

-- | Statefull query of a symbol into GlobalState symT
symbolDefinedST :: String -> GlobalState Bool
symbolDefinedST id = fmap isJust (getSymbolContextST id)

-- | Statefull query of symbol context.
getSymbolContextST :: String -> GlobalState (Maybe ST.SymbolContext)
getSymbolContextST id = do 
    ustate <- get

    let context = ST.getSymbolContext id (symT ustate)

    return context

-- | Statefull query of symbol type.
getSymbolTypeST :: String -> GlobalState (Either String (Maybe A.Type))
getSymbolTypeST id = do  
    ustate <- get

    return $ ST.getSymbolType id (symT ustate) 

-- | Statefull query of symbol content.
getSymbolContentST :: String -> GlobalState (Maybe ST.Result)
getSymbolContentST id = do  
    ustate <- get

    case ST.getSymbolContent id (symT ustate) of 
        Left errorMsg -> do 
            -- insertError errorMsg ###
            lift $ putStrLn errorMsg
            return Nothing
        Right tp -> return tp

-- | Insertion of error into state ??
{-
insertError :: String -> GlobalState ()
insertError errMsg = do
    ustate <- get

    let newErrors = errMsg : errors ustate


    put ustate { errors = newErrors }
-}

{- Functions for error dictionary -}

-- | Generic function to append list elements in map where "values" are keys.
insertDictionary :: Ord k => k -> a -> M.Map k [a] -> M.Map k [a]
insertDictionary word meaning = M.insertWith (\a b-> b ++ a) word [meaning]

-- | Statefull insertion on error dictionary
insertDictionaryST :: Filename -> ErrorContext -> GlobalState ()
insertDictionaryST file errorContext = do 
    ustate <- get 

    let newDict = insertDictionary file errorContext (errorDictionary ustate)

    put ustate {errorDictionary = newDict}

{- Helper functions -}

{- | Given a string representing a file, returns an association list between the line 
 - number and the line content disregarding empty or only whitespace lines.
 -}
numberedLines :: String -> [(Int,String)]
numberedLines = numberLines 1
    where
        numberLines :: Int -> String -> [(Int,String)]
        numberLines _ [] = []
        numberLines n xs
            | null postNewLine = [(n,preNewLine)]
            | otherwise = if null preNewLine || all isSpace preNewLine
                          then next
                          else (n,preNewLine) : next
            where
                (preNewLine,postNewLine) = span (/='\n') xs
                next = numberLines (n+1) (tail postNewLine)

{- Relevant Virtual Machine functions -}

-- | This function is a renaming of the alexScanTokens function that performs tokenization.
lexer :: String -> [Either Err.TokenError Tk.ContextToken]
lexer = L.alexScanTokens

{- | This function is a renaming of the parse function that creates an AST from an
 - input stream of Tokens
 -}
parse :: [Tk.ContextToken] -> Either String A.Program
parse tks = case P.parse tks of
    Err.Ok result  -> Right result
    Err.Failed err -> Left err


{- Constants -}

baseUserState :: UserState
baseUserState = UState
                    { errorDictionary = M.empty
                    , nextLine = 1
                    , currentOpenFile = Nothing
                    , pathName = ""
                    , symT = ST.initialST
                    }

