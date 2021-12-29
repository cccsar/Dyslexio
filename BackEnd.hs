module BackEnd
( UserState(..)
, GlobalState
, errorRegistration
, getFileErrorString
, setInputLine
, resetSymT
, insertDictionaryST
, insertSymbolST
, symbolDefinedST
, getSymbolContentST
, getSymbolContextST
, getSymbolTypeST
, numberedLines
, removeCancelledActions
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
import System.IO (stderr,hPutStrLn)
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
    { errorDictionary :: Dictionary Filename ErrorContext
    , inputLine :: String
    , currentOpenFile :: Maybe Filename
    , pathName :: FilePath
    , nextLine :: Int
    , symT :: ST.SymTable
    }

type GlobalState a = StateT UserState IO a

{- | Function for error report.
 - It first checks if the error to be reported is a file error.
 - * If it is, it adjusts print layout and inserts the error into the global error dictionary.
 - * If it is not, simply displays the error.
 -}
errorRegistration :: String -> GlobalState ()
errorRegistration errorString = do
    ustate <- get
    case currentOpenFile ustate of
        Just filename -> do -- ! Print layout when errors appear on a file.
            let fileErrorString = getFileErrorString filename (nextLine ustate) errorString
 
            lift $ hPutStrLn stderr fileErrorString
 
            -- Update error dictionary with found errors.
            let lineNumber    = nextLine ustate
                errorContext  = (lineNumber,errorString)

            insertDictionaryST filename errorContext
        _             -> lift $ hPutStrLn stderr errorString -- ! Print layout when errors are typed directly.

-- | Given a file and it's related errors, returns a list of error strings.
getFileErrorString :: String -> Int -> String -> String
getFileErrorString filename line errorString = 
    "( " ++ filename ++ ", " ++ show line ++ ", " ++ errorString ++ " )"

-- | setter to quickly store input line.
setInputLine :: String -> GlobalState ()
setInputLine input = do 
    ustate <- get
    put ustate { inputLine = input }

resetSymT :: GlobalState () 
resetSymT = do 
    ustate <- get

    put $ ustate {symT = ST.initialST }

-- | Statefull insertion of a symbol into GlobalState symT 
insertSymbolST :: String -> ST.SymbolContext -> GlobalState ()
insertSymbolST anId context = do
    ustate <- get

    let newST = ST.insertSymbolInfo anId context (symT ustate)

    put ustate { symT = newST }

-- | Statefull query of a symbol into GlobalState symT
symbolDefinedST :: String -> GlobalState Bool
symbolDefinedST anId = fmap isJust (getSymbolContextST anId)

-- | Statefull query of symbol context.
getSymbolContextST :: String -> GlobalState (Maybe ST.SymbolContext)
getSymbolContextST anId = do
    ustate <- get

    let context = ST.getSymbolContext anId (symT ustate)

    return context

-- | Statefull query of symbol type.
getSymbolTypeST :: String -> GlobalState (Either String (Maybe A.Type))
getSymbolTypeST anId = do
    ustate <- get

    return $ ST.getSymbolType anId (symT ustate)

{- | Statefull query of symbol content. It is assumed that the symbol content
 - is either always present or the symbol not exists. This is possible due to static type validation,
 - preventing symbols without a type to go through
 -}
getSymbolContentST :: String -> GlobalState ST.Result
getSymbolContentST anId = do
    ustate <- get

    case ST.getSymbolContent anId (symT ustate) of
        Left errorMsg -> do
            -- insertError errorMsg ###
            lift $ putStrLn errorMsg
            return ST.ERROR
        Right tp -> return tp

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

-- | Function necessary to interpretate actions that were successfully type validated.
removeCancelledActions :: A.Program -> [Bool] -> A.Program
removeCancelledActions imp@A.Ins{} xs = imp{ A.list = map snd . filter fst . zip xs $ A.list imp }
removeCancelledActions el _ = el

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
                    , inputLine = ""
                    , nextLine = 1
                    , currentOpenFile = Nothing
                    , pathName = ""
                    , symT = ST.initialST
                    }

