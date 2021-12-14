module REPL 
( loop
, initializeDisplay
) where

{-
 - Module containing the REPL related logic, as well as some
 - helper functions concerning display only.
-}

import Control.Monad (foldM)
import Control.Monad.State
import Data.Either (partitionEithers)
import Data.List (intercalate)
import System.Directory (doesFileExist)
import System.Exit (exitSuccess)
import System.FilePath (dropFileName)
import System.IO (hFlush, stdout)

import qualified Data.Map as M

import qualified AST as A
import qualified BackEnd as BE
import qualified Error as Err (TokenError)
import qualified Interpreter as I
import qualified Tokens as Tk (ContextToken)
import qualified SymTable as ST
import qualified TypeVer as Tv


-- | Prompt display and user inputut.
loop :: BE.GlobalState () 
loop = do
    
    input <- lift $ do 
        putStr prompt
        hFlush stdout
        getLine

    choice input 
    
    loop

-- | Logic for interpretation of special commands.
--choice :: BE.UserState -> String -> IO BE.UserState
choice :: String -> BE.GlobalState ()
choice input = case words input of
    (".lex":xs)  -> checkLexErrors (unwords xs) lexerAction 
    (".load":xs) -> chooseLoad xs
    [".failed"]  -> chooseFailed 
    [".reset"]   -> chooseReset 
    (".ast":xs)  -> checkLexErrors (unwords xs) astAction 
    (".symT":xs) -> do
        ustate <- get
        lift $ putStrLn (ST.prettySymT (BE.symT ustate))
    ["."]        -> lift $ exitSuccess
    _            -> process input 

-- | Implementation of logic for loading a file.
-- chooseLoad ::  BE.UserState -> [String] -> IO BE.UserState
chooseLoad :: [String] -> BE.GlobalState ()
chooseLoad input = do
    ustate <- get

    let filename = BE.pathName ustate ++ unwords input

    exist <- lift $ doesFileExist filename

    if exist 
        then do
        content <- lift $ readFile filename 

        lift $ putStrLn $ "--> Loading " ++ show filename ++ " .. "

        let numberAndLineList = BE.numberedLines content
            filePath = dropFileName filename           
            baseState = ustate { BE.currentOpenFile = Just filename
                               , BE.pathName = filePath 
                               }

        -- Set a base state for execution
        put baseState 

        foldM 
            (\() (line,lineContent) -> do 
                -- modify state on each line
                currentUstate <- get
                put $ currentUstate { BE.nextLine = line }
                choice lineContent 
            ) 
            () 
            numberAndLineList

        newState <- get

        -- reset state
        put $ newState { BE.nextLine        = BE.nextLine ustate
                       , BE.currentOpenFile = BE.currentOpenFile ustate
                       , BE.pathName        = BE.pathName ustate
                       , BE.symT            = BE.symT ustate
                       } 
        else lift $ putStrLn $ "ERROR: No such file \"" ++ filename ++ "\"."

-- | Implementation of logic for file error display.
chooseFailed :: BE.GlobalState () 
chooseFailed = do
    ustate <- get

    if M.size (BE.errorDictionary ustate) == 0 
        then lift $ putStrLn "WARNING: Empty error list. No errors to show."

        else do
            let errors = concatMap 
                          (\(filename,errorContext) -> 
                              map (\(line,errorString) -> 
                                   '\t' : getFileErrorString filename line errorString ++ ","
                                  )
                                  errorContext
                          )
                          (M.toAscList (BE.errorDictionary ustate)) 

                errorDisplay = init errors ++ [init $ last errors]
                
            lift $ do 
                putStrLn "[ "
                mapM_ putStrLn errorDisplay
                putStrLn "] "

-- | Implementation of logic for reseting error list.
chooseReset :: BE.GlobalState () 
chooseReset = do
    ustate <- get
    if M.size (BE.errorDictionary ustate) == 0 
        then lift $  putStrLn "List of errors is empty."  
        else put $ ustate { BE.errorDictionary = M.empty } 

{- REPL Interface functions -}

-- | Only displays an error message for now.
process :: String -> BE.GlobalState () 
process inputLine = checkLexErrors inputLine validateAction --putStrLn $ "ERROR: " ++ inputLine ++ " ==> undefined interpretation."


{- | Given user or file input, check whether there is a lexer error and act accordingly -}
checkLexErrors :: String -> String -> BE.GlobalState () 
checkLexErrors inputLine action = do 
    let scan = BE.lexer inputLine
        (errors,tokens) = partitionEithers scan

    if null errors 
        then onSuccessLex tokens inputLine action 
        else onFailLex errors inputLine 

{- | Given a token stream, depending on action either display successul tokenization or 
 - display AST as a program -}
onSuccessLex :: [Tk.ContextToken] -> String -> String -> BE.GlobalState () 
onSuccessLex tokens inputLine action = case action of 
    "lexer"     -> lift $ putStrLn $ getAcceptationString inputLine tokens
    "ast"       -> showAST tokens 
    "validate"  -> validate tokens 
    _           -> error "REPL Error --> Panic!: This condition shouldn't ever occur."

{- | Print lexer error accordingly -}
onFailLex :: [Err.TokenError] -> String -> BE.GlobalState () 
onFailLex errors inputLine = do
    ustate <- get
    case BE.currentOpenFile ustate of
        Just filename -> do
            -- Print layout when errors appear on a file.
            let errorString     = getErrorString inputLine errors
                fileErrorString = getFileErrorString filename (BE.nextLine ustate) errorString
 
            lift $ putStrLn fileErrorString
 
            -- Update error dictionary with found errors.
            let lineNumber    = BE.nextLine ustate
                errorContext  = (lineNumber,errorString)

            BE.insertDictionaryST filename errorContext
 
        _             -> lift $ putStrLn (getErrorString inputLine errors)
            -- Print layout when errors are typed directly.

validate :: [Tk.ContextToken] -> BE.GlobalState () 
validate tks = do
    let parseResult = BE.parse tks

    case parseResult of
        Right resultAst    -> do 

            typeverResult <- Tv.validateProgram resultAst

            case typeverResult of 
                
                -- Here are the actions ### Enhance the semantics and pipeline for this.
                Left listOfAcceptation -> do
                    mapM_ I.execute (A.list (BE.removeCancelledActions resultAst listOfAcceptation)) -- ###

                    if and listOfAcceptation then lift $ putStrLn $ "Ok: All actions performed."
                        else lift $ putStrLn $ "Warning: Some actions weren't performed"

                -- Here are the expressions ### Enhance the semantics and pipeline for this.
                Right (Just _) -> do
                    resultEval <- I.eval (A.expr resultAst) -- ### 

                    case resultEval of 
                        ST.ERROR -> return () 
                        _        -> lift $ putStrLn $ "OK: Result for expression is ==> " ++ show resultEval 
                    
                Right _         -> return () 

        Left parseError -> lift $ putStrLn parseError

{- | Given a input stream of tokens, returns a string representation of the AST
 - generated by the parser.
 -}
showAST :: [Tk.ContextToken] -> BE.GlobalState () 
showAST tks = do
    let parseResult = BE.parse tks

    case parseResult of
        Right result    -> lift $ putStrLn $ show result
        Left parseError -> lift $ putStrLn parseError

-- | Given a file and it's related errors, returns a list of error strings.
getFileErrorString :: String -> Int -> String -> String
getFileErrorString filename line errorString = 
    "( " ++ filename ++ ", " ++ show line ++ ", " ++ errorString ++ " )"

-- | Given input line and a list of errors, returns the correct error string.
getErrorString :: String -> [Err.TokenError] -> String
getErrorString inputLine errors =
    "ERROR: lexer(" ++ show inputLine ++ ") ==> " ++
    "invalid tokens: [ "
    ++ intercalate " , " (map show errors)
    ++ " ]"

-- | Given input line and a list of errors, returns the correct token string.
getAcceptationString  :: String -> [Tk.ContextToken] -> String
getAcceptationString inputLine tokens =
    "OK: lexer(" ++ show inputLine ++ ") ==> [ " 
    ++ intercalate " , " (map show tokens) ++ " ]"

{- Additional display functions -}

-- | Clears the screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

-- | Moves to a position on the screen.
goTo :: (Int,Int) -> IO ()
goTo (x, y) = putStr $ "\ESC[" ++ show y ++ ";" ++ show x ++ "H"

-- | |Clears the screen and goes to the beginning  of it.
initializeDisplay :: IO ()
initializeDisplay = do
    clearScreen
    goTo (0,0)

{- Constants -}

prompt :: String
prompt = "Dyslexio> "

lexerAction , astAction :: String
lexerAction = "lexer"
astAction   = "ast"
validateAction = "validate"