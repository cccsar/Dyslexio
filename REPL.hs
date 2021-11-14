module REPL 
( loop
, initializeDisplay
) where

{-
 - Module containing the REPL related logic, as well as some
 - helper functions concerning display only.
-}

import Control.Monad (foldM)
import Data.Either (partitionEithers)
import Data.List (intercalate)
import System.Directory (doesFileExist)
import System.Exit (exitSuccess)
import System.FilePath (dropFileName,(</>))
import System.IO (hFlush, stdout)


import qualified Data.Map as M

import qualified BackEnd as BE
import qualified Error as Err (TokenError)
import qualified Tokens as Tk (ContextToken)
import qualified Basement.BlockN as BE


-- | Prompt display and user inputut.
loop :: BE.UserState -> IO ()
loop tks = do
    putStr prompt
    hFlush stdout
    input <- getLine

    choice tks input >>= loop

-- | Logic for interpretation of special commands.
choice :: BE.UserState -> String -> IO BE.UserState
choice ustate input = case words input of
    (".lex":xs)  -> checkLexErrors (unwords xs) lexerAction ustate 
    (".load":xs) -> chooseLoad ustate xs
    [".failed"]  -> chooseFailed ustate
    [".reset"]   -> chooseReset ustate 
    (".ast":xs)  -> checkLexErrors (unwords xs) astAction ustate
    ["."]        -> exitSuccess
    _            -> do
        process input
        return ustate

-- | Implementation of logic for loading a file.
chooseLoad ::  BE.UserState -> [String] -> IO BE.UserState
chooseLoad ustate input = do
    let filename = BE.pathName ustate ++ unwords input
    exists <- doesFileExist filename

    if exists 
        then do
        content <- readFile filename 

        putStrLn $ "--> Loading " ++ show filename ++ " .. "

        let numberAndLineList = BE.numberedLines content
            filePath = dropFileName filename           
            baseState = ustate { BE.currentOpenFile = Just filename
                               , BE.pathName = filePath 
                               }

        -- ### This one is a bit confusing, see if it can get simpler
        newState <- foldM 
                    (\currentUstate (line,content) -> 
                        let modifiedUstate = currentUstate { BE.nextLine = line }
                        in choice modifiedUstate content 
                    ) 
                    baseState 
                    numberAndLineList

        return newState { BE.nextLine        = BE.nextLine ustate
                        , BE.currentOpenFile = BE.currentOpenFile ustate
                        , BE.pathName        = BE.pathName ustate
                        } 
        else do
            putStrLn $ "ERROR: No such file \"" ++ filename ++ "\"."
            return ustate

-- | Implementation of logic for file error display.
chooseFailed :: BE.UserState -> IO BE.UserState
chooseFailed ustate = do
    if M.size (BE.errorDictionary ustate) == 0 
        then do
            putStrLn "WARNING: Empty error list. No errors to show."
            return ustate

        else do
            -- ### This one is a bit confusing
            let errors = concatMap 
                          (\(filename,errorContext) -> 
                              map (\(line,errorString) -> 
                                   '\t' : getFileErrorString filename line errorString ++ ","
                                  )
                                  errorContext
                          )
                          (M.toAscList (BE.errorDictionary ustate)) 

                errorDisplay = init errors ++ [init $ last errors]
                
            putStrLn "[ "
            mapM_ putStrLn errorDisplay
            putStrLn "] "

            return ustate

-- | Implementation of logic for reseting error list.
chooseReset :: BE.UserState -> IO BE.UserState
chooseReset ustate = 
    if M.size (BE.errorDictionary ustate) == 0 
        then do 
        putStrLn "List of errors is empty."  
                                              
        return ustate
        else return $ ustate { BE.errorDictionary = M.empty } 

{- REPL Interface functions -}

-- | Only displays an error message for now.
process :: String -> IO ()
process inputLine = putStrLn $ "ERROR: " ++ inputLine ++ " ==> undefined interpretation."

{- | Given user or file input, check whether there is a lexer error and act accordingly -}
checkLexErrors :: String -> String -> BE.UserState -> IO BE.UserState
checkLexErrors inputLine action ustate = do
    let scan = BE.lexer inputLine
        (errors,tokens) = partitionEithers scan

    if null errors 
        then onSuccessLex tokens inputLine action ustate
        else onFailLex errors inputLine ustate

{- | Given a token stream, depending on action either display successul tokenization or 
 - display AST as a program -}
onSuccessLex :: [Tk.ContextToken] -> String -> String -> BE.UserState -> IO BE.UserState 
onSuccessLex tokens inputLine action ustate = case action of 
    "lexer" -> do putStrLn $ getAcceptationString inputLine tokens
                  return ustate
    "ast"   -> showAST tokens ustate
    _       -> undefined -- ###

{- | Print lexer error accordingly -}
onFailLex :: [Err.TokenError] -> String ->  BE.UserState -> IO BE.UserState  
onFailLex errors inputLine ustate = case BE.currentOpenFile ustate of
    Just filename -> do
        -- Print layout when errors appear on a file.
        let errorString     = getErrorString inputLine errors
            fileErrorString = getFileErrorString 
                                filename (BE.nextLine ustate) errorString
        putStrLn fileErrorString

        -- Update error dictionary with found errors.
        let lineNumber    = BE.nextLine ustate
            errorContext  = (lineNumber,errorString)
            newErrorTrack = BE.insertDictionary filename errorContext 
                                (BE.errorDictionary ustate)

        return $ ustate { BE.errorDictionary = newErrorTrack }

    _             -> do
        -- Print layout when errors are typed directly.
        putStrLn (getErrorString inputLine errors)

        return ustate

{- | Given a input stream of tokens, returns a string representation of the AST
 - generated by the parser.
 -}
showAST :: [Tk.ContextToken] -> BE.UserState -> IO BE.UserState 
showAST tks ustate = do
    let parseResult = BE.parse tks
    mapM_ (putStrLn . show) parseResult
    return ustate

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