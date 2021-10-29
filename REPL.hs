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
    (".lex":xs)  -> printLex (unwords xs) ustate
    (".load":xs) -> chooseLoad ustate xs
    [".failed"]  -> chooseFailed ustate
    [".reset"]   -> chooseReset ustate 
    ["."]       -> exitSuccess
    _           -> do
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

        return newState { BE.nextLine = BE.nextLine ustate
                        , BE.currentOpenFile = BE.currentOpenFile ustate
                        , BE.pathName = BE.pathName ustate
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
                            (\(filename,context) -> 
                                map 
                                    (\content -> '\t':content++",") 
                                    (displayFileErrors filename context)
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

{- | Given user or file input, displays the result of tokenization accordingly and
 - updates user state when necessary.
 -}
printLex :: String -> BE.UserState ->  IO BE.UserState
printLex inputLine ustate = do
    let scan = BE.lexer inputLine
        (errors,tokens) = partitionEithers scan

    if null errors 
        then do
        putStrLn $ displayTokens inputLine tokens
        return ustate

        else do
            case BE.currentOpenFile ustate of
                Just filename -> do
                    -- Print layout when errors appear on a file.
                    let fileErrors = displayFileErrors 
                                        filename [(BE.nextLine ustate,inputLine,errors)]
                    mapM_ putStrLn fileErrors

                    -- Update error dictionary with found errors.
                    let lineNumber    = BE.nextLine ustate
                        errorContext  = (lineNumber,inputLine,errors)
                        newErrorTrack = BE.insertDictionary filename errorContext 
                                            (BE.errorDictionary ustate)

                    return $ ustate { BE.errorDictionary = newErrorTrack }

                _             -> do
                    -- Print layout when errors are typed directly.
                    putStrLn (displayErrors inputLine errors)

                    return ustate

-- | Given a file and it's related errors, returns a list of error strings.
displayFileErrors :: String -> [(Int,String,[Err.TokenError])] -> [String]
displayFileErrors filename errors = map
                                    (\(line,context,err) -> 
                                        "( " ++ filename++ ", " ++ show line ++ ", " 
                                        ++ displayErrors context err ++ ")"
                                    ) 
                                    errors

-- | Given input line and a list of errors, returns the correct error string.
displayErrors :: String -> [Err.TokenError] -> String
displayErrors inputLine errors =
    "ERROR: lexer(" ++ show inputLine ++ ") ==> " ++
    "invalid token: [ "
    ++ intercalate " , " (map show errors)
    ++ " ]"

-- | Given input line and a list of errors, returns the correct token string.
displayTokens  :: String -> [Tk.ContextToken] -> String
displayTokens inputLine tokens =
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