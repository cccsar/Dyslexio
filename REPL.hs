module REPL where

import qualified Data.Map as M

import Control.Monad (foldM)
import System.Directory (doesFileExist)
import Data.Either (partitionEithers)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)
import Data.List (intercalate)

import qualified Tokens as Tk
import qualified Error as Err
import qualified BackEnd as BE

-- Prompt display and user input.
loop :: BE.UserState' -> IO ()
loop tks = do
    putStr prompt
    hFlush stdout
    inp <- getLine

    choice tks inp >>= loop

-- Logic for interpretation of special commands.
choice :: BE.UserState' -> String -> IO BE.UserState'
choice ustate inp = case words inp of
    (".lex":xs)  -> printLex (unwords xs) ustate

    (".load":xs) -> do
        let filename = unwords xs
        exists <- doesFileExist filename

        if exists then do
            content <- readFile (unwords xs)
            putStrLn $ "Loading " ++ show filename ++ " .. "

            let lns = BE.numberedLines content
                baseState = ustate { BE.currentOpenFile = Just filename }

            finalState <- foldM (\currentUstate (line,content) -> 
                        let modifUstate = currentUstate { BE.nextLine = line}
                        in choice modifUstate content ) baseState lns

            return $ finalState { BE.nextLine = BE.nextLine ustate, BE.currentOpenFile = BE.currentOpenFile ustate } 

            else do
                putStrLn "File does not exists"
                return ustate

    [".failed"] -> do  
        if M.size (BE.errorDict ustate) == 0 then do
            putStrLn "No errors to show."
            return ustate

            else do
                let errorsToDisplay = concatMap
                        (\(filename,context) -> map
                            (\content -> '\t':content++",") $ displayFileErrors filename context)
                                (M.toAscList (BE.errorDict ustate))

                putStrLn "[ "
                mapM_ putStrLn errorsToDisplay
                putStrLn "] "

                return ustate


    [".reset"]  -> if M.size (BE.errorDict ustate) == 0 then do 
                    putStrLn "List of errors already empty."  -- ### Distinguish between an empty list of erros
                                                              -- and a non existent one
                    return ustate
                    else return $ ustate { BE.errorDict = M.empty } 

    ["."]       -> exitSuccess

    _         -> do
        process inp
        return ustate


{- REPL Interface functions -}

process :: String -> IO ()
process tks = putStrLn $ "ERROR: " ++ tks ++ " ==> undefined interpretation"

printLex :: String -> BE.UserState' ->  IO BE.UserState'
printLex str ustate = do
    let scan = BE.lexer str
        (errs,tkList) = partitionEithers scan

    if null errs then  do
        putStrLn $ displayTokens str tkList
        return ustate

        else do
            case BE.currentOpenFile ustate of
                Just filename -> do
                    -- Print layout when errors appear on a file
                    let fileErrors = displayFileErrors filename [(BE.nextLine ustate,str,errs)]
                    mapM_ putStrLn fileErrors

                    -- Update error dictionary with found errors
                    let lineNumber = BE.nextLine ustate
                        newErrorTrack = BE.insertDictionary filename (lineNumber,str,errs) (BE.errorDict ustate)

                    return $ ustate { BE.errorDict = newErrorTrack }

                _             -> do
                    -- Print layout when errors appear directly on input
                    putStrLn (displayErrors str errs)

                    return ustate


displayFileErrors :: String -> [(Int,String,[Err.TokenError])] -> [String]
displayFileErrors filename errors = 
    map (\(line,context,err) -> "( " ++ filename++ ", "
                ++ show line ++ ", " ++ displayErrors context err ++ ")") errors

displayErrors :: String -> [Err.TokenError] -> String
displayErrors str errs =
    "ERROR:lexer(" ++ show str ++ ") ==> " ++
    "tokens invalidos de la entrada: ["
    ++ intercalate " , " (map show errs)
    ++ " ]"

displayTokens  :: String -> [Tk.ContextToken] -> String
displayTokens str tkList =
    "OK:lexer(" ++ show str ++ ") ==> [ " 
    ++ intercalate " , " (map show tkList) ++ " ]"


{- Additional display functions -}

-- Clears the screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

-- Moves to a position on the screen.
goTo :: (Int,Int) -> IO ()
goTo (x, y) = putStr $ "\ESC[" ++ show y ++ ";" ++ show x ++ "H"

-- Clears the screen and goes to the beginning  of it.
initializeDisplay :: IO ()
initializeDisplay = do
    clearScreen
    goTo (0,0)

{- Constants: messages, errors, warnings -}

prompt :: String
prompt = "Dyslexio> "

intro = "Welcome to Dyslexio! a good option to interpretate LIPS programming language"