module REPL where

import Data.Either (partitionEithers)
import System.Directory (doesFileExist)
import System.Exit (exitSuccess)

import qualified Tokens as Tk
import qualified Error as Err
import qualified BackEnd as BE

-- Prompt display and user input.
loop :: BE.UserState -> IO ()
loop tks = do
    putStr prompt
    inp <- getLine

    newTks <- choice tks inp
    loop newTks

-- Logic for interpretation of special commands.
choice :: BE.UserState -> String -> IO BE.UserState
choice ustate inp = case words inp of 
    (".lex":xs)  -> do 
        lexResult <- printLex (unwords xs)
        return $ ustate { BE.tks = lexResult } 

    (".load":xs) -> do
        rel <- doesFileExist (unwords xs)

        if rel then do
            content <- readFile (unwords xs)
            -- Extend logic
            newTks  <- printLex content
            return $ ustate { BE.tks = newTks }
            else do
                putStrLn "File does not exists"
                return ustate 

    [".failed"] -> do 
        let (errs,_) = partitionEithers (BE.tks ustate)

        if null errs then do
            putStrLn "No errors to show."
            return ustate
            else do 
                putStrLn "Errors: "
                mapM_ print errs
                return ustate

    [".reset"]  -> return $ ustate { BE.tks = [] } 

    ["."]       -> exitSuccess

    _         -> do 
        process inp
        return ustate

{- REPL Interface functions -}

process :: String -> IO ()
process tks = putStrLn $ "ERROR: " ++ tks ++ " Undefined interpretation."

printLex :: String -> IO [Either Err.TokenError Tk.ContextToken]
printLex str = do 
    let scan = BE.lexer str
        (errs,tkList) = partitionEithers scan

    if null errs then  do
        mapM_ print tkList 
        else do 
            putStrLn "Errors: "
            mapM_ print errs
    
    return scan

{- Constants: messages, errors, warnings -}

prompt :: String
prompt = "Dyslexio> "

intro = "Welcome to Dyslexio! your bset intrepetre for LIPS programming language"