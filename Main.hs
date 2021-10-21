module Main where

import Data.Either (partitionEithers)
import System.Directory (doesFileExist)
import System.Exit (exitSuccess)

import qualified Lexer as L (alexScanTokens)
import qualified Tokens as Tk
import qualified Error as Err

data UserState = UState 
    { tks :: [Either Err.TokenError Tk.ContextToken ]
    }

-- Entry point
main = do
    putStrLn intro
    loop (UState { tks = [] })

-- Prompt display and user input.
loop :: UserState -> IO ()
loop tks = do
    putStr prompt
    inp <- getLine

    choice tks inp

-- Logic for interpretation of special commands.
choice :: UserState -> String -> IO ()
choice ustate inp = case words inp of 
    (".lex":xs)  -> do 
        lexResult <- lexer (unwords xs)
        loop $ ustate { tks = lexResult } 

    (".load":xs) -> do
        rel <- doesFileExist (unwords xs)

        if rel then do
            content   <- readFile (unwords xs)
            newTks <- lexer content
            loop $ ustate { tks = newTks }
            else do
                putStrLn "File does not exists"
                loop ustate 

    [".failed"] -> do 
        let (errs,_) = partitionEithers (tks ustate)

        if null errs then do
            putStrLn "No errors to show."
            loop ustate
            else do 
                putStrLn "Errors: "
                mapM_ print errs
                loop ustate

    [".reset"]  -> loop $ ustate { tks = [] } 

    ["."]       -> exitSuccess

    _         -> do 
        process inp
        loop ustate
        

{- REPL Interface functions -}

process :: String -> IO ()
process tks = putStrLn $ "ERROR: " ++ tks ++ " Undefined interpretation."

lexer :: String -> IO [Either Err.TokenError Tk.ContextToken]
lexer str = do 
    let scan = L.alexScanTokens str
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