module Main where

import REPL
import BackEnd (baseUserState)

-- | Program entry point.
main :: IO ()
main = do
    initializeDisplay
    putStrLn "Welcome to Dyslexio! a good option to interpretate LIPS programming language"
    loop baseUserState