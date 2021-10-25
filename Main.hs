module Main where

import REPL (initializeDisplay,intro,loop)
import BackEnd (baseUserState)

-- Program entry point.
main = do
    initializeDisplay
    putStrLn intro
    loop baseUserState