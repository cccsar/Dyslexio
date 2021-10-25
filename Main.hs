module Main where

import REPL (intro,loop)
import BackEnd (baseUserState)

-- Program entry point.
main = do
    putStrLn intro
    loop baseUserState