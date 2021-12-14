module Main where

import REPL
import BackEnd (baseUserState)
import Control.Monad.State (runStateT)

-- | Program entry point.
main :: IO ()
main = do
    initializeDisplay
    putStrLn "Welcome to Dyslexio! a good option to interpretate LIPS programming language"

    fmap fst $ runStateT loop baseUserState