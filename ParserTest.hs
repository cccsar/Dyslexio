module ParserTest where

import System.Directory (doesFileExist)
import System.IO (hFlush, stdout)
import Parser (parse)
import BackEnd (lexer)
import Data.Either (partitionEithers)

main :: IO ()
main = do
    putStr"provide file: "
    hFlush stdout

    filename <- getLine
    exists <- doesFileExist filename

    if exists
        then do
            content <- readFile filename
            let (errs,tks) = partitionEithers $ lexer content

            if null errs then do
                let parseResult = reverse $ parse tks 
                mapM_ (putStrLn . show) parseResult
                main

                else do
                    putStrLn "Lexer errors"
                    mapM_ (putStrLn . show) errs 
                    main

        else putStrLn "file does not exists" >> main