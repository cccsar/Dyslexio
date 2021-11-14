module BackEnd 
( UserState(..)
, numberedLines
, lexer
, insertDictionary
, baseUserState
, parse
)
where

{-
 - Module with supporting functions and data definitions for the REPL module
 -}

import Data.Char(isSpace)
import System.FilePath(FilePath)

import qualified Data.Map as M

import qualified AST as A
import qualified Error as Err
import qualified Lexer as L
import qualified Parser as P
import qualified Tokens as Tk


-- | Line number, and erorr string
type ErrorContext = (Int,String) 
type Filename = String
-- | A dictionary is a word and a list of acceptions for that word.
type Dictionary a b = M.Map a [b]

data UserState = UState 
    { errorDictionary :: Dictionary Filename ErrorContext,
      nextLine :: Int,
      currentOpenFile :: Maybe Filename,
      pathName :: FilePath
    }


{- Helper functions -}

-- | Generic function to append list elements in map where "values" are keys.
insertDictionary :: Ord k => k -> a -> M.Map k [a] -> M.Map k [a]
insertDictionary word meaning = M.insertWith (++) word [meaning]

{- | Given a string repreNewLinesenting a file, returns an association list between the line 
 - number and the line content disregarding empty or only whitespace lines.
 -}
numberedLines :: String -> [(Int,String)] 
numberedLines = numberLines 1
    where
        numberLines :: Int -> String -> [(Int,String)]
        numberLines _ [] = []
        numberLines n xs 
            | null postNewLine = [(n,preNewLine)]
            | otherwise = if null preNewLine || all isSpace preNewLine 
                          then next 
                          else (n,preNewLine) : next 
            where 
                (preNewLine,postNewLine) = span (/='\n') xs
                next = numberLines (n+1) (tail postNewLine)

-- | For now, optional
checkIntegerOverflow :: Int -> Bool
checkIntegerOverflow n 
    | n > uprBound || n < lwrBound = False
    | otherwise                    = True
    where 
        uprBound = 2147483647  -- Positive integer bound, equivalent to 2^31 - 1 (ommits sign bit)
        lwrBound = -2147483648 -- Negative integer bound, equivalent to 2^31 (ommits sign bit)

{- Relevant Virtual Machine functions -}

-- | This function is a renaming of the alexScanTokens function that performs tokenization.
lexer :: String -> [Either Err.TokenError Tk.ContextToken]
lexer = L.alexScanTokens

{- | This function is a renaming of the parse function that creates an AST from an
 - input stream of Tokens
 -}
parse :: [Tk.ContextToken] -> A.Program
parse = reverse . P.parse


{- Constants -}

baseUserState :: UserState
baseUserState = UState 
                    { errorDictionary = M.empty 
                    , nextLine = 1
                    , currentOpenFile = Nothing
                    , pathName = ""
                    }