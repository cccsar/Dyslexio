module BackEnd where

import qualified Tokens as Tk
import qualified Error as Err
import qualified Lexer as L

import Data.Char(isSpace)
import qualified Data.Map as M

data UserState = UState 
    { tks :: [Either Err.TokenError Tk.ContextToken ]
    } -- ###

data UserState' = UState' 
    { errorDict :: M.Map String [(Int,String,[Err.TokenError])],
      nextLine :: Int,
      currentOpenFile :: Maybe String }


insertDictionary :: Ord k => k -> a -> M.Map k [a] -> M.Map k [a]
insertDictionary word meaning = M.insertWith (++) word [meaning]

{- Helper functions -}

-- Given a string representing a file, returns an association list between the line 
-- number and the line content disregarding empty or only whitespace lines.
numberedLines :: String -> [(Int,String)] 
numberedLines = numberLines 1
    where
        numberLines :: Int -> String -> [(Int,String)]
        numberLines _ [] = []
        numberLines n xs 
            | null post = [(n,pre)]
            | otherwise = if null pre || all isSpace pre then next 
                            else (n,pre) : next 
            where 
                (pre,post) = span (/='\n') xs
                next = numberLines (n+1) (tail post)


{- Relevant functions Virtual Machine -}

lexer :: String -> [Either Err.TokenError Tk.ContextToken]
lexer = L.alexScanTokens

{- Constants -}
baseUserState = UState {tks = [] }
baseUserState' = UState' { errorDict = M.empty , nextLine = 1, currentOpenFile = Nothing }