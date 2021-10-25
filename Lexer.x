{ 
module Lexer where

import qualified Tokens as Tk (Token(..), ContextToken(..), Position(..), Content (..))
import qualified Error as Err
import Data.Char (toUpper)
}

%wrapper "posn"

$digit = 0-9			
$alpha = [a-zA-Z]		

tokens :-
    $white+           ;

    -- reserved words                      
    int                                 {\posn str-> buildToken Tk.TkNum posn str}   
    bool                                {\posn str-> buildToken Tk.TkBool posn str}              
    
     -- constants
    [\-]{0,1}$digit+                    {\posn str-> buildToken Tk.TkInteger posn str } 
    true                                {\posn str-> buildToken Tk.TkTrue posn str }
    false                               {\posn str-> buildToken Tk.TkFalse posn str }
    
    -- operators
    \(                                  {\posn str-> buildToken Tk.TkOpenPar posn str}
    \)                                  {\posn str-> buildToken Tk.TkOpenPar posn str}
    \^                                  {\posn str-> buildToken Tk.TkPower posn str}
    \+                                  {\posn str-> buildToken Tk.TkPlus posn str}
    \-                                  {\posn str-> buildToken Tk.TkMinus posn str}
    \!                                  {\posn str-> buildToken Tk.TkNot posn str}
    \*                                  {\posn str-> buildToken Tk.TkMult posn str}
    \%                                  {\posn str-> buildToken Tk.TkMod posn str}
    \<                                  {\posn str-> buildToken Tk.TkLT posn str}
    \<=                                 {\posn str-> buildToken Tk.TkLE posn str}
    \>=                                 {\posn str-> buildToken Tk.TkGE posn str}
    \>                                  {\posn str-> buildToken Tk.TkGT posn str}
    \=                                  {\posn str-> buildToken Tk.TkEQ posn str}
    \<>                                 {\posn str-> buildToken Tk.TkNE posn str}
    \&&                                 {\posn str-> buildToken Tk.TkAnd posn str}
    \|\|                                {\posn str-> buildToken Tk.TkOr posn str}
    
    -- symbols
    \'                                  {\posn str-> buildToken Tk.TkQuote posn str}
    \,                                  {\posn str-> buildToken Tk.TkComma posn str}
    \:=                                 {\posn str-> buildToken Tk.TkAssign posn str}
    \;                                  {\posn str-> buildToken Tk.TkSemicolon posn str}
    \=>                                 {\posn str-> buildToken Tk.TkYields posn str}
    \->                                 {\posn str-> buildToken Tk.TkRArrow posn str}
    \<\-                                 {\posn str-> buildToken Tk.TkLArrow posn str}
    \[                                  {\posn str-> buildToken Tk.TkOpenBracket posn str}
    \]                                  {\posn str-> buildToken Tk.TkCloseBracket posn str}
    \{                                  {\posn str-> buildToken Tk.TkOpenBrace posn str}
    \}                                  {\posn str-> buildToken Tk.TkCloseBrace posn str}
    \.                                  {\posn str-> buildToken Tk.TkDot posn str}
    \:                                  {\posn str-> buildToken Tk.TkColon posn str}
    \::                                 {\posn str-> buildToken Tk.TkColonColon posn str}
    while                               {\posn str-> buildToken Tk.TkWhile posn str}
    if                                  {\posn str-> buildToken Tk.TkIf posn str}
   
    -- ids
    ([$alpha\_]){1,}[$alpha\_$digit]*   {\posn str-> buildToken Tk.TkId posn str }
   
    .                                   {\posn str-> buildError posn str}


{ 

type LexerContent = Either Err.TokenError Tk.ContextToken

-- Given a Token, it's context information and the related string
-- Creates the proper tokenized ouput.
buildToken :: Tk.Token -> AlexPosn -> String -> LexerContent 
buildToken espTk (AlexPn _ r c) str = Right tk
    where 
        tk = Tk.CtxToken {
            Tk.position      = Tk.Pos (r,c),
            Tk.string        = str,
            Tk.stringContent = chooseContent espTk str,
            Tk.tk            = espTk 
        }

-- Given a Token and a string, it propperly assigns context to Token that require it.
chooseContent :: Tk.Token -> String -> Maybe Tk.Content 
chooseContent token string = case token of 
    Tk.TkInteger -> Just $ Tk.Integer (read string :: Int)
    Tk.TkTrue    -> Just $ adaptBool string
    Tk.TkFalse   -> Just $ adaptBool string 
    Tk.TkId      -> Just $ Tk.Id string
    _            -> Nothing
    where
        adaptBool bl = let new =  (toUpper $ head bl) : tail bl  
                       in Tk.Bool (read new :: Bool)

-- Handles creation of invalid input.
buildError :: AlexPosn -> String -> LexerContent 
buildError (AlexPn _ r c) str = Left err 
    where
        err = Err.TkErr { 
            Err.name = str,
            Err.pos = Tk.Pos (r,c)
        }

}