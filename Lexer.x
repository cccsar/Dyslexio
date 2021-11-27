{ 
module Lexer where

import qualified Tokens as Tk 
import qualified Error as Err
}

%wrapper "posn"

$digit = 0-9			
$alpha = [a-zA-Z]		

tokens :-
    $white+                             ;

    -- Reserved words                      
    int                                 { buildToken Tk.TkInt }   
    bool                                { buildToken Tk.TkBool }              
    type                                { buildToken Tk.TkType }
    lazy                                { buildToken Tk.TkLazy }
                                         
    -- Constants                        
    $digit+                             { buildToken Tk.TkNum } 
    true                                { buildToken Tk.TkTrue }
    false                               { buildToken Tk.TkFalse }
                                         
    -- Operators                         
    \(                                  { buildToken Tk.TkOpenPar }
    \)                                  { buildToken Tk.TkClosePar }
    \^                                  { buildToken Tk.TkPower }
    \+                                  { buildToken Tk.TkPlus }
    \-                                  { buildToken Tk.TkMinus }
    \!                                  { buildToken Tk.TkNot }
    \*                                  { buildToken Tk.TkMult }
    \%                                  { buildToken Tk.TkMod }
    \<                                  { buildToken Tk.TkLT }
    \<=                                 { buildToken Tk.TkLE }
    \>=                                 { buildToken Tk.TkGE }
    \>                                  { buildToken Tk.TkGT }
    \=                                  { buildToken Tk.TkEQ }
    \<>                                 { buildToken Tk.TkNE }
    \&&                                 { buildToken Tk.TkAnd }
    \|\|                                { buildToken Tk.TkOr }
                                         
    -- Symbols                           
    \'                                  { buildToken Tk.TkQuote }
    \,                                  { buildToken Tk.TkComma }
    \:=                                 { buildToken Tk.TkAssign }
    \;                                  { buildToken Tk.TkSemicolon }
    \=>                                 { buildToken Tk.TkYields }
    \->                                 { buildToken Tk.TkRArrow }
    \<\-                                { buildToken Tk.TkLArrow }
    \[                                  { buildToken Tk.TkOpenBracket }
    \]                                  { buildToken Tk.TkCloseBracket }
    \{                                  { buildToken Tk.TkOpenBrace }
    \}                                  { buildToken Tk.TkCloseBrace }
    \.                                  { buildToken Tk.TkDot }
    \:                                  { buildToken Tk.TkColon }
    \::                                 { buildToken Tk.TkColonColon }
    while                               { buildToken Tk.TkWhile }
    if                                  { buildToken Tk.TkIf }

    -- Ids                               
    ([$alpha\_]){1,}[$alpha\_$digit]*   { buildToken Tk.TkId  }

    .                                   { buildError }

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
    Tk.TkNum     -> Just $ Tk.Integer (read string :: Int)
    Tk.TkId      -> Just $ Tk.Id string
    _            -> Nothing
    
-- Handles creation of invalid input.
buildError :: AlexPosn -> String -> LexerContent 
buildError (AlexPn _ r c) str = Left err 
    where
        err = Err.TkErr { 
            Err.name = str,
            Err.pos = Tk.Pos (r,c)
        }

}