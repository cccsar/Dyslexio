{ 
module Lexer where

import qualified Tokens as Tk (Token(..), ContextToken(..), Position(..))
import qualified Error as Err
}

%wrapper "posn"

$digit = 0-9			
$alpha = [a-zA-Z]		

tokens :-
    $white+           ;
                       
    int                                 {\posn str-> Right $ buildToken Tk.TkNum posn str}
    bool                                {\posn str-> Right $ buildToken Tk.TkBool posn str}
                         
    true                                {\posn str-> Right $ buildToken Tk.TkTrue posn str }
    false                               {\posn str-> Right $ buildToken Tk.TkFalse posn str }
    [\-]{0,1}$digit+                    {\posn str-> Right $ buildToken Tk.TkInteger posn str } 
    
    ([$alpha\_]){1,}[$alpha\_$digit]*   {\posn str -> Right $ buildToken Tk.TkId posn str }

    .                                   {\posn str-> Left $ buildError posn str}



{ 

type LexerContent = Either Err.TokenError Tk.ContextToken

buildToken :: Tk.Token -> AlexPosn -> String -> LexerContent 
buildToken espTk (AlexPn _ r c) str = Right tk
    where 
        tk = Tk.CtxToken {
            Tk.position   = Tk.Pos (r,c),
            Tk.string     = str,
            Tk.content    = chooseContent espTk str,
            Tk.tk         = espTk 
        }

-- Selects propper content assignation to token
chooseContent :: TK.Token -> String -> Tk.Content 
chooseContent token string = case token of 
    Tk.TkInteger -> Tk.Integer (read string :: Int)
    Tk.TkTrue    -> Tk.Integer (read string :: Bool)
    Tk.TkFalse   -> Tk.Integer (read string :: Bool)
    Tk.TkId      -> Tk.Id string

buildError :: AlexPosn -> String -> LexerContent 
buildError (AlexPn _ r c) str = Left err 
    where
        err = Err.TkErr { 
            Err.name = str,
            Err.pos = Tk.Pos (r,c)
        }

}