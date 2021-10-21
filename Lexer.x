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
                       
    int               {\posn str-> Right $ buildToken Tk.TkNum posn str}
    bool              {\posn str-> Right $ buildToken Tk.TkBool posn str}
                
    true              {\posn str-> Right $ buildToken Tk.TkTrue posn str }
    false             {\posn str-> Right $ buildToken Tk.TkFalse posn str }
    [\-]{0,1}$digit+  {\posn str-> Right $ buildToken Tk.TkInteger posn str } 
    
    ([$alpha\_]){1,}[$alpha\_$digit]*   {\posn str -> Right $ buildToken Tk.TkId posn str }

    .                 {\posn str-> Left $ buildError posn str}





{ 

buildToken :: Tk.Token -> AlexPosn -> String -> Tk.ContextToken
buildToken espTk (AlexPn _ r c) str = tk
    where 
        tk = Tk.CtxToken {
            Tk.pos = Tk.Pos (r,c),
            Tk.content = str,
            Tk.tk = espTk
        }

buildError :: AlexPosn -> String -> Err.TokenError
buildError (AlexPn _ r c) str = err 
    where
        err = Err.TkErr { 
            Err.name = str,
            Err.pos = Tk.Pos (r,c)
        }

}