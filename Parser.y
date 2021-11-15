{
module Parser where

import Tokens
import AST
import Error

import Data.Maybe (fromJust)

}

%name parse 
%tokentype { ContextToken }
%error { parseError }
%monad { E } { thenE } { returnE }

%token
    int  { CtxToken { tk = TkInt }}
    bool { CtxToken { tk = TkBool }}
    type { CtxToken { tk = TkType}}
    lazy { CtxToken { tk = TkLazy}}
 
    numLiteral { CtxToken { tk = TkNum}}
    true       { CtxToken { tk = TkTrue}}
    false      { CtxToken { tk = TkFalse}}
 
    id { CtxToken { tk = TkId}}
 
    '(' { CtxToken { tk = TkOpenPar}}
    ')' { CtxToken { tk = TkClosePar}}
    '^' { CtxToken { tk = TkPower}}
    '+' { CtxToken { tk = TkPlus}}
    '-' { CtxToken { tk = TkMinus}}
    '!' { CtxToken { tk = TkNot}}
    '*' { CtxToken { tk = TkMult}}
    '%' { CtxToken { tk = TkMod}}

    '<'  { CtxToken { tk = TkLT }}
    '<=' { CtxToken { tk = TkLE }}
    '>'  { CtxToken { tk = TkGT}}
    '>=' { CtxToken { tk = TkGE }}
    '='  { CtxToken { tk = TkEQ }}
    '<>' { CtxToken { tk = TkNE }}

    '&&' { CtxToken { tk = TkAnd}}
    '||' { CtxToken { tk = TkOr}}
 
    '`'   { CtxToken { tk = TkQuote}}
    ','   { CtxToken { tk = TkComma}}
    ':='  { CtxToken { tk = TkAssign}}
    ';'   { CtxToken { tk = TkSemicolon}}
    '=>'  { CtxToken { tk = TkYields}}
    '->'  { CtxToken { tk = TkRArrow}}
    '<-'  { CtxToken { tk = TkLArrow}}
    '{'   { CtxToken { tk = TkOpenBracket}}
    '}'   { CtxToken { tk = TkCloseBracket}}
    '['   { CtxToken { tk = TkOpenBrace}}
    ']'   { CtxToken { tk = TkCloseBrace}}
    '.'   { CtxToken { tk = TkDot}}
    ':'   { CtxToken { tk = TkColon}}
    '::'  { CtxToken { tk = TkColonColon}}
    while { CtxToken { tk = TkWhile}}
    if    { CtxToken { tk = TkIf}}

%left '||'
%left '&&' 
%nonassoc '=' '<>'
%nonassoc '<' '<=' '>' '>='
%left '+' '-'
%left '*' '%'
%nonassoc '!'
%right '^'

%%

PROGRAM :: { Program } 
    : INSTS { Ins (reverse $1) }
    | E     { Ex $1 }

INSTS :: { [Instruction] }
        : INST         { [$1] }
        | INSTS INST { $2 : $1 }

INST :: { Instruction }
INST : TP id ':=' E ';' { Inicialization $1 (getId $2) $4 }
     | id ':=' E ';'    { Assignment (getId $1) $3 }

TP :: { Type }
TP : lazy BASETP { Lazy $2 } 
   | BASETP      { Concrete $1 }

BASETP :: { ConcreteType }
BASETP : int   { Int }
       | bool  { Bool }

ES :: { [Expr] } 
    : E        { [$1] }
    | ES ',' E { $3 : $1 }

E :: { Expr }
E : numLiteral { let Integer x = fromJust (stringContent $1) in IntExp x }
  | true       { BoolExp True }
  | false      { BoolExp False }
  | '`' E '`'  { LazyExp $2 }

  | E '+' E    { Add $1 $3 }
  | E '-' E    { Sub $1 $3 }
  | '-' E      { Minus $2 }
  | E '*' E    { Mult $1 $3 }
  | E '%' E    { Mod $1 $3 }
  | E '^' E    { Power $1 $3 }

  | E '<' E    { LessThan $1 $3 }
  | E '<=' E   { LessEqualThan $1 $3 }
  | E '>' E    { GreaterThan $1 $3 }
  | E '>=' E   { GreaterEqualThan $1 $3 }
  | E '=' E    { Equal $1 $3 }
  | E '<>' E   { NotEqual $1 $3 }

  | E '&&' E   { And $1 $3 }
  | E '||' E   { Or $1 $3 }
  | '!' E      { Not $2 }

  | id '(' ES ')' { Function (getId $1) (reverse $3) }
  | '(' E ')'  { Parentheses $2 }
  | id         { Identifier (getId $1) }  

{
getId :: ContextToken -> String
getId tk = case fromJust . stringContent $ tk of
    (Id el) -> el

parseError _ = failE "Parse Error" 
}