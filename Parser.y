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
%monad { ErrorMonad } { bind } { Ok }

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
%nonassoc '!' NEG
%right '^'

%%

PROGRAM :: { Program } 
    : INSTS { let exprList = reverse $1 in Ins {list = exprList, instructionPos = getPosition (head exprList) } }
    | E     { Ex {expr = $1, exprPos = getPosition $1 } }

INSTS :: { [Instruction] }
        : INST         { [$1] }
        | INSTS INST { $2 : $1 }

INST :: { Instruction }
INST : TP id ':=' E ';' { Inicialization { initType = $1, initId = (getId $2), initExpr = $4, initPos = position $3 } }
     | id ':=' E ';'    { Assignment { assignId = (getId $1), assignExpr = $3, assignPos = position $2 } }

TP :: { Type }
TP : lazy BASETP { Lazy { tp = $2, lazyTypePos = position $1 } } 
   | BASETP      { Concrete {tp = $1, concreteTypePos = getPosition ($1) } }

BASETP :: { ConcreteType }
BASETP : int   { Int { intTypePos = position $1 } }
       | bool  { Bool { boolTypePos = position $1 } }

ES :: { [Expr] } 
    : E           { [$1] }
    | ES ',' E    { $3 : $1 }
               
E :: { Expr }  
E : numLiteral      { let Integer x = fromJust (stringContent $1) in IntExp { intVal = x, intPos = position $1 } }
  | true            { BoolExp { boolVal = True , boolPos = position $1 } }
  | false           { BoolExp { boolVal = False, boolPos = position $1 } }
  | '`' E '`'       { LazyExp { lazyVal = $2 , lazyPos = position $1 } }
                  
  | E '+' E         { Add { lhs = $1 , rhs = $3 , addPos = position $2 } }
  | E '-' E         { Sub { lhs = $1 , rhs = $3 , subPos = position $2 } }
  | '-' E %prec NEG { Minus { minusVal = $2, minusPos = position $1 } }
  | '+' E %prec NEG { Mas { masVal = $2, masPos = position $1} }
  | E '*' E         { Mult { lhs = $1 , rhs = $3, multPos = position $2} }
  | E '%' E         { Mod { lhs = $1 , rhs = $3 , modPos = position $2 } }
  | E '^' E         { Power { lhs = $1 , rhs = $3, powerPos = position $2 } }
                  
  | E '<' E         { LessThan { lhs = $1, rhs = $3, ltPos = position $2 } }
  | E '<=' E        { LessEqualThan { lhs = $1, rhs = $3, letPos = position $2 } }
  | E '>' E         { GreaterThan { lhs = $1, rhs = $3, gtPos = position $2 } }
  | E '>=' E        { GreaterEqualThan { lhs = $1 , rhs = $3 , geqPos = position $2 } }
  | E '=' E         { Equal { lhs = $1, rhs = $3, eqPos = position $2 } }
  | E '<>' E        { NotEqual { lhs = $1, rhs = $3, neqPos = position $2 } }
                  
  | E '&&' E        { And { lhs = $1, rhs = $3 , andPos = position $2 } }
  | E '||' E        { Or { lhs = $1, rhs = $3, orPos = position $2 } }
  | '!' E           { Not { notVal = $2, notPos = position $1 } }
                  
  | id '(' ES ')'   { Function { functionName = (getId $1), functionArguments = (reverse $3), functionPos = position $1 } }
  | '(' E ')'       { Parentheses { parenthVal = $2 , parenthPos = position $1 } }
  | id              { Identifier { idName = (getId $1), idPos = position $1 } }  

{
getId :: ContextToken -> String
getId tk = case fromJust . stringContent $ tk of
    (Id el) -> el

parseError :: [ContextToken] -> ErrorMonad a
parseError [] = Failed "Syntax Error"
parseError (CtxToken { position = p, string = str, tk = token }:xs) = Failed (
        "Syntax error --> related to \"" 
        ++ str ++ "\" at " ++ show p ++ " coming from token: " 
        ++ show token ++ "." 
        )
}