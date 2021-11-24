module AST 
( Program(..)
, Instruction (..)
, Type(..)
, ConcreteType (..)
, Expr (..)
, Id
) where

import Data.List (intercalate)

{-
 - Data definitions for the abstract syntax tree 
 -}

data ConcreteType = Bool | Int 

instance Show ConcreteType where 
    show Bool = "bool" 
    show Int = "int"

data Type = Lazy ConcreteType | Concrete ConcreteType 

instance Show Type where
    show (Lazy conc) = "lazy " ++ show conc
    show (Concrete conc) = show conc

type Id = String

data Program = Ins [Instruction] | Ex Expr 

instance Show Program where
    show (Ins xs)  = intercalate "\n" . map show $ xs
    show (Ex expr) = show expr

data Instruction 
   = Inicialization Type Id Expr
   | Assignment Id Expr

instance Show Instruction where
    show (Assignment progId expr)        = "assign(" ++ progId ++ ", " ++ show expr ++ ")"
    show (Inicialization tp progId expr) = "def(" ++ show tp ++ ", " ++ progId ++ ", " ++ show expr ++ ")" 

data Expr
    -- Leafs == Non terminals
    = IntExp Int
    | BoolExp Bool
    | LazyExp Expr

    -- Arithmetical expressions
    | Add Expr Expr
    | Sub Expr Expr
    | Minus Expr
    | Mas Expr
    | Mult Expr Expr
    | Mod Expr Expr
    | Power Expr Expr
    
    -- Relational expressions
    | LessThan Expr Expr
    | LessEqualThan Expr Expr
    | GreaterThan Expr Expr
    | GreaterEqualThan Expr Expr
    | Equal Expr Expr
    | NotEqual Expr Expr

    -- Boolean expressions
    | And Expr Expr
    | Or Expr Expr
    | Not Expr

    -- Miscellaneus
    | Parentheses Expr
    | Identifier Id
    | Function Id [Expr]

instance Show Expr where
    show (IntExp num)               = show num
    show (BoolExp val)              = show val
    show (LazyExp expr)             = "lazyExpr( " ++ show expr ++ " )" 

    show (Add lse rse)              = showBinOp lse rse "+"
    show (Sub lse rse)              = showBinOp lse rse "-"
    show (Minus expr)               = showUnOp expr "-"
    show (Mas expr)                 = showUnOp expr "+"
    show (Mult lse rse)             = showBinOp lse rse "*"
    show (Mod lse rse)              = showBinOp lse rse "%"
    show (Power lse rse)            = showBinOp lse rse "^"

    show (LessThan lse rse)         = showBinOp lse rse "<"
    show (LessEqualThan lse rse)    = showBinOp lse rse "<="
    show (GreaterThan lse rse)      = showBinOp lse rse ">"
    show (GreaterEqualThan lse rse) = showBinOp lse rse ">="
    show (Equal lse rse)            = showBinOp lse rse "="
    show (NotEqual lse rse)         = showBinOp lse rse "<>"
    show (And lse rse)              = showBinOp lse rse "&&"
    show (Or lse rse)               = showBinOp lse rse "||"
    show (Not expr)                 = showUnOp expr "!"

    show (Parentheses expr)         = show expr 
    show (Identifier name)          = name 
    show (Function name exprs)       = name ++ "( " ++ (intercalate ", " . map show $ exprs) ++ " )"

showBinOp :: Expr -> Expr -> String -> String
showBinOp left right op = op ++ "(" ++ show left ++ ", " ++ show right ++ ")"

showUnOp :: Expr -> String -> String
showUnOp expr op = op ++ show expr