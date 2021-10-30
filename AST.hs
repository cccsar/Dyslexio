module AST 
( Expr (..)
, ConcreteType (..)
, Type(..)
) where

{-
 - Data definitions for the abstract syntax tree 
 -}

data ConcreteType = Bool | Int deriving Show

data Type = Lazy ConcreteType | PseudoType deriving Show

type Identifier = String

data Instruction 
   = Inicialization Type Identifier Expr
   | Assignment Identifier Expr
   | Sequence Instruction Instruction -- ###

instance Show Instruction where
    show (Assignment id expr) = show id ++ " := " ++ show expr
    show (Inicialization tp id expr) = show tp ++ " " ++ show id ++ " := " ++ show expr
    show (Sequence first next) = show first ++ " ;\n" ++ show next

data Expr
    -- Leafs == Non terminals
    = IntExp Int
    | BoolExp Bool
    | LazyExp Expr

    -- Arithmetical expressions
    | Add Expr Expr
    | Sub Expr Expr
    | Minus Expr
    | Mult Expr Expr
    | Div Expr Expr
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

instance Show Expr where
    show (IntExp num)               = show num
    show (BoolExp val)              = show val
    show (LazyExp expr)             = showSourround expr "'" "'"

    show (Add lse rse)              = showBinOp lse rse "+"
    show (Sub lse rse)              = showBinOp lse rse "-"
    show (Minus expr)               = showUnOp expr "-"
    show (Mult lse rse)             = showBinOp lse rse "*"
    show (Div lse rse)              = showBinOp lse rse "/"
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

    show (Parentheses expr)         = showSourround expr "(" ")"

showBinOp :: Expr -> Expr -> String -> String
showBinOp left right op = show left ++ " " ++ op ++ " " ++ show right

showUnOp :: Expr -> String -> String
showUnOp expr op = op ++ show expr

showSourround :: Expr -> String -> String -> String
showSourround expr left right = left ++ " " ++ show expr ++ " " ++ right