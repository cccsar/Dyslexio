module AST
( Program(..)
, Instruction (..)
, Type(..)
, ConcreteType (..)
, Expr (..)
, Id
, getPosition
) where

import Data.List (intercalate)

{-
 - Data definitions for the abstract syntax tree 
 -}

data ConcreteType
    = Bool  { boolTypePos :: Int }
    | Int   { intTypePos :: Int }

instance Eq ConcreteType where
    Bool{} == Bool{} = True
    Int {} == Int{} = True
    _ == _ = False

instance Show ConcreteType where
    show Bool {} = "bool"
    show Int {} = "int"

data Type
    = Lazy { tp :: ConcreteType, lazyTypePos :: Int }
    | Concrete { tp :: ConcreteType , concreteTypePos :: Int}

instance Eq Type where
    a@Lazy {} == b@Lazy {} = tp a == tp b 
    a@Concrete{} == b@Concrete{} = tp a == tp b
    _ == _ = False

instance Show Type where
    show Lazy {tp = conc} = "lazy " ++ show conc
    show Concrete {tp = conc} = show conc

type Id = String

data Program
    = Ins { list :: [Instruction] , instructionPos :: Int }
    | Ex { expr :: Expr, exprPos :: Int }

instance Show Program where
    show Ins {list = xs}  = intercalate "\n" . map show $ xs
    show Ex {expr = expression} = show expression

data Instruction
   = Inicialization { initType :: Type, initId ::  Id , initExpr :: Expr, initPos :: Int }
   | Assignment { assignId :: Id , assignExpr :: Expr, assignPos :: Int }

instance Show Instruction where
    show Assignment { assignId = progId , assignExpr = expression} = "assign(" ++ progId ++ ", " ++ show expression ++ ")"
    show Inicialization { initType = aType, initId = progId, initExpr = expression} = "def(" ++ show aType ++ ", " ++ progId ++ ", " ++ show expression ++ ")"

data Expr
    -- Leafs == Non terminals
    = IntExp { intVal :: Int, intPos :: Int }
    | BoolExp { boolVal :: Bool, boolPos :: Int }
    | LazyExp { lazyVal :: Expr, lazyPos :: Int }

    -- Arithmetical expressions
    | Add { lhs :: Expr, rhs :: Expr, addPos :: Int }
    | Sub { lhs :: Expr, rhs :: Expr, subPos :: Int }
    | Minus { minusVal :: Expr, minusPos :: Int }
    | Mas { masVal :: Expr , masPos :: Int }
    | Mult { lhs :: Expr, rhs :: Expr, multPos :: Int }
    | Mod { lhs :: Expr, rhs :: Expr, modPos :: Int }
    | Power { lhs :: Expr, rhs :: Expr , powerPos :: Int }

    -- Relational expressions
    | LessThan { lhs :: Expr, rhs :: Expr, ltPos :: Int}
    | LessEqualThan { lhs :: Expr, rhs :: Expr, letPos :: Int}
    | GreaterThan { lhs :: Expr, rhs :: Expr, gtPos :: Int}
    | GreaterEqualThan { lhs :: Expr, rhs :: Expr, geqPos :: Int }
    | Equal { lhs :: Expr, rhs :: Expr, eqPos :: Int }
    | NotEqual { lhs :: Expr, rhs ::  Expr, neqPos :: Int }

    -- Boolean expressions
    | And { lhs :: Expr, rhs :: Expr, andPos :: Int }
    | Or { lhs :: Expr, rhs :: Expr, orPos :: Int }
    | Not { notVal :: Expr, notPos :: Int }

    -- Miscellaneus
    | Parentheses { parenthVal :: Expr, parenthPos :: Int }
    | Identifier { idName :: Id, idPos :: Int }
    | Function { functionName :: Id , functionArguments :: [Expr], functionPos :: Int }

instance Show Expr where
    show IntExp {intVal = num}                   = show num
    show BoolExp {boolVal = val}                 = show val
    show LazyExp {lazyVal = expression}          = "lazyExpr( " ++ show expression ++ " )"

    show Add {lhs = lse, rhs = rse}              = showBinOp lse rse "+"
    show Sub {lhs = lse, rhs = rse}              = showBinOp lse rse "-"
    show Minus {minusVal = expression}           = showUnOp expression "-"
    show Mas {masVal = expression}               = showUnOp expression "+"
    show Mult {lhs = lse, rhs = rse}             = showBinOp lse rse "*"
    show Mod {lhs = lse, rhs = rse}              = showBinOp lse rse "%"
    show Power {lhs = lse, rhs = rse}            = showBinOp lse rse "^"

    show LessThan {lhs = lse, rhs = rse}         = showBinOp lse rse "<"
    show LessEqualThan {lhs = lse, rhs = rse}    = showBinOp lse rse "<="
    show GreaterThan {lhs = lse, rhs = rse}      = showBinOp lse rse ">"
    show GreaterEqualThan {lhs = lse, rhs = rse} = showBinOp lse rse ">="
    show Equal {lhs =lse, rhs = rse}             = showBinOp lse rse "="
    show NotEqual {lhs = lse, rhs = rse}         = showBinOp lse rse "<>"
    show And {lhs = lse, rhs = rse}              = showBinOp lse rse "&&"
    show Or {lhs = lse, rhs = rse}               = showBinOp lse rse "||"
    show Not {notVal = expression}               = showUnOp expression "!"

    show Parentheses {parenthVal = expression}   = show expression
    show Identifier {idName = name}              = name
    show Function {functionName = name, functionArguments = exprs} = name ++ "( " ++ (intercalate ", " . map show $ exprs) ++ " )"

showBinOp :: Expr -> Expr -> String -> String
showBinOp left right op = op ++ "(" ++ show left ++ ", " ++ show right ++ ")"

showUnOp :: Expr -> String -> String
showUnOp expression op = op ++ show expression

class WithPosition a where 
    getPosition :: a -> Int 

instance WithPosition ConcreteType where
    getPosition a@Bool{} = boolTypePos a
    getPosition a@Int{} = intTypePos a

instance WithPosition Type where
    getPosition a@Lazy{} = lazyTypePos a
    getPosition a@Concrete{} = concreteTypePos a

instance WithPosition Instruction where
    getPosition a@Inicialization{} = initPos a
    getPosition a@Assignment{} = initPos a

instance WithPosition Expr where
    getPosition a@IntExp {} = intPos a 
    getPosition a@BoolExp {} = boolPos a
    getPosition a@LazyExp {} = lazyPos a

    getPosition a@Add {} = addPos a
    getPosition a@Sub {} = subPos a
    getPosition a@Minus {} = minusPos a
    getPosition a@Mas {} = masPos a 
    getPosition a@Mult {} = multPos a 
    getPosition a@Mod {} = modPos a 
    getPosition a@Power {} = powerPos a 

    getPosition a@LessThan {} = ltPos a
    getPosition a@LessEqualThan {} = letPos a
    getPosition a@GreaterThan {} = gtPos a 
    getPosition a@GreaterEqualThan {} = geqPos a 
    getPosition a@Equal {} = eqPos a 
    getPosition a@NotEqual {} = neqPos a 
    getPosition a@And {} = andPos a 
    getPosition a@Or {} = orPos a 
    getPosition a@Not {} = notPos a 

    getPosition a@Parentheses {} = parenthPos a 
    getPosition a@Identifier {} = idPos a 
    getPosition a@Function {} = functionPos a 