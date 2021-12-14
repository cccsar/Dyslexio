module Eval 
( evalProgram
, evalInstruction
, evalExpr
)where

{-
 - Module for everything concerning evaluation
 -}

import System.IO  (hPutStrLn, stderr)
import Control.Monad.State (lift)

import BackEnd
import AST
import SymTable

evalProgram :: Program -> GlobalState (Either () Result)
evalProgram elem@Ins{} = undefined
evalProgram elem@Ex{} = Right <$>  evalExpr (expr elem) 

evalInstruction :: Instruction -> GlobalState ()
evalInstruction Inicialization{} = undefined
evalInstruction Assignment{} = undefined

-- | Expression evaluation implementaiton.
evalExpr :: Expr -> GlobalState Result
-- Leafs == Non terminals
evalExpr elem@IntExp {}  = return $ INT (intVal elem)
evalExpr elem@BoolExp {} = return $ BOOL (boolVal elem)
evalExpr elem@LazyExp {} = return $ LAZY (lazyVal elem)
-- Arithmetical expressions
evalExpr elem@Add {}   = applyBinOpST "+" (lhs elem) (rhs elem)
evalExpr elem@Sub {}   = applyBinOpST "-" (lhs elem) (rhs elem)
evalExpr elem@Minus {} = applyUnOpST "-" (minusVal elem)
evalExpr elem@Mas {}   = applyUnOpST "+" (masVal elem)
evalExpr elem@Mult {}  = applyBinOpST "*" (lhs elem) (rhs elem) 
evalExpr elem@Mod {}   = applyBinOpST "%" (lhs elem) (rhs elem)
evalExpr elem@Power {} = applyBinOpST "^" (lhs elem) (rhs elem)
-- Relational expressions
evalExpr elem@LessThan {}         = applyBinOpST "<" (lhs elem) (rhs elem) 
evalExpr elem@LessEqualThan {}    = applyBinOpST "<=" (lhs elem) (rhs elem)
evalExpr elem@GreaterThan {}      = applyBinOpST ">" (lhs elem) (rhs elem)
evalExpr elem@GreaterEqualThan {} = applyBinOpST ">=" (lhs elem) (rhs elem)
evalExpr elem@Equal {}            = applyBinOpST "=" (lhs elem) (rhs elem)
evalExpr elem@NotEqual {}         = applyBinOpST "/=" (lhs elem) (rhs elem)
-- Boolean expressions
evalExpr elem@And {} = applyBinOpST "&&" (lhs elem) (rhs elem)
evalExpr elem@Or {}  = applyBinOpST "||" (lhs elem) (rhs elem)
evalExpr elem@Not {} = applyUnOpST "!" (notVal elem)
-- Miscellaneus
evalExpr elem@Parentheses {} = evalExpr (parenthVal elem)
evalExpr elem@Identifier {}  = undefined
evalExpr elem@Function {}    = undefined

-- | FrontEnd to call for the application of a statefull binary operation.
applyBinOpST :: String -> Expr -> Expr -> GlobalState Result
applyBinOpST op lse rse = do 
    lres <- evalExpr lse
    rres <- evalExpr rse 
    
    applyBinOp op lres rres

-- | Application of a known binary operation using a state wrapper.
applyBinOp :: String -> Result -> Result -> GlobalState Result 
applyBinOp _ ERROR _ = return ERROR
applyBinOp _ _ ERROR = return ERROR
applyBinOp op (INT l) (INT r)  = case op of 
    "+"  -> return $ INT (l + r)
    "-"  -> return $ INT (l - r) 
    "*"  -> return $ INT (l * r)
    "%"  -> return $ INT (l `myMod` r)
    "^"  -> myPow l r
    ">"  -> return $ BOOL (l > r) 
    ">=" -> return $ BOOL (l >= r) 
    "<"  -> return $ BOOL (l < r)
    "<=" -> return $ BOOL (l <= r)
    "="  -> return $ BOOL (l == r)
    "/=" -> return $ BOOL (l /= r)
    _    -> error "Dyslexio: This shouldn't happen."
applyBinOp op (BOOL l) (BOOL r)  = case op of 
    "&&" -> return $ BOOL (l && r )
    "||" -> return $ BOOL (l || r)
    "="  -> return $ BOOL (l == r) 
    "/=" -> return $ BOOL (l /= r)
    _    -> error "Dyslexio: This shouldn't happen."
applyBinOp _ _ _ = error "Dyslexio: This shouldn't happen."

-- | Application of a known unary operation using a state wrapper.
applyUnOpST :: String -> Expr -> GlobalState Result
applyUnOpST op val = do 
    el <- evalExpr val 
    return $ applyUnOp op el 

-- | Normal application of a unary operation.
applyUnOp :: String -> Result -> Result
applyUnOp _ ERROR = ERROR
applyUnOp op (INT v) = case op of 
    "+" -> INT v
    "-" -> INT (-v) 
applyUnOp "!" (BOOL v) = BOOL (not v) 
applyUnOp _ _ = error "Dyslexio: This shouldn't happen."


{- Some languaje specific implementation of functions -}

-- | Language specific mod implementation.
myMod :: Int -> Int -> Int
myMod m n
    | m < n = m 
    | otherwise = myMod (m-n) n

-- | Language specific 'elevation to power' implementation. Since negative exponents are not allowed
-- a state wrapper is used to report the error and propagate it.
myPow :: Int -> Int -> GlobalState Result
myPow base exp  
    | exp < 0   = do 
            let errorMsg = "Execution error: Power requires an integer as the base and a non negative integer as the exponent." 
            reportExecutionError errorMsg
            return ERROR

    | otherwise = return $ INT (powTr base base exp)
    where
        powTr :: Int -> Int -> Int -> Int
        powTr _ _ 0 = 1
        powTr _ result 1 = result 
        powTr base acc exp = powTr base (acc*base) (exp-1)


{- Constants -}

execErrorPrefix = "-->ExecError: "

{- Error report helpers -}

reportExecutionError :: String -> GlobalState () 
reportExecutionError errorMsg = do 
    lift $ hPutStrLn stderr (execErrorPrefix ++ errorMsg)