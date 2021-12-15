module Interpreter 
( execute
, eval
)where

{-
 - Module for everything concerning evaluation of expressions and special functions
 -
 - Only runtime errors are addressed here since it is assumed that a program that arrives here passed
 - all it's typechecks. Nevertheless, exit conditions for Dyslexio with context information are placed
 - to detect those unexpected errors.
 - 
 - Most special functions are given the implementations of their equivalents in Haskell.
 -}

import System.IO  (hPutStrLn, stderr)
import Control.Monad.State (lift)
import Data.Maybe (fromJust)
import System.Random (newStdGen,randomR)
import Data.Time.Clock.POSIX (getPOSIXTime)

import BackEnd
import AST
import SymTable
import TypeVer as TV


-- | Execution of actions.
execute :: Instruction -> GlobalState ()
execute imp@Inicialization{} = do 
    context <- getSymbolContextST (initId imp) 
    result  <- eval (initExpr imp)

    let extractedContext = fromJust context -- ### Typever pass guarantees that a context would be returned
        newContext = extractedContext { symbolContent = Just result }

    insertSymbolST (initId imp) newContext
execute imp@Assignment{} = do 
    context <- getSymbolContextST (assignId imp) 
    result  <- eval (assignExpr imp)

    let extractedContext = fromJust context -- ### Typever pass guarantees that a context would be returned
        newContext = extractedContext { symbolContent = Just result }

    insertSymbolST (assignId imp) newContext

-- | Expression evaluation implementaiton.
eval :: Expr -> GlobalState Result
-- Leafs == Non terminals
eval imp@IntExp {}  = return $ INT (intVal imp)
eval imp@BoolExp {} = return $ BOOL (boolVal imp)
eval imp@LazyExp {} = return $ LAZY (lazyVal imp)
-- Arithmetical expressions
eval imp@Add {}   = applyBinOpST "+" (lhs imp) (rhs imp)
eval imp@Sub {}   = applyBinOpST "-" (lhs imp) (rhs imp)
eval imp@Minus {} = applyUnOpST "-" (minusVal imp)
eval imp@Mas {}   = applyUnOpST "+" (masVal imp)
eval imp@Mult {}  = applyBinOpST "*" (lhs imp) (rhs imp) 
eval imp@Mod {}   = applyBinOpST "%" (lhs imp) (rhs imp)
eval imp@Power {} = applyBinOpST "^" (lhs imp) (rhs imp)
-- Relational expressions
eval imp@LessThan {}         = applyBinOpST "<" (lhs imp) (rhs imp) 
eval imp@LessEqualThan {}    = applyBinOpST "<=" (lhs imp) (rhs imp)
eval imp@GreaterThan {}      = applyBinOpST ">" (lhs imp) (rhs imp)
eval imp@GreaterEqualThan {} = applyBinOpST ">=" (lhs imp) (rhs imp)
eval imp@Equal {}            = applyBinOpST "=" (lhs imp) (rhs imp)
eval imp@NotEqual {}         = applyBinOpST "/=" (lhs imp) (rhs imp)
-- Boolean expressions
eval imp@And {} = applyBinOpST "&&" (lhs imp) (rhs imp)
eval imp@Or {}  = applyBinOpST "||" (lhs imp) (rhs imp)
eval imp@Not {} = applyUnOpST "!" (notVal imp)
-- Miscellaneus
eval imp@Parentheses {} = eval (parenthVal imp)
eval imp@Identifier {}  = do 
    result <- getSymbolContentST (idName imp)

    case result of 
        ERROR            -> return ERROR
        LAZY expression  -> eval expression
        resultExpression -> return resultExpression
        
eval imp@Function {}    = case functionName imp of 
    "if" -> do 
        case functionArguments imp of 
            [cond, success, failure] -> do 

                preCheck <- eval cond 
      
                case preCheck of 
                    BOOL check -> if check then eval success 
                                  else eval failure

                    _ -> unexpectedFunctionError "if"
            _ -> unexpectedFunctionError "if"
    "type" -> do 

        case functionArguments imp of 
            [arg] -> do

                result <- TV.validateExpr arg 
      
                case result of 
                    Just content -> return $ REFXTYPE (tp content)
                    _ -> unexpectedFunctionError "type"
            _ -> unexpectedFunctionError "type"
    "ltype" -> do 

        case functionArguments imp of
            [impId@Identifier{}] -> do

                result <- getSymbolTypeST (idName impId) 
                case result of 
                    Right (Just actualType) -> return $ LTYPE actualType
                    _ -> unexpectedFunctionError "ltype"
            _ -> unexpectedFunctionError "ltype"

    "cvalue" -> do 
        case functionArguments imp of 
            [impId@Identifier{}] -> getSymbolContentST (idName impId)
            _ -> unexpectedFunctionError "cvalue"

    "reset" -> do 
        resetSymT 
        return (BOOL True)
    "irandom" -> do 

        case functionArguments imp of 
            [uBound] -> do 

                result <- eval uBound
                generator <- lift $ newStdGen 

                case result of 
                    (INT n) -> return $ INT $ fst $ randomR (0,n-1) generator

                    _ -> unexpectedFunctionError "irandom"
            _ -> unexpectedFunctionError "irandom" 
    "fibo" -> do
        case functionArguments imp of
            [expression] -> do 
                result <- eval expression
      
                case result of 
                    (INT n) -> fibo n        
                    _       -> unexpectedFunctionError "fibo" 
            _ -> unexpectedFunctionError "fibo" 
            
    "gcd" -> do
        case functionArguments imp of 
            [expA, expB] -> do

                resultA <- eval expA
                resultB <- eval expB
      
                case (resultA, resultB) of 
                    (INT n, INT m) -> return $ INT (gcd n m)
                    _              -> unexpectedFunctionError "gcd" 
            _ -> unexpectedFunctionError "gcd" 

    "now" -> do 
        unixTime <- lift $ getPOSIXTime

        return $ (INT $ floor $ toRational unixTime)
    _ -> unexpectedCondition "eval / functions"

-- | FrontEnd to call for the application of a statefull binary operation.
applyBinOpST :: String -> Expr -> Expr -> GlobalState Result
applyBinOpST op lse rse = do 
    lres <- eval lse
    rres <- eval rse 
    
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
    _    -> unexpectedCondition "applyBinOp / relational and integer operators"
applyBinOp op (BOOL l) (BOOL r)  = case op of 
    "&&" -> return $ BOOL (l && r )
    "||" -> return $ BOOL (l || r)
    "="  -> return $ BOOL (l == r) 
    "/=" -> return $ BOOL (l /= r)
    _    -> unexpectedCondition "applyBinOp / boolean operators"
applyBinOp _ _ _ = unexpectedCondition "applyBinOp / catch all"

-- | Application of a known unary operation using a state wrapper.
applyUnOpST :: String -> Expr -> GlobalState Result
applyUnOpST op val = do 
    el <- eval val 
    return $ applyUnOp op el 

-- | Normal application of a unary operation.
applyUnOp :: String -> Result -> Result
applyUnOp _ ERROR = ERROR
applyUnOp op (INT v) = case op of 
    "+" -> INT v
    "-" -> INT (-v) 
    _   -> unexpectedCondition "applyUnOp"
applyUnOp "!" (BOOL v) = BOOL (not v) 
applyUnOp _ _ = unexpectedCondition "applyUnOp"


{- Some languaje specific implementation of functions -}

-- | Language specific mod implementation.
myMod :: Int -> Int -> Int
myMod m n
    | m < n = m 
    | otherwise = myMod (m-n) n

-- | Language specific 'elevation to power' implementation. Since negative exponents are not allowed
-- a state wrapper is used to report the error and propagate it.
myPow :: Int -> Int -> GlobalState Result
myPow bs xp
    | xp < 0   = do 
            let errorMsg = "Execution error. Power requires an integer as the base and a non negative integer as the exponent." 
            reportExecutionError errorMsg
            return ERROR

    | otherwise = return $ INT (powTr bs bs xp)
    where
        powTr :: Int -> Int -> Int -> Int
        powTr _ _ 0 = 1
        powTr _ result 1 = result 
        powTr base acc expo = powTr base (acc*base) (expo-1)

fibo :: Int -> GlobalState Result
fibo n
    | n < 0 = do 
        let errorMsg = "Execution error. Fibonacci requires a possitive integer as argument."
        reportExecutionError errorMsg
        return ERROR
    | otherwise = return $ INT (myFibo n) 
    where
        myFibo :: Int -> Int
        myFibo 0 = 0
        myFibo 1 = 1 
        myFibo m = myFibo (m-1) + myFibo (m-2)

{- Constants -}

execErrorPrefix , unexpectedErrorPrefix :: String
execErrorPrefix = "-->ExecError: "

unexpectedErrorPrefix = "Dyslexio: This Shoulnd't happen. "

{- Error report helpers -}

-- | Report of a runtime error.
reportExecutionError :: String -> GlobalState () 
reportExecutionError errorMsg = lift $ hPutStrLn stderr (execErrorPrefix ++ errorMsg)

-- | Exit message for Dyslexio execution with context information of where it occured.
unexpectedCondition :: String -> a 
unexpectedCondition context = error $ unexpectedErrorPrefix  ++ context

-- | Exit message for Dyslexio execution related to a function unexpected condition.
unexpectedFunctionError :: String -> a
unexpectedFunctionError foo = error $ unexpectedErrorPrefix ++ " Typever guarantees bindings for " ++ foo