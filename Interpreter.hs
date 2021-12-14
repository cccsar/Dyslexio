module Interpreter 
( execute
, eval
)where

{-
 - Module for everything concerning evaluation
 -}

import System.IO  (hPutStrLn, stderr)
import Control.Monad.State (lift, get)
import Data.Maybe (fromJust)
import System.Random 
import Data.Time.Clock.POSIX (getPOSIXTime)

import BackEnd
import AST
import SymTable
import TypeVer as TV


-- | Execution of actions.
execute :: Instruction -> GlobalState ()
execute elem@Inicialization{} = do 
    ustate <- get
    
    context <- getSymbolContextST (initId elem) 
    result  <- eval (initExpr elem)

    let extractedContext = fromJust context -- ### Typever pass guarantees that a context would be returned
        newContext = extractedContext { symbolContent = Just result }

    insertSymbolST (initId elem) newContext
execute elem@Assignment{} = do 
    ustate <- get

    context <- getSymbolContextST (assignId elem) 
    result  <- eval (assignExpr elem)

    let extractedContext = fromJust context -- ### Typever pass guarantees that a context would be returned
        newContext = extractedContext { symbolContent = Just result }

    insertSymbolST (initId elem) newContext

-- | Expression evaluation implementaiton.
eval :: Expr -> GlobalState Result
-- Leafs == Non terminals
eval elem@IntExp {}  = return $ INT (intVal elem)
eval elem@BoolExp {} = return $ BOOL (boolVal elem)
eval elem@LazyExp {} = return $ LAZY (lazyVal elem)
-- Arithmetical expressions
eval elem@Add {}   = applyBinOpST "+" (lhs elem) (rhs elem)
eval elem@Sub {}   = applyBinOpST "-" (lhs elem) (rhs elem)
eval elem@Minus {} = applyUnOpST "-" (minusVal elem)
eval elem@Mas {}   = applyUnOpST "+" (masVal elem)
eval elem@Mult {}  = applyBinOpST "*" (lhs elem) (rhs elem) 
eval elem@Mod {}   = applyBinOpST "%" (lhs elem) (rhs elem)
eval elem@Power {} = applyBinOpST "^" (lhs elem) (rhs elem)
-- Relational expressions
eval elem@LessThan {}         = applyBinOpST "<" (lhs elem) (rhs elem) 
eval elem@LessEqualThan {}    = applyBinOpST "<=" (lhs elem) (rhs elem)
eval elem@GreaterThan {}      = applyBinOpST ">" (lhs elem) (rhs elem)
eval elem@GreaterEqualThan {} = applyBinOpST ">=" (lhs elem) (rhs elem)
eval elem@Equal {}            = applyBinOpST "=" (lhs elem) (rhs elem)
eval elem@NotEqual {}         = applyBinOpST "/=" (lhs elem) (rhs elem)
-- Boolean expressions
eval elem@And {} = applyBinOpST "&&" (lhs elem) (rhs elem)
eval elem@Or {}  = applyBinOpST "||" (lhs elem) (rhs elem)
eval elem@Not {} = applyUnOpST "!" (notVal elem)
-- Miscellaneus
eval elem@Parentheses {} = eval (parenthVal elem)
eval elem@Identifier {}  = do 
    result <- getSymbolContentST (idName elem)

    case result of 
        ERROR  -> return ERROR
        result -> return result
        
eval elem@Function {}    = case functionName elem of 
    "if" -> do 
        
        let [cond,success,failure] = functionArguments elem

        preCheck <- eval cond 

        let BOOL check = preCheck

        if check then eval success 
            else eval failure
    "type" -> do 
        let [arg] = functionArguments elem

        result <- TV.validateExpr arg 

        let Just content = result
            actual = tp content

        return $ REFXTYPE actual 
    "ltype" -> do 
        let [elemId@Identifier{}] = functionArguments elem

        result <- getSymbolTypeST (idName elemId) 

        let Right (Just actualType) = result

        return $ LTYPE actualType

    "cvalue" -> do 
        let [elemId@Identifier{}] = functionArguments elem

        getSymbolContentST (idName elemId)

    "reset" -> do 
        resetSymT 
        return (BOOL True)
    "irandom" -> do 

        let [uBound] = functionArguments elem

        result <- eval uBound
        generator <- lift $ newStdGen 

        let (INT n) = result

        return $ INT $ fst $ randomR (0,n-1) generator
    "fibo" -> do
        let [exp] = functionArguments elem

        result <- eval exp

        let (INT n) = result

        fibo n        
    "gcd" -> do
        let [expA,expB] = functionArguments elem

        resultA <- eval expA
        resultB <- eval expB

        let (INT n, INT m) = (resultA, resultB) 

        return $ INT (gcd n m)
    "now" -> do 
        unixTime <- lift $ getPOSIXTime

        return $ (INT $ floor $ toRational unixTime)
    _ -> error "Dyslexio: This shouldn't happen"

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
    el <- eval val 
    return $ applyUnOp op el 

-- | Normal application of a unary operation.
applyUnOp :: String -> Result -> Result
applyUnOp _ ERROR = ERROR
applyUnOp op (INT v) = case op of 
    "+" -> INT v
    "-" -> INT (-v) 
    _   -> error "Dyslexio: This shouldn't happen"
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
            let errorMsg = "Execution error. Power requires an integer as the base and a non negative integer as the exponent." 
            reportExecutionError errorMsg
            return ERROR

    | otherwise = return $ INT (powTr base base exp)
    where
        powTr :: Int -> Int -> Int -> Int
        powTr _ _ 0 = 1
        powTr _ result 1 = result 
        powTr base acc exp = powTr base (acc*base) (exp-1)

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
        myFibo n = myFibo (n-1) + myFibo (n-2)

{- Constants -}

execErrorPrefix = "-->ExecError: "

{- Error report helpers -}

reportExecutionError :: String -> GlobalState () 
reportExecutionError errorMsg = lift $ hPutStrLn stderr (execErrorPrefix ++ errorMsg)