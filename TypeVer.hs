module TypeVer 
( validateProgram
, validateInstruction
, validateExpr
)where

{-
 - Module for propper type validation
 - 
 - Type comparison is defined with the behaviour of structural type equality as defined in the 
 - Eq instance for 'Type' in AST.
 - 
 - There is a relaxed type equality in assignment of lazy expressions since a promise of a int  (for example)
 - can also be an int.
 -}

import Control.Monad.State
import Data.List (intercalate)
import Data.Maybe (isJust, fromJust)
import System.IO (hPutStrLn, stderr)

import SymTable as ST
import AST 
import qualified BackEnd as BE  

-- | Validating a program handles either proper type verification for an action : the actions happens or not, 
-- and type validation for an expression, it has a well defined type, or not.
validateProgram :: Program -> BE.GlobalState (Either [Bool] (Maybe Type))
validateProgram imp@Ins{} = do 
    resultList <- mapM validateInstruction (list imp) 
    return $ Left resultList
validateProgram imp@Ex{} = Right <$> validateExpr (expr imp) 

-- | Instructions type validation.
validateInstruction :: Instruction -> BE.GlobalState Bool
validateInstruction imp@Inicialization{} = do 
    ustate <- get
    check <- BE.symbolDefinedST (initId imp)     

    if check then do 
        let errorMsg = (BE.inputLine ustate) ++ ". Symbol for inicialization already defined. Related to inicialization at column " 
                       ++ show (getPosition imp) ++ ". Ignoring type validation."

        reportTypeError errorMsg
        return False

        else do
            expressionType <- validateExpr (initExpr imp)
     
            case expressionType of 
                Nothing ->  do
                    let errorMsg = (BE.inputLine ustate) ++ ". Invalid inicialization types. Related to inicialization at column" 
                                   ++ show  (getPosition imp) ++ ". Found type " 
                                   ++ show  (initType imp) ++ " and a type error for the expression. "
                    
                    reportTypeError errorMsg
                    return False
                Just aType -> if initType imp `relaxedTypeEquality` aType  then do
     
                        let newSymbolContext = ST.Context { 
                            ST.symbolType    = Just (initType imp),
                            ST.symbolContent = Nothing
                        }
     
                        BE.insertSymbolST (initId imp) newSymbolContext 
     
                        return True
                    
                    else do 
                        let errorMsg = (BE.inputLine ustate) ++ ". Invalid inicialization types. Related to inicialization at column " 
                                        ++ show (getPosition imp) ++ ". Found types " 
                                        ++ show (initType imp) ++ " and " ++ show aType 
                                        ++ " , but expected equal types."
     
                        
                        reportTypeError errorMsg
                        return False
validateInstruction imp@Assignment{} = do
    ustate <- get
    check <- BE.symbolDefinedST (assignId imp)     

    if check then do 
        result <- BE.getSymbolTypeST (assignId imp)

        case result of 
            Left errorMsg -> do 

                reportTypeError errorMsg
                return False

            Right Nothing -> do 

                let errorMsg = (BE.inputLine ustate) ++ ". A function cannot be assigned. Related to assignment at column " ++ show (getPosition imp) 
                               ++ "."
                            
                reportTypeError errorMsg
                return False 

            Right (Just aSymbolType) -> do 
                
                resultExpr <- validateExpr (assignExpr imp)

                case resultExpr of 
                    Nothing ->  do
                        let errorMsg = (BE.inputLine ustate) ++ ". Invalid assignment types. Related to assignment at column" 
                                       ++ show  (getPosition imp) ++ ". Found type " 
                                       ++ show aSymbolType ++ " and a type error for the assignment expression. "
                        
                        reportTypeError errorMsg
                        return False
                    Just expressionType -> 
                        if aSymbolType `relaxedTypeEquality` expressionType then return True 
                        else do 
                            let errorMsg = (BE.inputLine ustate) ++ ". Invalid Assignment. Related to assignment at column" 
                                           ++ show (getPosition imp) ++ ". Variable '" 
                                           ++ assignId imp ++ "' has not been defined."

                            reportTypeError errorMsg
                            return False

    else do
        let errorMsg = (BE.inputLine ustate) ++ ". Invalid assignment. Related to assignment at column " ++ show (getPosition imp) 
                       ++ ". Symbol '" ++ assignId imp ++ "' has not been defined. Ignoring type validation."

        reportTypeError errorMsg
        return False

-- | Expression type validation.
validateExpr :: Expr -> BE.GlobalState (Maybe Type)
validateExpr imp@Identifier {} = do 
    result <- BE.getSymbolTypeST (idName imp) 
    case result of 
        Left errorMsg            -> do  
            reportTypeError errorMsg
            return Nothing
        Right Nothing            -> do 
            let errorMessage = "Invalid name for an identifier. Identifiers cannot have names already given to "
                                ++ "predefined functions. Related to identifier at column " ++ show (idPos imp) ++ "."
            reportTypeError errorMessage
            return Nothing
        Right (Just Lazy{tp=something}) -> return $ Just dummyReturnInt{tp=something}
        Right someType           -> return someType
        
validateExpr imp@Function {} = do 
    check <- BE.symbolDefinedST (functionName imp)

    if check then do 
        case functionName imp of 
            "if"      -> case functionArguments imp of 
                [boolExpr, sucExpr, failExpr] -> do 
                    a <- validateExpr boolExpr
                    b <- validateExpr sucExpr
                    c <- validateExpr failExpr

                    case (a,b,c) of 
                        (Just typeForCondition, Just typeForSuccess, Just typeForFailure) -> do

                            if typeForCondition == dummyReturnBool then 

                                if typeForSuccess == typeForFailure then  return (Just typeForSuccess)
                                else do
                                    let errorMsg =  "Types for if alternative must be equal. Found '" 
                                                    ++ show typeForSuccess ++ "' and " ++ show typeForFailure 
                                                    ++ ". Related to function at column " ++ show (functionPos imp)
                                    reportTypeError errorMsg
                                    return Nothing

                            else do 
                                let errorMsg = "Expected type for if expression is Bool. Found '" 
                                                ++ show typeForCondition ++ ". Related to function at column " 
                                                ++ show (functionPos imp) 
                                reportTypeError errorMsg
                                return Nothing

                        _                              -> return Nothing
                _ -> reportInvalidNArgs 3 (functionPos imp) "if"
            "type"    -> case functionArguments imp of 
                [expression] -> do 
                    result <- validateExpr expression

                    case result of
                        Nothing -> return Nothing
                        Just Lazy {tp=someType} -> return $ Just $ dummyReturnInt{tp=someType}
                        Just something          -> return $ Just something
                _ -> reportInvalidNArgs 1 (functionPos imp) "type" 

            "ltype"   -> case functionArguments imp of
                [idElem@Identifier{}] -> do 
                    checkSym <- BE.symbolDefinedST (idName idElem) 

                    if checkSym then do
                        result <- BE.getSymbolTypeST (idName idElem) 
                        case result of 
                            Left errorMsg -> do  
                                reportTypeError errorMsg
                                return Nothing
                            Right Nothing -> do 
                                let errorMessage = "Invalid identifier. '" ++ idName idElem ++ " 'ltype' function "
                                                    ++ " requires an existing variable name to provide it's type "
                                                    ++ " information, but insteand found a predefined function name. "
                                                    ++ " Related to function at column " ++ show (functionPos imp) ++"."
                                reportTypeError errorMessage
                                return Nothing
                            Right justSomeType -> return justSomeType
 
                    else do
                        let errorMsg = "Undefined symbol '" ++ idName idElem ++ "'. 'ltype' function requires an "
                                       ++ "existing variable name to provide it's type information . Related to "
                                       ++ "function at column " ++ show (functionPos imp) ++ "."

                        reportTypeError errorMsg
                        return Nothing


                _ -> do
                    let errorMsg = "Invalid Arguments. 'ltype' functions requires a single existing variable name."
                                    ++ "Related to function at column " ++ show (functionPos imp) 
                    reportTypeError errorMsg
                    return Nothing

            "cvalue"  -> case functionArguments imp of 
                [idElem@Identifier{}] -> do 
                    checkSym <- BE.symbolDefinedST (idName idElem) 

                    if checkSym then do
                        result <- BE.getSymbolTypeST (idName idElem) 
                        case result of 
                            Left errorMsg -> do  
                                reportTypeError errorMsg
                                return Nothing
                            Right Nothing -> do 
                                let errorMessage = "Invalid identifier. '" ++ idName idElem ++ " 'ctype' function "
                                                    ++ " requires an existing variable name to provide it's content "
                                                    ++ " but insteand found a predefined function name. "
                                                    ++ " Related to function at column " ++ show (functionPos imp) ++"."
                                reportTypeError errorMessage
                                return Nothing
                            Right justSomeType -> return justSomeType
 
                    else do
                        let errorMsg = "Undefined symbol '" ++ idName idElem ++ "'. 'ctype' function requires an "
                                       ++ "existing variable name to provide it's content . Related to "
                                       ++ "function at column " ++ show (functionPos imp) ++ "."

                        reportTypeError errorMsg
                        return Nothing
                [_] -> do 
                    let errorMsg = "Invalid use. 'cvalue' expects a variable as an argument. Related to function "
                                    ++ " at column " ++ show (functionPos imp) ++ "."

                    reportTypeError errorMsg
                    return Nothing
                _ -> reportInvalidNArgs 1 (functionPos imp) "cvalue"

            "reset"   -> case functionArguments imp of 

                [] -> return $ Just (dummyReturnBool )
                _ -> reportInvalidNArgs 0 (functionPos imp) "reset"

            "irandom" -> case functionArguments imp of 

                [expression] -> do 

                    result <- validateExpr expression 
                    case result of 
                        Nothing                  -> return Nothing
                        Just Concrete {tp=Int{}} -> return $ Just (dummyReturnInt)
                        Just other               -> reportInvalidFunctionArgs (functionPos imp) "irandom" [dummyReturnInt] [other]
                _ -> reportInvalidNArgs 1 (functionPos imp) "irandom"

            "fibo"    -> case functionArguments imp of 

                [exprN] -> do 

                    result <- validateExpr exprN

                    let intTypeResult = Just dummyReturnInt 

                    case result of  
                        (Just Concrete {tp = Int{}}) -> return intTypeResult
                        Just bad                              -> do
                            let goodType = [ dummyReturnInt ]

                            reportInvalidFunctionArgs (getPosition imp) "fibo" goodType [bad]
                        _                                     -> return Nothing
                _ -> reportInvalidNArgs 1 (getPosition imp) "fibo"

            "gcd"     -> case functionArguments imp of 

                [exprN, exprM] -> do
                    a <- validateExpr exprN 
                    b <- validateExpr exprM

                    let intTypeResult = Just dummyReturnInt 

                    case (a,b) of 
                        (Just Concrete {tp = Int{}}, Just Concrete {tp = Int {}} ) -> return intTypeResult
                        (Just bad1, Just bad2)                                                -> do
                            let goodTypes = [ dummyReturnInt, dummyReturnInt ]

                            reportInvalidFunctionArgs (functionPos imp) "gcd" goodTypes [bad1,bad2]
                        (_, _)                                                                -> return Nothing
                _ -> reportInvalidNArgs 2 (functionPos imp) "gcd"

            "now"     -> case functionArguments imp of 
                [] -> return $ Just dummyReturnInt
                _ -> reportInvalidNArgs 0 (functionPos imp) "now"
            _ -> undefined

        else do 
            let errorMsg = functionName imp ++ " is not a Dyslexio reckognized function."
            reportTypeError errorMsg 
            return Nothing
validateExpr IntExp{}  = return $ Just dummyReturnInt 
validateExpr BoolExp{} = return $ Just dummyReturnBool 
validateExpr imp@LazyExp{} = do 
    inner <- validateExpr (lazyVal imp)
    case inner of 
        Nothing                          -> return Nothing
        Just Concrete{tp=typeOfInterest} -> return $ Just dummyReturnLazyInt{tp=typeOfInterest} 
        toReturn@(Just Lazy{})           -> return toReturn
validateExpr imp@Add{} = do 
    lse <- validateExpr $ lhs imp
    rse <- validateExpr $ rhs imp

    validateArithmetic 2 (addPos imp) "+" [lse,rse]
validateExpr imp@Sub{} = do 
    lse <- validateExpr $ lhs imp
    rse <- validateExpr $ rhs imp

    validateArithmetic 2 (subPos imp) "-" [lse,rse]
validateExpr imp@Minus{} = do 
    expression <- validateExpr $ minusVal imp

    validateArithmetic 1 (subPos imp) "Unary -" [expression]
validateExpr imp@Mas{} = do 
    expression <- validateExpr $ masVal imp

    validateArithmetic 1 (masPos imp) "Unary +" [expression]
validateExpr imp@Mult{} = do 
    lse <- validateExpr $ lhs imp
    rse <- validateExpr $ rhs imp

    validateArithmetic 2 (multPos imp) "*" [lse,rse]
validateExpr imp@Mod{} = do
    lse <- validateExpr $ lhs imp
    rse <- validateExpr $ rhs imp

    validateArithmetic 2 (modPos imp) "%" [lse,rse]
validateExpr imp@Power{} = do
    lse <- validateExpr $ lhs imp
    rse <- validateExpr $ rhs imp

    validateArithmetic 2 (powerPos imp) "^" [lse,rse]
validateExpr imp@LessThan{} = do
    lse <- validateExpr $ lhs imp
    rse <- validateExpr $ rhs imp

    validateRelational 2 (ltPos imp) "<" [lse,rse]
validateExpr imp@LessEqualThan{} = do
    lse <- validateExpr $ lhs imp
    rse <- validateExpr $ rhs imp

    validateRelational 2 (letPos imp) "<=" [lse,rse]
validateExpr imp@GreaterThan{} = do
    lse <- validateExpr $ lhs imp
    rse <- validateExpr $ rhs imp

    validateRelational 2 (gtPos imp) ">" [lse,rse]
validateExpr imp@GreaterEqualThan{} = do
    lse <- validateExpr $ lhs imp
    rse <- validateExpr $ rhs imp

    validateRelational 2 (geqPos imp) ">=" [lse,rse]
validateExpr imp@Equal{} = do
    lse <- validateExpr $ lhs imp
    rse <- validateExpr $ rhs imp

    if isJust lse && isJust rse then do
        if lse == rse then return lse
        else do 
            let errorMsg = "Invalid types. Equality expects impents of equal type, but types differ."
                           ++ "At column " ++ show (getPosition imp) ++ "."
            reportTypeError errorMsg
            return Nothing
                
    else return Nothing
validateExpr imp@NotEqual{} = do
    lse <- validateExpr $ lhs imp
    rse <- validateExpr $ rhs imp

    if isJust lse && isJust rse then do
        if lse == rse then return lse
        else do 
            let errorMsg = "Invalid types. Inequality expects impents of equal type, but types differ."
                           ++ "At column " ++ show (getPosition imp) ++ "."
            reportTypeError errorMsg
            return Nothing
                
    else return Nothing
validateExpr imp@And{} = do 
    lse <- validateExpr $ lhs imp
    rse <- validateExpr $ rhs imp

    validateBoolean 2 (getPosition imp) "&&" [lse,rse]
validateExpr imp@Or{} = do 
    lse <- validateExpr $ lhs imp
    rse <- validateExpr $ rhs imp

    validateBoolean 2 (getPosition imp) "||" [lse,rse]
validateExpr imp@Not{} = do 
    expression <- validateExpr $ notVal imp

    validateBoolean 1 (getPosition imp) "!" [expression]
validateExpr imp@Parentheses{} = validateExpr (parenthVal imp)
        

{- Type validation helpers -}

-- | Encapsulated type validation for onto operations.
validateOntoOperator :: Type -> Int -> Int -> String -> [Maybe Type] -> BE.GlobalState (Maybe Type)
validateOntoOperator _ _ _ _ [] = error "Dyslexio. This shouldn't happen."
validateOntoOperator interestingType nArgs errorPos opId xs = 

    if all isJust xs then do
        if all (== Just interestingType) xs then return $ Just interestingType
        else do 
            let expectedTypes = replicate nArgs interestingType 
                actualTypes   = map fromJust xs 

            reportInvalidOpTypes errorPos opId expectedTypes actualTypes 
    else return Nothing

-- | Encapsulated type validation for arithmetic operations.
validateArithmetic :: Int -> Int -> String -> [Maybe Type] -> BE.GlobalState (Maybe Type)       
validateArithmetic = validateOntoOperator dummyReturnInt

-- | Encapsulate type validation for boolean operations.
validateBoolean :: Int -> Int -> String -> [Maybe Type] -> BE.GlobalState (Maybe Type) 
validateBoolean = validateOntoOperator dummyReturnBool

-- | Encapsulated type validation for relational operations.
validateRelational :: Int -> Int -> String -> [Maybe Type] -> BE.GlobalState (Maybe Type)       
validateRelational _ _ _ [] = error "Dyslexio. This shouldn't happen."
validateRelational nArgs errorPos opId xs = do 

    let relType = dummyReturnBool 
        argType = dummyReturnInt 

    if all isJust xs then do
        if all (== Just argType) xs then return $ Just relType
        else do 
            let expectedTypes = replicate nArgs argType
                actualTypes   = map fromJust xs 

            reportInvalidOpTypes errorPos opId expectedTypes actualTypes 
    else return Nothing

{- This function is defined to address the "type equality" required for assignment and inicialization. 
 - where a lazy T can hold either a value of type T or an expression that would eventually be T
 -}
relaxedTypeEquality :: Type -> Type -> Bool
lse `relaxedTypeEquality` rse = case lse of 
    Lazy{} -> tp lse == tp rse 
    _      -> lse == rse 


{- Error report Helpers -}

-- | Generic type error report.
reportTypeError :: String -> BE.GlobalState() 
reportTypeError msg = lift $ hPutStrLn stderr (tpPrefix ++ msg)
    where tpPrefix = "Error: "

-- | Report for invalid number of arguments of a predefined function.
reportInvalidNArgs :: Int -> Int -> String -> BE.GlobalState (Maybe Type)
reportInvalidNArgs nArgs errorPos foo = do 
    let errorMsg = "Invalid number of arguments. '" ++ foo ++ "' requires "++ show nArgs ++" arguments. Related to function"
                    ++ " at column " ++ show errorPos
    reportTypeError errorMsg
    return Nothing

-- | Report of invalid type of arguments of a predefined function.
reportInvalidFunctionArgs :: Int -> String -> [Type] -> [Type] -> BE.GlobalState (Maybe Type) 
reportInvalidFunctionArgs errorPos foo expectedTypes actualTypes = do 
    let errorMsg = "Invalid types for function. '" ++ foo ++ "' expects types " ++ intercalate ", " (map show expectedTypes) 
                   ++ " but found types " ++ intercalate ", " (map show actualTypes) ++ ". Related to function at" 
                   ++ " column " ++ show errorPos

    reportTypeError errorMsg
    return Nothing

-- | Report of invalid arguments for an operator.
reportInvalidOpTypes :: Int -> String -> [Type] -> [Type] -> BE.GlobalState (Maybe Type)
reportInvalidOpTypes errorPos op expectedTypes actualTypes = do 
    let errorMsg = "Invalid types for '" ++ op ++ "'. Expected " ++ intercalate ", " (map show expectedTypes) 
                   ++ " but found " ++ intercalate ", " (map show actualTypes) ++ " At column " ++ show errorPos  ++ "."

    reportTypeError errorMsg
    return Nothing

{- Constants -}

-- Used to avoid compiler warnings of some type returned.
dummyReturnInt , dummyReturnBool, dummyReturnLazyInt :: Type 
dummyReturnInt = Concrete {
    tp=Int{ intTypePos = dummyPosition },
    concreteTypePos = dummyPosition
    }
dummyReturnBool = Concrete {
    tp=Bool{ boolTypePos = dummyPosition }, 
    concreteTypePos = dummyPosition
    }

dummyReturnLazyInt = Lazy { 
    tp = Int{ intTypePos = dummyPosition }, 
    lazyTypePos = dummyPosition
}

dummyPosition :: Int
dummyPosition = (-1)