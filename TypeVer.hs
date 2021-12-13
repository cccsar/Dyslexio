module TypeVer 
( validateProgram
, validateInstruction
, validateExpr
)where

{-
 - Module for propper type validation
 -}

import Control.Monad.State
import Data.List (intercalate)
import Data.Maybe (isJust, fromJust)
import System.IO (hPutStrLn, stderr)

import SymTable as ST
import AST 
import qualified BackEnd as BE  

validateProgram :: Program -> BE.GlobalState (Either [Bool] (Maybe Type))
validateProgram elem@Ins{} = do 
    resultList <- mapM validateInstruction (list elem) 
    return $ Left resultList
validateProgram elem@Ex{}  = Right <$> validateExpr (expr elem) 

validateInstruction :: Instruction -> BE.GlobalState Bool
validateInstruction elem@Inicialization{} = do 

    check <- BE.symbolDefinedST (initId elem)     

    if check then do 
        let errorMsg = "Symbol for inicialization already defined. Related to inicialization at column " 
                       ++ show (getPosition elem) ++ ". Ignoring type validation."

        reportTypeError errorMsg
        -- insertError errorMsg ###
        return False

        else do
            expressionType <- validateExpr (initExpr elem)
     
            case expressionType of 
                Nothing ->  do
                    let errorMsg = "Invalid inicialization types. Related to inicialization at column" 
                                   ++ show  (getPosition elem) ++ ". Found type " 
                                   ++ show  (initType elem) ++ " and a type error for the expression. "
                    
                    reportTypeError errorMsg
                    -- insertError errorMsg ###
                    return False
                Just tp -> if (initType elem) `relaxedTypeEquality` tp  then do
     
                        let newSymbolContext = ST.Context { 
                            ST.symbolType = expressionType,
                            ST.symbolContent = Nothing
                        }
     
                        BE.insertSymbolST (initId elem) newSymbolContext 
     
                        return True
                    
                    else do 
                        let errorMsg = "Invalid inicialization types. Related to inicialization at column " 
                                        ++ show (getPosition elem) ++ ". Found types " 
                                        ++ show (initType elem) ++ " and " ++ show expressionType 
                                        ++ " , but expected equal types."
     
                        
                        reportTypeError errorMsg
                        -- insertError errorMsg ###
                        return False

validateInstruction elem@Assignment{} = do
    check <- BE.symbolDefinedST (initId elem)     

    if check then do 
        result <- BE.getSymbolTypeST (assignId elem)

        case result of 
            Left errorMsg -> do 

                reportTypeError errorMsg
                -- insertError errorMsg ###
                return False

            Right Nothing -> do 

                let errorMsg = "A function cannot be assigned. Related to assignment at column " ++ show (getPosition elem) 
                               ++ "."
                            
                reportTypeError errorMsg
                return False 

            Right (Just symbolType) -> do 
                
                resultExpr <- validateExpr (assignExpr elem)

                case resultExpr of 
                    Nothing ->  do
                        let errorMsg = "Invalid assignment types. Related to assignment at column" 
                                       ++ show  (getPosition elem) ++ ". Found type " 
                                       ++ show symbolType ++ " and a type error for the assignment expression. "
                        
                        reportTypeError errorMsg
                        -- insertError errorMsg ###
                        return False
                    Just expressionType -> 
                        if symbolType `relaxedTypeEquality` expressionType then return True 
                        else do 
                            let errorMsg = "Invalid Assignment. Related to assignment at column" 
                                           ++ show (getPosition elem) ++ ". Variable '" 
                                           ++ assignId elem ++ "' has not been defined."

                            reportTypeError errorMsg
                            -- insertError errorMsg  ###
                            return False

    else do
        let errorMsg = "Invalid assignment. Related to assignment at column " ++ show (getPosition elem) 
                       ++ ". Symbol '" ++ (assignId elem) ++ "' has not been defined. Ignoring type validation."

        reportTypeError errorMsg
        -- insertError errorMsg  ###
        return False


validateExpr :: Expr -> BE.GlobalState (Maybe Type)
validateExpr elem@Identifier {} = do 
    result <- BE.getSymbolTypeST (idName elem) 
    case result of 
        Left errorMsg            -> do  
            reportTypeError errorMsg
            return Nothing
        Right Nothing            -> do 
            let errorMessage = "Invalid name for an identifier. Identifiers cannot have names already given to "
                                ++ "predefined functions. Related to identifier at column " ++ show (idPos elem) ++ "."
            reportTypeError errorMessage
            return Nothing
        Right (Just Lazy{tp=something}) -> return $ Just Concrete{tp=something}
        Right someType           -> return someType
        
validateExpr elem@Function {} = do 
    check <- BE.symbolDefinedST (functionName elem)

    if check then do 
        case functionName elem of 
            "if"      -> case functionArguments elem of 
                [boolExpr, sucExpr, failExpr] -> do 
                    a <- validateExpr boolExpr
                    b <- validateExpr sucExpr
                    c <- validateExpr failExpr

                    case (a,b,c) of 
                        (Just typeForCondition, Just typeForSuccess, Just typeForFailure) -> do

                            if typeForCondition == Concrete {tp=Bool{}} then 

                                if typeForSuccess == typeForFailure then  return (Just typeForSuccess)
                                else do
                                    let errorMsg =  "Types for if alternative must be equal. Found '" 
                                                    ++ show typeForSuccess ++ "' and " ++ show typeForFailure 
                                                    ++ ". Related to function at column " ++ show (functionPos elem)
                                    reportTypeError errorMsg
                                    return Nothing

                            else do 
                                let errorMsg = "Expected type for if expression is Bool. Found '" 
                                                ++ show typeForCondition ++ ". Related to function at column " 
                                                ++ show (functionPos elem) 
                                reportTypeError errorMsg
                                return Nothing

                        _                              -> return Nothing
                _ -> reportInvalidNArgs 3 (functionPos elem) "if"
            "type"    -> case functionArguments elem of 
                [expr] -> return Nothing -- ### Decision made since type is a function for reflexivity
                _ -> reportInvalidNArgs 1 (functionPos elem) "type" 

            "ltype"   -> case functionArguments elem of
                [idElem@Identifier{}] -> do 
                    check <- BE.symbolDefinedST (idName idElem) 

                    if check then do
                        result <- BE.getSymbolTypeST (idName idElem) 
                        case result of 
                            Left errorMsg -> do  
                                reportTypeError errorMsg
                                return Nothing
                            Right Nothing -> do 
                                let errorMessage = "Invalid identifier. '" ++ idName idElem ++ " 'ltype' function "
                                                    ++ " requires an existing variable name to provide it's type "
                                                    ++ " information, but insteand found a predefined function name. "
                                                    ++ " Related to function at column " ++ show (functionPos elem) ++"."
                                reportTypeError errorMessage
                                return Nothing
                            Right justSomeType -> return justSomeType
 
                    else do
                        let errorMsg = "Undefined symbol '" ++ idName idElem ++ "'. 'ltype' function requires an "
                                       ++ "existing variable name to provide it's type information . Related to "
                                       ++ "function at column " ++ show (functionPos elem) ++ "."

                        reportTypeError errorMsg
                        return Nothing


                _ -> reportInvalidNArgs 1 (functionPos elem) "ltype"

            "cvalue"  -> case functionArguments elem of 
                [idElem@Identifier{}] -> do 
                    check <- BE.symbolDefinedST (idName idElem) 

                    if check then do
                        result <- BE.getSymbolTypeST (idName idElem) 
                        case result of 
                            Left errorMsg -> do  
                                reportTypeError errorMsg
                                return Nothing
                            Right Nothing -> do 
                                let errorMessage = "Invalid identifier. '" ++ idName idElem ++ " 'ctype' function "
                                                    ++ " requires an existing variable name to provide it's content "
                                                    ++ " but insteand found a predefined function name. "
                                                    ++ " Related to function at column " ++ show (functionPos elem) ++"."
                                reportTypeError errorMessage
                                return Nothing
                            Right justSomeType -> return justSomeType
 
                    else do
                        let errorMsg = "Undefined symbol '" ++ idName idElem ++ "'. 'ctype' function requires an "
                                       ++ "existing variable name to provide it's content . Related to "
                                       ++ "function at column " ++ show (functionPos elem) ++ "."

                        reportTypeError errorMsg
                        return Nothing
                [_] -> do 
                    let errorMsg = "Invalid use. 'cvalue' expects a variable as an argument. Related to function "
                                    ++ " at column " ++ show (functionPos elem) ++ "."

                    reportTypeError errorMsg
                    return Nothing
                _ -> reportInvalidNArgs 1 (functionPos elem) "cvalue"

            "reset"   -> case functionArguments elem of 

                [] -> return $ Just (Concrete {tp = Bool {}} )
                _ -> reportInvalidNArgs 0 (functionPos elem) "reset"

            "irandom" -> case functionArguments elem of 

                [expr] -> do 

                    result <- validateExpr expr 
                    case result of 
                        Nothing                  -> return Nothing
                        Just Concrete {tp=Int{}} -> return $ Just (Concrete {tp=Int{}})
                        Just other               -> reportInvalidFunctionArgs (functionPos elem) "irandom" [Concrete {tp=Int{}}] [other]
                _ -> reportInvalidNArgs 1 (functionPos elem) "irandom"

            "fibo"    -> case functionArguments elem of 

                [exprN] -> do 

                    result <- validateExpr exprN

                    let intTypeResult = Just Concrete {tp = Int{}}

                    case result of  
                        goodResult@(Just Concrete {tp = Int{}}) -> return intTypeResult
                        Just bad                              -> do
                            let goodType = [ Concrete{tp = Int {}}]

                            reportInvalidFunctionArgs (getPosition elem) "fibo" goodType [bad]
                        _                                     -> return Nothing
                _ -> reportInvalidNArgs 1 (getPosition elem) "fibo"

            "gcd"     -> case functionArguments elem of 

                [exprN, exprM] -> do
                    a <- validateExpr exprN 
                    b <- validateExpr exprM

                    let intTypeResult = Just Concrete {tp = Int{}}

                    case (a,b) of 
                        goodResult@(Just Concrete {tp = Int{}}, Just Concrete {tp = Int {}} ) -> return intTypeResult
                        (Just bad1, Just bad2)                                                -> do
                            let goodTypes = [ Concrete{tp = Int {}}, Concrete {tp = Int {}} ]

                            reportInvalidFunctionArgs (functionPos elem) "gcd" goodTypes [bad1,bad2]
                        (_, _)                                                                -> return Nothing
                _ -> reportInvalidNArgs 2 (functionPos elem) "gcd"

            "now"     -> case functionArguments elem of 
                [] -> return $ Just Concrete {tp=Int{}} 
                _ -> reportInvalidNArgs 0 (functionPos elem) "now"
            _ -> undefined

        else do 
            let errorMsg = functionName elem ++ " is not a Dyslexio reckognized function."
            reportTypeError errorMsg 
            return Nothing
validateExpr IntExp{}  = return $ Just Concrete{tp=Int{}}
validateExpr BoolExp{} = return $ Just Concrete{tp=Bool{}}
validateExpr elem@LazyExp{} = do 
    inner <- validateExpr (lazyVal elem)
    case inner of 
        Nothing                          -> return Nothing
        Just Concrete{tp=typeOfInterest} -> return $ Just Lazy {tp=typeOfInterest}
        toReturn@(Just Lazy{})           -> return toReturn
validateExpr elem@Add{} = do 
    lse <- validateExpr $ lhs elem
    rse <- validateExpr $ rhs elem

    validateArithmetic 2 (addPos elem) "+" [lse,rse]
validateExpr elem@Sub{} = do 
    lse <- validateExpr $ lhs elem
    rse <- validateExpr $ rhs elem

    validateArithmetic 2 (subPos elem) "-" [lse,rse]
validateExpr elem@Minus{} = do 
    exp <- validateExpr $ minusVal elem

    validateArithmetic 1 (subPos elem) "Unary -" [exp]
validateExpr elem@Mas{} = do 
    exp <- validateExpr $ masVal elem

    validateArithmetic 1 (masPos elem) "Unary +" [exp]
validateExpr elem@Mult{} = do 
    lse <- validateExpr $ lhs elem
    rse <- validateExpr $ rhs elem

    validateArithmetic 2 (multPos elem) "*" [lse,rse]
validateExpr elem@Mod{} = do
    lse <- validateExpr $ lhs elem
    rse <- validateExpr $ rhs elem

    validateArithmetic 2 (modPos elem) "%" [lse,rse]
validateExpr elem@Power{} = do
    lse <- validateExpr $ lhs elem
    rse <- validateExpr $ rhs elem

    validateArithmetic 2 (powerPos elem) "^" [lse,rse]
validateExpr elem@LessThan{} = do
    lse <- validateExpr $ lhs elem
    rse <- validateExpr $ rhs elem

    validateRelational 2 (ltPos elem) "<" [lse,rse]
validateExpr elem@LessEqualThan{} = do
    lse <- validateExpr $ lhs elem
    rse <- validateExpr $ rhs elem

    validateRelational 2 (letPos elem) "<=" [lse,rse]
validateExpr elem@GreaterThan{} = do
    lse <- validateExpr $ lhs elem
    rse <- validateExpr $ rhs elem

    validateRelational 2 (gtPos elem) ">" [lse,rse]
validateExpr elem@GreaterEqualThan{} = do
    lse <- validateExpr $ lhs elem
    rse <- validateExpr $ rhs elem

    validateRelational 2 (geqPos elem) ">=" [lse,rse]
validateExpr elem@Equal{} = do
    lse <- validateExpr $ lhs elem
    rse <- validateExpr $ rhs elem

    if isJust lse && isJust rse then do
        if (lse == rse) then return lse
        else do 
            let errorMsg = "Invalid types. Equality expects elements of equal type, but types differ."
                           ++ "At column " ++ show (getPosition elem) ++ "."
            reportTypeError errorMsg
            return Nothing
                
    else return Nothing
validateExpr elem@NotEqual{} = do
    lse <- validateExpr $ lhs elem
    rse <- validateExpr $ rhs elem

    if isJust lse && isJust rse then do
        if (lse == rse) then return lse
        else do 
            let errorMsg = "Invalid types. Inequality expects elements of equal type, but types differ."
                           ++ "At column " ++ show (getPosition elem) ++ "."
            reportTypeError errorMsg
            return Nothing
                
    else return Nothing
validateExpr elem@And{} = do 
    lse <- validateExpr $ lhs elem
    rse <- validateExpr $ rhs elem

    validateBoolean 2 (getPosition elem) "&&" [lse,rse]
validateExpr elem@Or{} = do 
    lse <- validateExpr $ lhs elem
    rse <- validateExpr $ rhs elem

    validateBoolean 2 (getPosition elem) "||" [lse,rse]
validateExpr elem@Not{} = do 
    expr <- validateExpr $ lhs elem

    validateBoolean 1 (getPosition elem) "!" [expr]
validateExpr elem@Parentheses{} = validateExpr (parenthVal elem)
        
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
validateArithmetic = validateOntoOperator (Concrete {tp=Int{}})

-- | Encapsulate type validation for boolean operations.
validateBoolean :: Int -> Int -> String -> [Maybe Type] -> BE.GlobalState (Maybe Type) 
validateBoolean = validateOntoOperator (Concrete {tp=Bool{}})

-- | Encapsulated type validation for relational operations.
validateRelational :: Int -> Int -> String -> [Maybe Type] -> BE.GlobalState (Maybe Type)       
validateRelational _ _ _ [] = error "Dyslexio. This shouldn't happen."
validateRelational nArgs errorPos opId xs = do 

    let relType = Concrete {tp=Bool{}}
        argType = Concrete {tp=Int{}}

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
lhs `relaxedTypeEquality` rhs = case lhs of 
    Lazy{} -> tp lhs == tp rhs 
    _      -> lhs == rhs 


{- Error report Helpers -}

reportTypeError :: String -> BE.GlobalState() 
reportTypeError msg = lift $ hPutStrLn stderr (tpPrefix ++ msg)
    where tpPrefix = "--> TypeError: "

reportInvalidNArgs :: Int -> Int -> String -> BE.GlobalState (Maybe Type)
reportInvalidNArgs nArgs errorPos foo = do 
    let errorMsg = "Invalid number of arguments. '" ++ foo ++ "' requires nArgs. Related to function"
                    ++ " at column " ++ show errorPos
    reportTypeError errorMsg
    return Nothing

reportInvalidFunctionArgs :: Int -> String -> [Type] -> [Type] -> BE.GlobalState (Maybe Type) 
reportInvalidFunctionArgs errorPos foo expectedTypes actualTypes = do 
    let errorMsg = "Invalid types for function. '" ++ foo ++ "' expects types " ++ intercalate ", " (map show expectedTypes) 
                   ++ " but found types " ++ intercalate ", " (map show actualTypes) ++ ". Related to function at" 
                   ++ " column " ++ show errorPos

    reportTypeError errorMsg
    return Nothing

reportInvalidOpTypes :: Int -> String -> [Type] -> [Type] -> BE.GlobalState (Maybe Type)
reportInvalidOpTypes errorPos op expectedTypes actualTypes = do 
    let errorMsg = "Invalid types for '" ++ op ++ "'. Expected " ++ intercalate ", " (map show expectedTypes) 
                   ++ " but found " ++ intercalate ", " (map show actualTypes) ++ " At column " ++ show errorPos  ++ "."

    reportTypeError errorMsg
    return Nothing