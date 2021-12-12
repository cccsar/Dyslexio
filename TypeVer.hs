module TypeVer where

{-
 - Module for propper type validation
 -}

import Control.Monad.State

import SymTable as ST
import AST 
import qualified BackEnd as BE  


validateInstruction :: Instruction -> BE.GlobalState Bool
validateInstruction elem@Inicialization{} = do 

    check <- BE.symbolDefinedST (initId elem)     

    if check then do 
        let errorMsg = "Symbol for inicialization already defined. Related to inicialization at column " 
                       ++ show (getPosition elem) ++ ". Ignoring type validation."

        lift $ putStrLn errorMsg
        -- insertError errorMsg ###
        return False

        else do
            expressionType <- validateExpr (initExpr elem)
     
            case expressionType of 
                Nothing ->  do
                    let errorMsg = "Invalid inicialization types. Related to inicialization at column" 
                                   ++ show  (getPosition elem) ++ ". Found type " 
                                   ++ show  (initType elem) ++ " and a type error for the expression. "
                    
                    lift $ putStrLn errorMsg
                    -- insertError errorMsg ###
                    return False
                Just tp -> if tp == (initType elem) then do
     
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
     
                        
                        lift $ putStrLn errorMsg
                        -- insertError errorMsg ###
                        return False

validateInstruction elem@Assignment{} = do
    check <- BE.symbolDefinedST (initId elem)     

    if check then do 
        result <- BE.getSymbolTypeST (assignId elem)

        case result of 
            Nothing -> do 
                let errorMsg = "A function cannot be assigned. Related to assignment at column " ++ show (getPosition elem) 
                               ++ "."

                lift $ putStrLn errorMsg
                -- insertError errorMsg ###
                return False

            Just symbolType -> do 
                resultExpr <- validateExpr (assignExpr elem)

                case resultExpr of 
                    Nothing ->  do
                        let errorMsg = "Invalid assignment types. Related to assignment at column" 
                                       ++ show  (getPosition elem) ++ ". Found type " 
                                       ++ show symbolType ++ " and a type error for the assignment expression. "
                        
                        lift $ putStrLn errorMsg
                        -- insertError errorMsg ###
                        return False
                    Just expressionType -> 
                        if symbolType == expressionType then return True 
                        else do 
                            let errorMsg = "Invalid Assignment. Related to assignment at column" 
                                           ++ show (getPosition elem) ++ ". Variable '" 
                                           ++ assignId elem ++ "' has not been defined."

                            lift $ putStrLn errorMsg
                            -- insertError errorMsg  ###
                            return False

    else do
        let errorMsg = "Invalid assignment. Related to assignment at column " ++ show (getPosition elem) 
                       ++ ". Symbol '" ++ (assignId elem) ++ "' has not been defined. Ignoring type validation."

        lift $ putStrLn errorMsg
        -- insertError errorMsg  ###
        return False


validateExpr :: Expr -> BE.GlobalState (Maybe Type)
validateExpr elem@Identifier {} = BE.getSymbolTypeST (idName elem)
validateExpr elem@Function {} = do 
    check <- BE.symbolDefinedST (functionName elem)

    if check then do 
        case (functionName elem) of 
            "if"      -> case functionArguments elem of 
                [boolExpr, sucExpr, failExpr] -> do 
                    typeForCondition <- validateExpr boolExpr
                    typeForSuccess   <- validateExpr sucExpr
                    typeForFailure   <- validateExpr failExpr

                    case (typeForCondition, typeForSuccess, typeForFailure) of 
                        (Just tp1, Just tp2, Just tp3) -> undefined
                        _                              -> undefined
                _ -> undefined
            "type"    -> case functionArguments elem of 
                [exp] -> undefined
                _ -> undefined
            "ltype"   -> case functionArguments elem of 
                [exp] -> undefined
                _ -> undefined
            "cvalue"  -> case functionArguments elem of 
                [exp] -> undefined
                _ -> undefined
            "reset"   -> case functionArguments elem of 
                [] -> undefined
                _ -> undefined
            "irandom" -> case functionArguments elem of 
                [exp] -> undefined
                _ -> undefined
            "fibo"    -> case functionArguments elem of 
                [exp] -> undefined
                _ -> undefined
            "gcd"     -> case functionArguments elem of 
                [expM, expn] -> undefined
                _ -> undefined
            "now"     -> case functionArguments elem of 
                [] -> undefined
                _ -> undefined
            _ -> undefined
        else do 
            let errorMsg = functionName elem ++ " is not a Dyslexio reckognized function."

            lift $ putStrLn errorMsg 

            return Nothing
