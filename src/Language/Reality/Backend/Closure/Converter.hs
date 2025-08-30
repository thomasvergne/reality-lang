module Language.Reality.Backend.Closure.Converter where

import Control.Monad.Result qualified as M
import Data.Map qualified as Map
import Data.Text qualified as Text
import GHC.IO qualified as IO
import Language.Reality.Backend.Closure.Free qualified as M
import Language.Reality.Backend.Closure.Hoisting qualified as HT
import Language.Reality.Frontend.Typechecker.Checker qualified as TC
import Language.Reality.Frontend.Typechecker.Unification qualified as TC
import Language.Reality.Syntax.HLIR qualified as HLIR

-- | Convert a program to a closure-converted program.
-- | This function takes a list of toplevel nodes, and returns a list of toplevel
-- | nodes with closures converted.
convertProgram ::
    (MonadIO m) => [HLIR.TLIR "toplevel"] -> m [HLIR.TLIR "toplevel"]
convertProgram nodes = HT.hoistLambdas . concat =<< mapM convertSingularNode nodes

-- | Convert a singular HLIR toplevel node.
-- | This function takes a toplevel node, and returns a toplevel node with closures
-- | converted.
convertSingularNode ::
    (MonadIO m) => HLIR.TLIR "toplevel" -> m [HLIR.TLIR "toplevel"]
convertSingularNode (HLIR.MkTopFunctionDeclaration name params returnType body) = do
    let funType = HLIR.MkTyFun (map (.typeValue) params) returnType

    modifyIORef' globals (Map.insert name.name funType)

    oldLocals <- readIORef locals

    modifyIORef' locals (<> Map.fromList (map HLIR.unannotate params))

    (newBody, ns, newReturnType) <- convertExpression body

    writeIORef locals oldLocals
    let newFunType = HLIR.MkTyFun (map (.typeValue) params) newReturnType

    modifyIORef' globals (Map.insert name.name newFunType)

    pure
        ( ns
            ++ [ HLIR.MkTopFunctionDeclaration
                    { HLIR.name = name
                    , HLIR.parameters = params
                    , HLIR.returnType = newReturnType
                    , HLIR.body = newBody
                    }
               ]
        )
convertSingularNode (HLIR.MkTopConstantDeclaration name expr) = do
    (newExpr, ns, exprType) <- convertExpression expr
    modifyIORef' globals (Map.insert name.name exprType)
    pure (ns ++ [HLIR.MkTopConstantDeclaration name newExpr])
convertSingularNode (HLIR.MkTopLocated p e) = do
    ns <- convertSingularNode e
    pure (map (HLIR.MkTopLocated p) ns)
convertSingularNode (HLIR.MkTopStructureDeclaration name fields) = do
    modifyIORef' structures (Map.insert name.name fields)
    pure [HLIR.MkTopStructureDeclaration name fields]
convertSingularNode (HLIR.MkTopPublic node) = do
    ns <- convertSingularNode node
    pure (map HLIR.MkTopPublic ns)
convertSingularNode (HLIR.MkTopExternalFunction header params returnType) = do
    let funType = HLIR.MkTyFun (map (.typeValue) params) returnType
    modifyIORef' natives (Map.insert header.name funType)
    pure [HLIR.MkTopExternalFunction header params returnType]
convertSingularNode other = pure [other]

-- | Convert an expression to a closure-converted expression.
-- | This function takes an expression, and returns an expression with closures
-- | converted.
convertExpression ::
    (MonadIO m) =>
    HLIR.TLIR "expression" ->
    m (HLIR.TLIR "expression", [HLIR.TLIR "toplevel"], HLIR.Type)
convertExpression (HLIR.MkExprLetIn binding value inExpr _)
    | Just (HLIR.MkExprLambda args returnType body) <- getLambda value = do
        oldLocals <- readIORef locals

        modifyIORef'
            locals
            (<> Map.singleton binding.name binding.typeValue.runIdentity)
        reserved <- readIORef locals
        (newValue, ns1, _) <-
            convertLambda (HLIR.MkExprLambda args returnType body) reserved

        (newInExpr, ns2, retTy) <- convertExpression inExpr

        writeIORef locals oldLocals

        pure
            ( HLIR.MkExprLetIn
                { HLIR.binding = binding
                , HLIR.value = newValue
                , HLIR.inExpr = newInExpr
                , HLIR.returnType = Identity retTy
                }
            , ns1 <> ns2
            , retTy
            )
    | otherwise = do
        (newValue, ns1, _) <- convertExpression value
        (newInExpr, ns2, retTy) <- convertExpression inExpr
        pure
            ( HLIR.MkExprLetIn
                { HLIR.binding = binding
                , HLIR.value = newValue
                , HLIR.inExpr = newInExpr
                , HLIR.returnType = Identity retTy
                }
            , ns1 <> ns2
            , retTy
            )
convertExpression (HLIR.MkExprApplication callee arguments) = do
    nativeFunctions <- readIORef natives
    globalFunctions <- readIORef globals

    let allFunctions = nativeFunctions <> globalFunctions

    (newArguments, nss, argTypes) <- unzip3 <$> mapM convertExpression arguments

    case getVariable callee of
        Just (name, ty) | Just _ <- Map.lookup name allFunctions -> do
            let chosenTy = fromMaybe ty (Map.lookup name globalFunctions)

            let (_, retTy) = case chosenTy of
                    HLIR.MkTyFun args ret -> (args, ret)
                    _ -> M.compilerError "Expected a function type"

            let funTy = argTypes HLIR.:->: retTy

            pure
                ( HLIR.MkExprApplication
                    { callee = HLIR.MkExprVariable (HLIR.MkAnnotation name (Identity funTy)) []
                    , arguments = newArguments
                    }
                , mconcat nss
                , retTy
                )
        _ -> do
            (convertedFunction, ns', calleeType) <- convertExpression callee

            lambdaCallName <- freshSymbol "lambda_call_"
            lambdaStructureName <- freshSymbol "lambda_struct_"

            let structure = HLIR.MkTopStructureDeclaration
                    { HLIR.header = HLIR.MkAnnotation lambdaStructureName []
                    , HLIR.fields = Map.fromList
                        [ ( "function"
                          , HLIR.MkTyFun (HLIR.MkTyPointer (HLIR.MkTyId "void") : argTypes) (case calleeType of
                                HLIR.MkTyFun _ ret -> ret
                                _ -> M.compilerError "Expected a function type")
                          )
                        , ("environment", HLIR.MkTyPointer (HLIR.MkTyId "void"))
                        ]
                    }

            let callVar =
                    HLIR.MkExprDereference (HLIR.MkExprVariable (HLIR.MkAnnotation lambdaCallName (Identity (HLIR.MkTyPointer (HLIR.MkTyId lambdaStructureName)))) []) (Identity (HLIR.MkTyId lambdaStructureName))
                function = HLIR.MkExprStructureAccess callVar "function"
                environment = HLIR.MkExprStructureAccess callVar "environment"

            let call = HLIR.MkExprApplication function (environment : arguments)

            pure
                ( HLIR.MkExprLetIn
                    { HLIR.binding = HLIR.MkAnnotation lambdaCallName (Identity (HLIR.MkTyPointer (HLIR.MkTyId lambdaStructureName)))
                    , HLIR.value = HLIR.MkExprCast convertedFunction (HLIR.MkTyPointer (HLIR.MkTyId lambdaStructureName))
                    , HLIR.inExpr = call
                    , HLIR.returnType = Identity (case calleeType of
                        HLIR.MkTyFun _ ret -> ret
                        _ -> M.compilerError "Expected a function type")
                    }
                , mconcat nss <> ns' <> [structure]
                , case calleeType of
                    HLIR.MkTyId _ -> M.compilerError "Cannot determine return type of lambda call"
                    HLIR.MkTyFun _ ret -> ret
                    _ -> M.compilerError "Expected a function type"
                )
convertExpression (HLIR.MkExprLocated _ e) = convertExpression e
convertExpression e@(HLIR.MkExprVariable (HLIR.MkAnnotation name (Identity ty)) _) = do
    locals' <- readIORef locals
    globals' <- readIORef globals

    let variables = locals' <> globals'

    case Map.lookup name variables of
        Just varTy -> pure (e, [], varTy)
        Nothing -> pure (e, [], ty)
convertExpression e@(HLIR.MkExprLiteral lit) =
    pure
        ( e
        , []
        , case lit of
            HLIR.MkLitInt _ -> HLIR.MkTyId "i32"
            HLIR.MkLitFloat _ -> HLIR.MkTyId "f32"
            HLIR.MkLitBool _ -> HLIR.MkTyId "bool"
            HLIR.MkLitString _ -> HLIR.MkTyPointer HLIR.MkTyChar
            HLIR.MkLitChar _ -> HLIR.MkTyChar
        )
convertExpression e@(HLIR.MkExprLambda{}) = do
    reserved <- readIORef locals
    convertLambda e reserved
convertExpression (HLIR.MkExprCondition cond thenB elseB _) = do
    (newCond, ns1, _) <- convertExpression cond
    (newThen, ns2, thenTy) <- convertExpression thenB
    (newElse, ns3, elseTy) <- convertExpression elseB

    unlessM (isRight <$> runExceptT (TC.isSubtypeOf thenTy elseTy))
        $ M.compilerError "Branches of conditional must have the same type"

    pure
        ( HLIR.MkExprCondition newCond newThen newElse (Identity thenTy)
        , ns1 <> ns2 <> ns3
        , thenTy
        )
convertExpression (HLIR.MkExprStructureAccess struct field) = do
    (newStruct, ns, structTy) <- convertExpression struct

    structures' <- readIORef structures
    let name = TC.getHeader structTy

    case structTy of
        HLIR.MkTyAnonymousStructure _ fields -> case Map.lookup field fields of
            Just ty -> pure (HLIR.MkExprStructureAccess newStruct field, ns, ty)
            Nothing -> M.compilerError $ "Field " <> field <> " does not exist in structure"
        _ -> case name of
            Just structName | Just fields <- Map.lookup structName structures' ->
                case Map.lookup field fields of
                    Just ty -> pure (HLIR.MkExprStructureAccess newStruct field, ns, ty)
                    Nothing ->
                        M.compilerError
                            $ "Field " <> field <> " does not exist in structure " <> structName
            _ -> M.compilerError $ "Expected a structure type: " <> show structTy
convertExpression (HLIR.MkExprStructureCreation ann fields) = do
    (newFields, nss, tys) <- unzip3 <$> mapM convertExpression (Map.elems fields)
    let fieldNames = Map.keys fields
        newFieldMap = Map.fromList (zip fieldNames newFields)
        fieldTypes = Map.fromList (zip fieldNames tys)

    pure
        ( HLIR.MkExprStructureCreation ann newFieldMap
        , mconcat nss
        , HLIR.MkTyAnonymousStructure ann fieldTypes
        )
convertExpression (HLIR.MkExprDereference e _) = do
    (newE, ns, eTy) <- convertExpression e

    case eTy of
        HLIR.MkTyPointer ty -> pure (HLIR.MkExprDereference newE (Identity ty), ns, ty)
        _ -> M.compilerError "Expected a pointer type"
convertExpression (HLIR.MkExprReference e _) = do
    (newE, ns, eTy) <- convertExpression e
    pure (HLIR.MkExprReference newE (Identity (HLIR.MkTyPointer eTy)), ns, HLIR.MkTyPointer eTy)
convertExpression (HLIR.MkExprUpdate update value _) = do
    (newUpdate, ns1, updateTy) <- convertExpression update
    (newValue, ns2, valueTy) <- convertExpression value

    unlessM (isRight <$> runExceptT (TC.isSubtypeOf valueTy updateTy))
        $ M.compilerError "Types of update and value must be the same"

    pure (HLIR.MkExprUpdate newUpdate newValue (Identity updateTy), ns1 <> ns2, updateTy)
convertExpression (HLIR.MkExprSizeOf t) = pure (HLIR.MkExprSizeOf t, [], HLIR.MkTyId "u64")
convertExpression (HLIR.MkExprSingleIf cond thenB _) = do
    (newCond, ns1, _) <- convertExpression cond
    (newThen, ns2, thenTy) <- convertExpression thenB

    pure
        ( HLIR.MkExprSingleIf newCond newThen (Identity thenTy)
        , ns1 <> ns2
        , thenTy
        )
convertExpression (HLIR.MkExprCast e t) = do
    (newE, ns, _) <- convertExpression e
    pure (HLIR.MkExprCast newE t, ns, t)
convertExpression (HLIR.MkExprWhile cond body _ inExpr) = do
    (newCond, ns1, _) <- convertExpression cond
    (newBody, ns2, _) <- convertExpression body
    (newInExpr, ns3, inTy) <- convertExpression inExpr
    pure
        ( HLIR.MkExprWhile newCond newBody (Identity inTy) newInExpr
        , ns1 <> ns2 <> ns3
        , inTy
        )


-- | Convert a lambda expression to a closure-converted expression.
-- | This function takes a lambda expression, and returns an expression with closures
-- | converted.
convertLambda ::
    (MonadIO m) =>
    HLIR.TLIR "expression" ->
    Map Text HLIR.Type ->
    m (HLIR.TLIR "expression", [HLIR.TLIR "toplevel"], HLIR.Type)
convertLambda (HLIR.MkExprLambda args returnType body) reserved = do
    let freeVariablesInBody = M.free body
    nativeFunctions <- readIORef natives
    globalFunctions <- readIORef globals

    let arguments = Map.fromList (map (HLIR.unannotate . (runIdentity <$>)) args)

    let finalNativeFunctions = (nativeFunctions <> globalFunctions) Map.\\ arguments

    let environment = freeVariablesInBody Map.\\ (finalNativeFunctions <> arguments <> reserved)

    environmentStructName <- freshSymbol "closure_env_"
    lambdaStructName <- freshSymbol "closure_fn_"
    environmentName <- freshSymbol "env_"

    let environmentStructure =
            HLIR.MkTopStructureDeclaration
                { HLIR.header = HLIR.MkAnnotation environmentStructName []
                , HLIR.fields = environment
                }
        environmentStructCreation =
            HLIR.MkExprStructureCreation
                { HLIR.annotation = HLIR.MkTyId environmentStructName
                , HLIR.fields =
                    Map.mapWithKey
                        (\name ty -> HLIR.MkExprVariable (HLIR.MkAnnotation name (Identity ty)) [])
                        environment
                }

    let lambdaStructure =
            HLIR.MkTopStructureDeclaration
                { HLIR.header = HLIR.MkAnnotation lambdaStructName []
                , HLIR.fields =
                    Map.fromList
                        [
                            ( "function"
                            , HLIR.MkTyFun
                                (HLIR.MkTyPointer (HLIR.MkTyId environmentStructName) : map (runIdentity . (.typeValue)) args)
                                returnType.runIdentity
                            )
                        , ("environment", HLIR.MkTyPointer (HLIR.MkTyId environmentStructName))
                        ]
                }

    let environmentVar =
            HLIR.MkExprVariable
                (HLIR.MkAnnotation environmentName (Identity (HLIR.MkTyId environmentStructName)))
                []

    oldLocals <- readIORef locals
    modifyIORef' locals (<> arguments)

    (newBody, ns, newReturnType) <- convertExpression body

    writeIORef locals oldLocals

    let newBody' =
            foldr
                ( \(name, ty) acc ->
                    HLIR.MkExprLetIn
                        { HLIR.binding = HLIR.MkAnnotation name (Identity ty)
                        , HLIR.value =
                            HLIR.MkExprStructureAccess
                                { HLIR.structure = HLIR.MkExprDereference environmentVar (Identity (HLIR.MkTyId environmentStructName))
                                , HLIR.field = name
                                }
                        , HLIR.inExpr = acc
                        , HLIR.returnType = Identity newReturnType
                        }
                )
                newBody
                (Map.toList environment)
    let newBodyWithEnvCasting = HLIR.MkExprLetIn (HLIR.MkAnnotation environmentName (Identity (HLIR.MkTyPointer (HLIR.MkTyId environmentStructName))) )
            (HLIR.MkExprCast environmentVar (HLIR.MkTyPointer (HLIR.MkTyId environmentStructName)))
            newBody'
            (Identity newReturnType)

    let lambdaStructureCreation =
            HLIR.MkExprStructureCreation
                { HLIR.annotation = HLIR.MkTyId lambdaStructName
                , HLIR.fields =
                    Map.fromList
                        [
                            ( "function"
                            , HLIR.MkExprLambda
                                { HLIR.parameters =
                                    HLIR.MkAnnotation "env" (Identity (HLIR.MkTyPointer (HLIR.MkTyId environmentStructName)))
                                        : args
                                , HLIR.returnType = Identity newReturnType
                                , HLIR.body = newBodyWithEnvCasting
                                }
                            )
                        , ("environment", HLIR.MkExprReference environmentStructCreation (Identity (HLIR.MkTyPointer (HLIR.MkTyId environmentStructName))) )
                        ]
            }

    pure
        ( HLIR.MkExprCast (HLIR.MkExprReference lambdaStructureCreation (Identity (HLIR.MkTyPointer (HLIR.MkTyId lambdaStructName))) ) (HLIR.MkTyPointer (HLIR.MkTyId "void"))
        , [environmentStructure, lambdaStructure] <> ns
        , HLIR.MkTyPointer (HLIR.MkTyId lambdaStructName)
        )
convertLambda _ _ = M.compilerError "Expected a lambda expression"

-- | Utility variables and types
-- | These are used to generate unique names for closure-converted functions and
-- | their environments.
{-# NOINLINE closureFunctionCounter #-}
closureFunctionCounter :: IORef Int
closureFunctionCounter = IO.unsafePerformIO $ newIORef 0

{-# NOINLINE globals #-}
globals :: IORef (Map Text HLIR.Type)
globals = IO.unsafePerformIO $ newIORef Map.empty

{-# NOINLINE natives #-}
natives :: IORef (Map Text HLIR.Type)
natives = IO.unsafePerformIO $ newIORef Map.empty

{-# NOINLINE locals #-}
locals :: IORef (Map Text HLIR.Type)
locals = IO.unsafePerformIO $ newIORef Map.empty

{-# NOINLINE structures #-}
structures :: IORef (Map Text (Map Text HLIR.Type))
structures = IO.unsafePerformIO $ newIORef Map.empty

freshSymbol :: (MonadIO m) => Text -> m Text
freshSymbol prefix = do
    c <- liftIO $ readIORef closureFunctionCounter
    liftIO $ writeIORef closureFunctionCounter (c + 1)
    pure $ prefix <> Text.pack (show c)

getLambda :: HLIR.TLIR "expression" -> Maybe (HLIR.TLIR "expression")
getLambda (HLIR.MkExprLocated _ e) = getLambda e
getLambda l@(HLIR.MkExprLambda{}) = Just l
getLambda _ = Nothing

getVariable :: HLIR.TLIR "expression" -> Maybe (Text, HLIR.Type)
getVariable (HLIR.MkExprLocated _ e) = getVariable e
getVariable (HLIR.MkExprVariable (HLIR.MkAnnotation name (Identity ty)) []) = Just (name, ty)
getVariable _ = Nothing
