{-# LANGUAGE LambdaCase #-}

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

{-# NOINLINE structureDeclarations #-}
structureDeclarations :: IORef [HLIR.TLIR "toplevel"]
structureDeclarations = IO.unsafePerformIO $ newIORef []

getGC :: HLIR.TLIR "expression"
getGC =
    HLIR.MkExprApplication
        { HLIR.callee =
            HLIR.MkExprVariable
                ( HLIR.MkAnnotation
                    "get_gc"
                    (Identity (HLIR.MkTyFun [] (HLIR.MkTyPointer HLIR.MkTyChar)))
                )
                []
        , HLIR.arguments = []
        , HLIR.returnType = Identity (HLIR.MkTyPointer HLIR.MkTyChar)
        }

malloc :: HLIR.Type -> HLIR.TLIR "expression"
malloc t =
    HLIR.MkExprApplication
        { HLIR.callee =
            HLIR.MkExprVariable
                ( HLIR.MkAnnotation
                    "gc_malloc"
                    (Identity (HLIR.MkTyFun [HLIR.MkTyId "u64"] (HLIR.MkTyPointer HLIR.MkTyChar)))
                )
                []
        , HLIR.arguments = [getGC, HLIR.MkExprSizeOf t]
        , HLIR.returnType = Identity (HLIR.MkTyPointer HLIR.MkTyChar)
        }

expandStructure :: (MonadIO m) => HLIR.TLIR "toplevel" -> m HLIR.Type
expandStructure (HLIR.MkTopStructureDeclaration name fields) = do
    newFields <- traverse expandType fields

    pure $ HLIR.MkTyAnonymousStructure (HLIR.MkTyId name.name) newFields
expandStructure (HLIR.MkTopLocated _ e) = expandStructure e
expandStructure _ = error "Expected a structure declaration"

expandType :: (MonadIO m) => HLIR.Type -> m HLIR.Type
expandType (HLIR.MkTyAnonymousStructure ann fields) = do
    (newAnn, _) <- convertType ann
    (newFields, _) <- mapAndUnzipM convertType (Map.elems fields)
    let fieldNames = Map.keys fields
        fieldTypes = Map.fromList (zip fieldNames newFields)

    pure $ HLIR.MkTyAnonymousStructure newAnn fieldTypes
expandType (HLIR.MkTyPointer pointee) = HLIR.MkTyPointer <$> expandType pointee
expandType (HLIR.MkTyFun args ret) = do
    newArgs <- mapM expandType args
    newRet <- expandType ret
    pure $ HLIR.MkTyFun newArgs newRet
expandType (HLIR.MkTyId n) = do
    structures' <- readIORef structures
    case Map.lookup n structures' of
        Just fields -> do
            let structDecl = HLIR.MkTopStructureDeclaration (HLIR.MkAnnotation n []) fields
            expandStructure structDecl
        Nothing -> pure $ HLIR.MkTyId n
expandType t = pure t

findM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
findM _ [] = pure Nothing
findM p (x : xs) = do
    r <- p x
    case r of
        Just y -> pure (Just y)
        Nothing -> findM p xs

convertType :: (MonadIO m) => HLIR.Type -> m (HLIR.Type, [HLIR.TLIR "toplevel"])
convertType (HLIR.MkTyFun argTypes returnType) = do
    (newArgTypes, ns1) <- mapAndUnzipM convertType argTypes
    (newReturnType, ns2) <- convertType returnType

    lambdaStructName <- freshSymbol "function_type_"

    let structDecl =
            HLIR.MkTopStructureDeclaration
                { HLIR.header = HLIR.MkAnnotation lambdaStructName []
                , HLIR.fields =
                    Map.fromList
                        [
                            ( "function"
                            , HLIR.MkTyFun (HLIR.MkTyPointer (HLIR.MkTyId "void") : newArgTypes) newReturnType
                            )
                        , ("environment", HLIR.MkTyPointer (HLIR.MkTyId "void"))
                        ]
                }

    f1 <-
        expandStructure structDecl >>= \case
            HLIR.MkTyAnonymousStructure _ f -> pure f
            _ -> pure Map.empty

    found <-
        findM
            ( \node -> do
                (name, f2) <-
                    expandStructure node >>= \case
                        HLIR.MkTyAnonymousStructure name f -> pure (Just name, f)
                        _ -> pure (Nothing, Map.empty)
                if f1 == f2
                    then case name of
                        Just n -> pure (Just n)
                        Nothing -> pure Nothing
                    else pure Nothing
            )
            =<< readIORef structureDeclarations

    case found of
        Just nameTy -> pure (HLIR.MkTyPointer nameTy, [])
        Nothing -> do
            modifyIORef' structureDeclarations (<> [structDecl])

            pure
                ( HLIR.MkTyPointer (HLIR.MkTyId lambdaStructName)
                , concat ns1 <> ns2 <> [structDecl]
                )
convertType (HLIR.MkTyPointer pointee) = do
    (newPointee, ns) <- convertType pointee
    pure (HLIR.MkTyPointer newPointee, ns)
convertType (HLIR.MkTyAnonymousStructure ann fields) = do
    (ty, ns) <- convertType ann
    (tys, nss) <- mapAndUnzipM convertType (Map.elems fields)
    let fieldNames = Map.keys fields
        fieldTypes = Map.fromList (zip fieldNames tys)

    pure (HLIR.MkTyAnonymousStructure ty fieldTypes, mconcat nss <> ns)
convertType t = pure (t, [])

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
    (nss, params') <-
        mapAndUnzipM
            ( \(HLIR.MkAnnotation paramName paramType) -> do
                (newParamType, ns) <- convertType paramType
                pure (ns, HLIR.MkAnnotation paramName newParamType)
            )
            params

    let funType = HLIR.MkTyFun (map (.typeValue) params) returnType

    modifyIORef' globals (Map.insert name.name funType)

    oldLocals <- readIORef locals

    modifyIORef' locals (<> Map.fromList (map HLIR.unannotate params))

    (newBody, ns, _) <- convertExpression body

    writeIORef locals oldLocals
    (newReturnType, ns') <- convertType returnType
    let newFunType = HLIR.MkTyFun (map (.typeValue) params) newReturnType

    modifyIORef' globals (Map.insert name.name newFunType)

    pure
        ( mconcat nss <> ns <> ns'
            ++ [ HLIR.MkTopFunctionDeclaration
                    { HLIR.name = name
                    , HLIR.parameters = params'
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
    (nss, newFields) <-
        mapAndUnzipM
            ( \(fieldName, fieldType) -> do
                (newFieldType, ns) <- convertType fieldType
                pure (ns, (fieldName, newFieldType))
            )
            (Map.toList fields)

    modifyIORef' structures (Map.insert name.name (Map.fromList newFields))
    pure
        $ concat nss <> [HLIR.MkTopStructureDeclaration name (Map.fromList newFields)]
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
        (newValue, ns1, ty) <-
            convertLambda
                (HLIR.MkExprLambda args returnType body)
                (Map.singleton binding.name binding.typeValue.runIdentity)

        -- We need to add the binding to the environment for the inExpr
        -- so that recursive functions can reference themselves
        oldEnv <- readIORef locals
        modifyIORef' locals (Map.insert binding.name binding.typeValue.runIdentity)

        (newInExpr, ns2, retTy) <- convertExpression inExpr

        writeIORef locals oldEnv

        pure
            ( HLIR.MkExprLetIn
                { HLIR.binding = binding{HLIR.typeValue = Identity ty}
                , HLIR.value = newValue
                , HLIR.inExpr = newInExpr
                , HLIR.returnType = Identity retTy
                }
            , ns1 <> ns2
            , retTy
            )
    | otherwise = do
        (newValue, ns1, ty) <- convertExpression value

        oldEnv <- readIORef locals
        modifyIORef' locals (Map.insert binding.name binding.typeValue.runIdentity)

        (newInExpr, ns2, retTy) <- convertExpression inExpr

        writeIORef locals oldEnv

        pure
            ( HLIR.MkExprLetIn
                { HLIR.binding = binding{HLIR.typeValue = Identity ty}
                , HLIR.value = newValue
                , HLIR.inExpr = newInExpr
                , HLIR.returnType = Identity retTy
                }
            , ns1 <> ns2
            , retTy
            )
convertExpression (HLIR.MkExprApplication callee arguments retType) = do
    nativeFunctions <- readIORef natives
    globalFunctions <- readIORef globals

    let allFunctions = nativeFunctions <> globalFunctions

    (newArguments, nss, argTypes) <- unzip3 <$> mapM convertExpression arguments

    (retType', ns') <- convertType retType.runIdentity

    case getVariable callee of
        Just (name, _) | Just _ <- Map.lookup name allFunctions -> do
            let funTy = argTypes HLIR.:->: retType'

            pure
                ( HLIR.MkExprApplication
                    { callee = HLIR.MkExprVariable (HLIR.MkAnnotation name (Identity funTy)) []
                    , arguments = newArguments
                    , returnType = Identity retType'
                    }
                , mconcat nss <> ns'
                , retType'
                )
        _ -> do
            (convertedFunction, ns'', calleeType) <- convertExpression callee

            lambdaCallName <- freshSymbol "lambda_call_"

            let callVar =
                    HLIR.MkExprDereference
                        ( HLIR.MkExprVariable
                            ( HLIR.MkAnnotation
                                lambdaCallName
                                (Identity calleeType)
                            )
                            []
                        )
                        (Identity calleeType)
                function = HLIR.MkExprStructureAccess callVar "function"
                environment = HLIR.MkExprStructureAccess callVar "environment"

            let call = HLIR.MkExprApplication function (environment : arguments) (Identity retType')

            pure
                ( HLIR.MkExprLetIn
                    { HLIR.binding =
                        HLIR.MkAnnotation
                            lambdaCallName
                            (Identity calleeType)
                    , HLIR.value =
                        HLIR.MkExprCast
                            convertedFunction
                            calleeType
                    , HLIR.inExpr = call
                    , HLIR.returnType =
                        Identity retType'
                    }
                , mconcat nss <> ns' <> ns''
                , retType'
                )
convertExpression (HLIR.MkExprLocated _ e) = convertExpression e
convertExpression e@(HLIR.MkExprVariable ann@(HLIR.MkAnnotation name (Identity ty)) _) = do
    locals' <- readIORef locals
    globals' <- readIORef globals

    let variables = locals' <> globals'

    case Map.lookup name variables of
        Just varTy -> do
            (varTy', ns) <- convertType varTy
            pure
                (HLIR.MkExprVariable ann{HLIR.typeValue = Identity varTy'} [], ns, varTy')
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
    convertLambda e Map.empty
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
    pure
        ( HLIR.MkExprReference newE (Identity (HLIR.MkTyPointer eTy))
        , ns
        , HLIR.MkTyPointer eTy
        )
convertExpression (HLIR.MkExprUpdate update value _) = do
    (newUpdate, ns1, updateTy) <- convertExpression update
    (newValue, ns2, _) <- convertExpression value

    pure
        (HLIR.MkExprUpdate newUpdate newValue (Identity updateTy), ns1 <> ns2, updateTy)
convertExpression (HLIR.MkExprSizeOf t) = do
    (t', ns) <- convertType t
    pure (HLIR.MkExprSizeOf t', ns, HLIR.MkTyId "u64")
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
    (ty, ns') <- convertType t
    pure (HLIR.MkExprCast newE ty, ns <> ns', ty)
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
convertLambda (HLIR.MkExprLambda args _ body) reserved = do
    let freeVariablesInBody = M.free body
    nativeFunctions <- readIORef natives
    globalFunctions <- readIORef globals

    (ns1, typedArguments) <-
        sequence
            <$> forM
                args
                ( \(HLIR.MkAnnotation name (Identity ty)) -> do
                    (newTy, ns) <- convertType ty
                    pure (ns, HLIR.MkAnnotation name (Identity newTy))
                )
    let arguments = Map.fromList (map (HLIR.unannotate . (runIdentity <$>)) args)

    let finalNativeFunctions = (nativeFunctions <> globalFunctions) Map.\\ arguments

    let preEnvironment = freeVariablesInBody Map.\\ (finalNativeFunctions <> arguments <> reserved)
    (ns, environment) <-
        sequence <$> traverse ((swap <$>) . convertType) preEnvironment

    environmentStructName <- freshSymbol "closure_env_"
    lambdaStructName <- freshSymbol "closure_fn_"

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

    let environmentVar =
            HLIR.MkExprVariable
                (HLIR.MkAnnotation "env" (Identity (HLIR.MkTyId environmentStructName)))
                []

    oldLocals <- readIORef locals
    modifyIORef' locals (<> arguments)

    (newBody, ns2, newReturnType) <- convertExpression body

    writeIORef locals oldLocals

    let newBody' =
            foldr
                ( \(name, ty) acc ->
                    HLIR.MkExprLetIn
                        { HLIR.binding = HLIR.MkAnnotation name (Identity ty)
                        , HLIR.value =
                            HLIR.MkExprStructureAccess
                                { HLIR.structure =
                                    HLIR.MkExprDereference
                                        environmentVar
                                        (Identity (HLIR.MkTyId environmentStructName))
                                , HLIR.field = name
                                }
                        , HLIR.inExpr = acc
                        , HLIR.returnType = Identity newReturnType
                        }
                )
                newBody
                (Map.toList environment)

    let lambdaStructure =
            HLIR.MkTopStructureDeclaration
                { HLIR.header = HLIR.MkAnnotation lambdaStructName []
                , HLIR.fields =
                    Map.fromList
                        [
                            ( "function"
                            , HLIR.MkTyFun
                                ( HLIR.MkTyPointer (HLIR.MkTyId environmentStructName)
                                    : map (runIdentity . (.typeValue)) typedArguments
                                )
                                newReturnType
                            )
                        , ("environment", HLIR.MkTyPointer (HLIR.MkTyId environmentStructName))
                        ]
                }

    envAllocName <- freshSymbol "env_alloc_"
    let envAlloc = malloc (HLIR.MkTyId environmentStructName)
        envAllocVar =
            HLIR.MkExprVariable
                ( HLIR.MkAnnotation
                    envAllocName
                    (Identity (HLIR.MkTyPointer (HLIR.MkTyId environmentStructName)))
                )
                []
        envUpdate =
            HLIR.MkExprUpdate
                ( HLIR.MkExprDereference
                    envAllocVar
                    (Identity (HLIR.MkTyId environmentStructName))
                )
                environmentStructCreation
                (Identity (HLIR.MkTyId environmentStructName))

    let lambdaStructureCreation =
            HLIR.MkExprStructureCreation
                { HLIR.annotation = HLIR.MkTyId lambdaStructName
                , HLIR.fields =
                    Map.fromList
                        [
                            ( "function"
                            , HLIR.MkExprLambda
                                { HLIR.parameters =
                                    HLIR.MkAnnotation
                                        "env"
                                        (Identity (HLIR.MkTyPointer (HLIR.MkTyId environmentStructName)))
                                        : typedArguments
                                , HLIR.returnType = Identity newReturnType
                                , HLIR.body = newBody'
                                }
                            )
                        ,
                            ( "environment"
                            , envAllocVar
                            )
                        ]
                }

    lambdaStructAllocName <- freshSymbol "lambda_alloc_"
    let lambdaStructAlloc = malloc (HLIR.MkTyId lambdaStructName)
        lambdaStructAllocVar =
            HLIR.MkExprVariable
                ( HLIR.MkAnnotation
                    lambdaStructAllocName
                    (Identity (HLIR.MkTyPointer (HLIR.MkTyId lambdaStructName)))
                )
                []
        lambdaStructUpdate =
            HLIR.MkExprUpdate
                ( HLIR.MkExprDereference
                    lambdaStructAllocVar
                    (Identity (HLIR.MkTyId lambdaStructName))
                )
                lambdaStructureCreation
                (Identity (HLIR.MkTyId lambdaStructName))

    let castAndReference = HLIR.MkExprCast
            lambdaStructAllocVar
            (HLIR.MkTyPointer (HLIR.MkTyId "void"))

    let updates =
            HLIR.MkExprLetIn
                { HLIR.binding = HLIR.MkAnnotation "_" (Identity (HLIR.MkTyId "void"))
                , HLIR.value = envUpdate
                , HLIR.inExpr = HLIR.MkExprLetIn
                    { HLIR.binding = HLIR.MkAnnotation "_" (Identity (HLIR.MkTyId "void"))
                    , HLIR.value = lambdaStructUpdate
                    , HLIR.inExpr = castAndReference
                    , HLIR.returnType = Identity (HLIR.MkTyPointer (HLIR.MkTyId lambdaStructName))
                    }
                , HLIR.returnType = Identity (HLIR.MkTyPointer (HLIR.MkTyId environmentStructName))
                }



    let lets = HLIR.MkExprLetIn
                { HLIR.binding =
                    HLIR.MkAnnotation
                        envAllocName
                        (Identity (HLIR.MkTyPointer (HLIR.MkTyId environmentStructName)))
                , HLIR.value = envAlloc
                , HLIR.inExpr =
                    HLIR.MkExprLetIn
                        { HLIR.binding =
                            HLIR.MkAnnotation
                                lambdaStructAllocName
                                (Identity (HLIR.MkTyPointer (HLIR.MkTyId lambdaStructName)))
                        , HLIR.value = lambdaStructAlloc
                        , HLIR.inExpr = updates
                        , HLIR.returnType = Identity (HLIR.MkTyPointer (HLIR.MkTyId lambdaStructName))
                        }
                , HLIR.returnType = Identity (HLIR.MkTyPointer (HLIR.MkTyId lambdaStructName))
                }
    pure
        ( lets
        , ns <> ns1 <> ns2 <> [environmentStructure, lambdaStructure]
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
