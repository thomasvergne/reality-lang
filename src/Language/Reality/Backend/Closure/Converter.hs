{-# LANGUAGE LambdaCase #-}

module Language.Reality.Backend.Closure.Converter where

import Control.Monad.Result qualified as M
import Data.Map qualified as Map
import Data.Text qualified as Text
import GHC.IO qualified as IO
import Language.Reality.Backend.Closure.Free qualified as M
import Language.Reality.Backend.Closure.Hoisting qualified as HT
import Language.Reality.Frontend.Typechecker.Checker qualified as TC
import Language.Reality.Syntax.HLIR qualified as HLIR
import Language.Reality.Backend.Specialization.Resolver qualified as SR

{-# NOINLINE structureDeclarations #-}
structureDeclarations :: IORef [HLIR.TLIR "toplevel"]
structureDeclarations = IO.unsafePerformIO $ newIORef []

convertTypeField :: (MonadIO m) => HLIR.StructureMember HLIR.Type -> m (HLIR.StructureMember HLIR.Type, [HLIR.TLIR "toplevel"])
convertTypeField (HLIR.MkStructField name ty) = do
    (newTy, ns) <- convertType ty
    pure (HLIR.MkStructField name newTy, ns)
convertTypeField (HLIR.MkStructStruct name fields) = do
    (newFields, nss) <- mapAndUnzipM convertTypeField fields
    pure (HLIR.MkStructStruct name newFields, concat nss)
convertTypeField (HLIR.MkStructUnion name fields) = do
    (newFields, nss) <- mapAndUnzipM convertTypeField fields
    pure (HLIR.MkStructUnion name newFields, concat nss)

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
                    "GC_MALLOC"
                    (Identity (HLIR.MkTyFun [HLIR.MkTyId "int"] (HLIR.MkTyPointer HLIR.MkTyChar)))
                )
                []
        , HLIR.arguments = [HLIR.MkExprSizeOf t]
        , HLIR.returnType = Identity (HLIR.MkTyPointer HLIR.MkTyChar)
        }

expandStructure :: (MonadIO m) => HLIR.TLIR "toplevel" -> m HLIR.Type
expandStructure (HLIR.MkTopStructureDeclaration name fields) = do
    newFields <- traverse expandTypeField fields
    let ty = SR.fieldToType <$> newFields

    pure $ HLIR.MkTyAnonymousStructure False (HLIR.MkTyId name.name) (Map.fromList ty)
expandStructure (HLIR.MkTopLocated _ e) = expandStructure e
expandStructure _ = error "Expected a structure declaration"


expandTypeField :: (MonadIO m) => HLIR.StructureMember HLIR.Type -> m (HLIR.StructureMember HLIR.Type)
expandTypeField (HLIR.MkStructField name ty) = do
    newTy <- expandType ty
    pure (HLIR.MkStructField name newTy)
expandTypeField (HLIR.MkStructStruct name fields) = do
    newFields <- mapM expandTypeField fields
    pure (HLIR.MkStructStruct name newFields)
expandTypeField (HLIR.MkStructUnion name fields) = do
    newFields <- mapM expandTypeField fields
    pure (HLIR.MkStructUnion name newFields)

expandType :: (MonadIO m) => HLIR.Type -> m HLIR.Type
expandType (HLIR.MkTyAnonymousStructure b ann fields) = do
    (newAnn, _) <- convertType ann
    (newFields, _) <- mapAndUnzipM convertType (Map.elems fields)
    let fieldNames = Map.keys fields
        fieldTypes = Map.fromList (zip fieldNames newFields)

    pure $ HLIR.MkTyAnonymousStructure b newAnn fieldTypes
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

-- | Convert a type to a closure-converted type.
-- | This function takes a type, and returns a type with closures converted.
-- | It also returns a list of toplevel nodes that were created during the conversion.
-- |
-- | For example, a function type `A -> B` will be converted to a structure type
-- | containing a function pointer and an environment pointer.
-- | The function pointer will have the type:
-- |
-- | struct {
-- |    function: (void*, A) -> B,
-- |    environment: void*
-- | }
-- |
-- | The function type will then be a pointer to this structure type.
convertType :: (MonadIO m) => HLIR.Type -> m (HLIR.Type, [HLIR.TLIR "toplevel"])
convertType (HLIR.MkTyFun argTypes returnType) = do
    -- Converting type arguments to their closure-converted form
    (newArgTypes, ns1) <- mapAndUnzipM convertType argTypes
    (newReturnType, ns2) <- convertType returnType

    -- Creating a unique name for the lambda structure
    lambdaStructName <- freshSymbol "function_type_"

    -- Creating the structure declaration for the lambda
    let structDecl =
            HLIR.MkTopStructureDeclaration
                { HLIR.header = HLIR.MkAnnotation lambdaStructName []
                , HLIR.fields =
                    [
                        HLIR.MkStructField "function"
                            ( HLIR.MkTyFun
                                ( HLIR.MkTyPointer HLIR.MkTyChar
                                    : newArgTypes
                                )
                                newReturnType
                            )
                    ,   HLIR.MkStructField "environment" (HLIR.MkTyPointer HLIR.MkTyChar)
                    ]
                }

    -- Expanding the structure to get a type with all types reduced
    -- so we can compare it with existing structures
    f1 <-
        expandStructure structDecl >>= \case
            HLIR.MkTyAnonymousStructure _ _ f -> pure f
            _ -> pure Map.empty

    found <-
        findM
            ( \node -> do
                -- Also expand existing structures to compare them properly
                -- (e.g. if they're strictly equal but with different names)
                (name, f2) <-
                    expandStructure node >>= \case
                        HLIR.MkTyAnonymousStructure _ name f -> pure (Just name, f)
                        _ -> pure (Nothing, Map.empty)

                -- If the fields are the same, we can reuse the existing structure
                -- otherwise, we need to create a new one
                if f1 == f2
                    then case name of
                        Just n -> pure (Just n)
                        Nothing -> pure Nothing
                    else pure Nothing
            )
            =<< readIORef structureDeclarations

    -- If we found an existing structure, reuse it
    -- otherwise, add the new structure to the list of declarations
    -- and return a pointer to it
    -- We return the list of new structure declarations as well
    -- so they can be added to the toplevel later
    -- This way, we avoid duplicating structure declarations
    -- for identical function types
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
convertType (HLIR.MkTyAnonymousStructure b ann fields) = do
    (ty, ns) <- convertType ann
    (tys, nss) <- mapAndUnzipM convertType (Map.elems fields)
    let fieldNames = Map.keys fields
        fieldTypes = Map.fromList (zip fieldNames tys)

    pure (HLIR.MkTyAnonymousStructure b ty fieldTypes, mconcat nss <> ns)
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
    -- Pre-converting the parameter types, and return type, but not
    -- using them yet, as the converter currently convert types when
    -- encountering variables, so we need to keep the original types
    -- for now.
    (nss, params') <-
        mapAndUnzipM
            ( \(HLIR.MkAnnotation paramName paramType) -> do
                (newParamType, ns) <- convertType paramType
                pure (ns, HLIR.MkAnnotation paramName newParamType)
            )
            params
    (newReturnType, ns') <- convertType returnType

    -- Inserting the function type in the global environment
    -- so that recursive functions can reference themselves
    -- with their closure-converted type

    let funType = HLIR.MkTyFun (map (.typeValue) params) returnType

    modifyIORef' globals (Map.insert name.name funType)

    -- Adding original parameters to the local environment
    -- so that they can be referenced in the body
    -- Also saving the old local environment to restore it later
    -- so that parameters don't leak outside the function body

    oldLocals <- readIORef locals

    modifyIORef' locals (<> Map.fromList (map HLIR.unannotate params))

    (newBody, ns, _) <- convertExpression body

    writeIORef locals oldLocals

    -- Creating the new function type with closure-converted types
    -- and inserting it in the global environment
    -- so that other functions can reference it
    -- This is necessary because the body may have introduced new types
    -- that were not present in the original parameter types
    -- (e.g. if a parameter was a function type that was converted to a closure type)
    let newFunType = HLIR.MkTyFun (map (.typeValue) params') newReturnType

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
            ( \field -> do
                (newFieldType, ns) <- convertTypeField field
                pure (ns, newFieldType)
            )
            fields

    modifyIORef' structures (Map.insert name.name newFields)
    pure
        $ concat nss <> [HLIR.MkTopStructureDeclaration name newFields]
convertSingularNode (HLIR.MkTopPublic node) = do
    ns <- convertSingularNode node
    pure (map HLIR.MkTopPublic ns)
convertSingularNode (HLIR.MkTopExternalFunction header params returnType) = do
    let funType = HLIR.MkTyFun (map (.typeValue) params) returnType
    modifyIORef' natives (Map.insert header.name funType)
    pure [HLIR.MkTopExternalFunction header params returnType]
convertSingularNode (HLIR.MkTopExternLet ann) = do
    modifyIORef' natives (Map.insert ann.name ann.typeValue)
    pure [HLIR.MkTopExternLet ann]
convertSingularNode (HLIR.MkTopAnnotation exprs node) = do
    ns <- convertSingularNode node

    case exprs of
        (x : _) | Just "intrinsic" <- HLIR.getFirstAnnotationArgument x -> pure []
        _ -> pure ns
convertSingularNode (HLIR.MkTopEnumeration header constructors) = do
    let typeHeader = HLIR.MkTyId header.name

    forM_ (Map.toList constructors) $ \case
        (name, Just tys) ->
            modifyIORef' globals (Map.insert name (HLIR.MkTyFun tys typeHeader))
        (name, Nothing) ->
            modifyIORef' globals (Map.insert name typeHeader)

    pure [HLIR.MkTopEnumeration header constructors]
convertSingularNode other = pure [other]

-- | Convert an expression to a closure-converted expression.
-- | This function takes an expression, and returns an expression with closures
-- | converted.
convertExpression ::
    (MonadIO m) =>
    HLIR.TLIR "expression" ->
    m (HLIR.TLIR "expression", [HLIR.TLIR "toplevel"], HLIR.Type)
convertExpression (HLIR.MkExprLetIn binding value inExpr _)
    -- If the value is a lambda, we need to convert it to a closure
    -- and taking care of the let-binding name to allow for recursion
    -- (e.g. let f = \x -> if x == 0 then 1 else x * f (x - 1) in f 5)
    | Just (HLIR.MkExprLambda args returnType body) <- getLambda value = do
        -- Converting the lambda expression to a closure. We pass
        -- the binding name and type in the environment so that the
        -- lambda can reference itself recursively
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
    -- Reading the global and native environments to check if the callee
    -- is a known function. If it is, we can use its type directly
    -- otherwise, we need to convert the callee expression
    -- and create a closure call
    nativeFunctions <- readIORef natives
    globalFunctions <- readIORef globals

    let allFunctions = nativeFunctions <> globalFunctions

    -- Converting the arguments and the return type of the application
    -- so that we can use them in the function type
    (newArguments, nss, argTypes) <- unzip3 <$> mapM convertExpression arguments

    (retType', ns') <- convertType retType.runIdentity

    case getVariable callee of
        -- Checking if the callee is a known function
        -- If it is, we can create a direct call
        -- otherwise, we need to create a closure call
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
            -- Converting the callee expression to get the closure
            -- structure and its type
            (convertedFunction, ns'', calleeType) <- convertExpression callee

            -- Creating a unique name for the lambda call variable
            -- to avoid name collisions
            lambdaCallName <- freshSymbol "lambda_call_"

            -- We exactly have this schema foreach closure call:
            --
            -- let lambda_call_X = (CLOSURE_EXPRESSION : CALLEE_TYPE) in
            --     lambda_call_X->function(lambda_call_X->environment, ARGS...)
            --
            -- Where CALLEE_TYPE is a pointer to a structure:
            --
            -- struct {
            --    function: (void*, ARG_TYPES...) -> RET_TYPE,
            --    environment: void*
            -- }
            --
            -- This way, we can call the function with its environment
            -- and the arguments
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
convertExpression e@(HLIR.MkExprVariable (HLIR.MkAnnotation name (Identity ty)) _) = do
    -- Reading the local and global environments to find the variable type
    -- and convert it to its closure-converted form
    locals' <- readIORef locals
    globals' <- readIORef globals
    natives' <- readIORef natives

    let variables = locals' <> globals'

    (expr, ns', exprTy) <- case Map.lookup name (globals' <> natives') of
        Just (HLIR.MkTyFun args retTy) -> do
            let function =
                    HLIR.MkExprLambda
                        { HLIR.parameters =
                            zipWith
                                (\i ty' -> HLIR.MkAnnotation ("arg" <> show i) (Identity ty'))
                                [(0 :: Int) ..]
                                args
                        , HLIR.returnType = Identity retTy
                        , HLIR.body =
                            HLIR.MkExprApplication
                                { HLIR.callee =
                                    HLIR.MkExprVariable
                                        (HLIR.MkAnnotation name (Identity (HLIR.MkTyFun args retTy)))
                                        []
                                , HLIR.arguments =
                                    zipWith
                                        ( \i ty' -> HLIR.MkExprVariable (HLIR.MkAnnotation ("arg" <> show i) (Identity ty')) []
                                        )
                                        [(0 :: Int) ..]
                                        args
                                , HLIR.returnType = Identity retTy
                                }
                        }

            convertExpression function
        _ -> pure (e, [], ty)

    case Map.lookup name variables of
        -- If the variable is found, we convert its type and return it
        Just varTy -> do
            (varTy', ns) <- convertType varTy
            pure
                (expr, ns <> ns', varTy')

        -- If the variable is not found, we assume it's a native variable
        --
        -- NOTE: We may add further checks to ensure the variable should not
        -- be encapsulated in a closure. Like this:
        --
        --            Γ f : τ₀ -> τ₁ ⊢ cc f
        -- ---------------------------------------------
        --  Γ f : τ₀ -> τ₁, x₀: τ₀ ⊢ cc (λx₀: τ₀. f x₀)
        --
        -- This permits native functions to be used in function calls
        -- as function call arguments
        Nothing -> pure (expr, ns', exprTy)
convertExpression e@(HLIR.MkExprLiteral lit) =
    pure
        ( e
        , []
        , case lit of
            HLIR.MkLitInt _ -> HLIR.MkTyId "int"
            HLIR.MkLitFloat _ -> HLIR.MkTyId "float"
            HLIR.MkLitBool _ -> HLIR.MkTyId "bool"
            HLIR.MkLitString _ -> HLIR.MkTyPointer HLIR.MkTyChar
            HLIR.MkLitChar _ -> HLIR.MkTyChar
        )
convertExpression e@(HLIR.MkExprLambda{}) = do
    convertLambda e Map.empty
convertExpression (HLIR.MkExprCondition cond thenB elseB _) = do
    (newCond, ns1, _) <- convertExpression cond
    (newThen, ns2, thenTy) <- convertExpression thenB
    (newElse, ns3, _) <- convertExpression elseB

    pure
        ( HLIR.MkExprCondition newCond newThen newElse (Identity thenTy)
        , ns1 <> ns2 <> ns3
        , thenTy
        )
convertExpression (HLIR.MkExprStructureAccess struct field) = do
    (newStruct, ns, structTy) <- convertExpression struct

    -- Looking up the structure fields in the global structure environment
    -- to find the type of the accessed field
    structures' <- readIORef structures
    let name = TC.getHeader structTy

    case structTy of
        -- If the structure is anonymous, we can directly look up the field
        -- in its fields
        HLIR.MkTyAnonymousStructure _ _ fields -> case Map.lookup field fields of
            Just ty -> pure (HLIR.MkExprStructureAccess newStruct field, ns, ty)
            Nothing -> M.compilerError $ "Field " <> field <> " does not exist in structure"
        _ -> case name of
            -- If the structure is named, we look it up in the global environment
            -- to find its fields
            Just structName | Just fields <- Map.lookup structName structures' ->
                case lookupField field fields of
                    Just ty -> pure (HLIR.MkExprStructureAccess newStruct field, ns, ty)
                    -- If the field is not found, we raise a compiler error
                    Nothing ->
                        M.compilerError
                            $ "Field " <> field <> " does not exist in structure " <> structName
            -- If the structure is not found, we raise a compiler error
            _ -> M.compilerError $ 
                "Expected a structure type: " 
                    <> show structTy 
                    <> " for expression: "
                    <> toText struct <> "." <> field

    where
        lookupField :: Text -> [HLIR.StructureMember HLIR.Type] -> Maybe HLIR.Type
        lookupField fName = foldr
                ( \member acc ->
                    case member of
                        HLIR.MkStructField mName mType ->
                            if mName == fName then Just mType else acc
                        HLIR.MkStructStruct mName mFields -> do
                            let fieldsAsType = Map.fromList $ map SR.fieldToType mFields
                            if mName == fName
                                then Just (HLIR.MkTyAnonymousStructure False (HLIR.MkTyId mName) fieldsAsType)
                                else acc
                        HLIR.MkStructUnion mName mFields -> do
                            let fieldsAsType = Map.fromList $ map SR.fieldToType mFields
                            if mName == fName
                                then Just (HLIR.MkTyAnonymousStructure True (HLIR.MkTyId mName) fieldsAsType)
                                else acc
                )
                Nothing
convertExpression (HLIR.MkExprStructureCreation ann fields) = do
    (newFields, nss, tys) <- unzip3 <$> mapM convertExpression (Map.elems fields)
    let fieldNames = Map.keys fields
        newFieldMap = Map.fromList (zip fieldNames newFields)
        fieldTypes = Map.fromList (zip fieldNames tys)

    pure
        ( HLIR.MkExprStructureCreation ann newFieldMap
        , mconcat nss
        , HLIR.MkTyAnonymousStructure False ann fieldTypes
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
    pure (HLIR.MkExprSizeOf t', ns, HLIR.MkTyId "int")
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
convertExpression (HLIR.MkExprIs e p _) = do
    (newE, ns1, ty) <- convertExpression e
    (newP, ns2) <- convertPattern p
    (newTy, ns3) <- convertType ty

    let freeP = M.free newP

    modifyIORef' locals (<> freeP)

    pure
        ( HLIR.MkExprIs newE newP (Identity newTy)
        , ns1 <> ns2 <> ns3
        , HLIR.MkTyId "bool"
        )
convertExpression (HLIR.MkExprFunctionAccess {}) = M.compilerError "Function access should have been inlined before closure conversion"
convertExpression (HLIR.MkExprReturn e) = do
    (newE, ns, eTy) <- convertExpression e
    pure (HLIR.MkExprReturn newE, ns, eTy)
convertExpression HLIR.MkExprContinue = pure (HLIR.MkExprContinue, [], HLIR.MkTyId "void")
convertExpression HLIR.MkExprBreak = pure (HLIR.MkExprBreak, [], HLIR.MkTyId "void")
convertExpression (HLIR.MkExprLetPatternIn pattern value inExpr _) = do
    (newPattern, ns1) <- convertPattern pattern
    (newValue, ns2, _) <- convertExpression value

    let freeP = M.free newPattern

    modifyIORef' locals (<> freeP)

    (newInExpr, ns3, inTy) <- convertExpression inExpr

    pure
        ( HLIR.MkExprLetPatternIn newPattern newValue newInExpr (Identity inTy)
        , ns1 <> ns2 <> ns3
        , inTy
        )

convertPattern :: (MonadIO m) => HLIR.TLIR "pattern" -> m (HLIR.TLIR "pattern", [HLIR.TLIR "toplevel"])
convertPattern (HLIR.MkPatternLocated _ p) = convertPattern p
convertPattern (HLIR.MkPatternLiteral lit) = pure (HLIR.MkPatternLiteral lit, [])
convertPattern (HLIR.MkPatternVariable ann) = pure (HLIR.MkPatternVariable ann, [])
convertPattern HLIR.MkPatternWildcard = pure (HLIR.MkPatternWildcard, [])
convertPattern (HLIR.MkPatternStructure ann fields) = do
    (newFields, nss) <- mapAndUnzipM convertPattern (Map.elems fields)
    let fieldNames = Map.keys fields
        newFieldMap = Map.fromList (zip fieldNames newFields)
    pure (HLIR.MkPatternStructure ann newFieldMap, mconcat nss)
convertPattern (HLIR.MkPatternConstructor name patterns ty) = do
    (newPatterns, nss) <- mapAndUnzipM convertPattern patterns
    pure (HLIR.MkPatternConstructor name newPatterns ty, mconcat nss)
convertPattern (HLIR.MkPatternLet ann) = do
    (newType, ns) <- convertType ann.typeValue.runIdentity
    pure (HLIR.MkPatternLet ann{HLIR.typeValue = Identity newType}, ns)

-- | Convert a lambda expression to a closure-converted expression.
-- | This function takes a lambda expression, and returns an expression with closures
-- | converted.
convertLambda ::
    (MonadIO m) =>
    HLIR.TLIR "expression" ->
    Map Text HLIR.Type ->
    m (HLIR.TLIR "expression", [HLIR.TLIR "toplevel"], HLIR.Type)
convertLambda (HLIR.MkExprLambda args _ body) reserved = do
    -- Finding the free variables in the lambda body to include them in the
    -- closure environment structure. We exclude the arguments and reserved
    -- variables (e.g. the function name for recursion) from the free variables.
    -- We also exclude native and global functions from the free variables
    -- as they don't need to be included in the closure environment.
    let freeVariablesInBody = M.free body
    nativeFunctions <- readIORef natives
    globalFunctions <- readIORef globals

    -- Building the current environment, by sequentially adding and exluding
    -- variables from the local environment. Then converting the pre-environment
    -- variables to their closure-converted form.
    let arguments = Map.fromList (map (HLIR.unannotate . (runIdentity <$>)) args)
    let finalNativeFunctions = (nativeFunctions <> globalFunctions) Map.\\ arguments
    let preEnvironment = freeVariablesInBody Map.\\ (finalNativeFunctions <> arguments <> reserved)
    (ns, environment) <-
        sequence <$> traverse ((swap <$>) . convertType) preEnvironment

    -- Creating unique names for the environment and lambda structures
    -- to avoid name collisions
    environmentStructName <- freshSymbol "closure_env_"
    lambdaStructName <- freshSymbol "closure_fn_"

    -- Creating both the environment structure declaration and its creation expression
    -- to be used in the closure structure creation:
    --
    -- struct env {
    --   x0: T0,
    --   x1: T1,
    --   ...
    -- }
    --
    -- let env = env {
    --   x0 = x0,
    --   x1 = x1,
    --  ...
    -- }
    let environmentStructure =
            HLIR.MkTopStructureDeclaration
                { HLIR.header = HLIR.MkAnnotation environmentStructName []
                , HLIR.fields = map (uncurry HLIR.MkStructField) (Map.toList environment)
                }
        environmentStructCreation =
            HLIR.MkExprStructureCreation
                { HLIR.annotation = HLIR.MkTyId environmentStructName
                , HLIR.fields =
                    Map.mapWithKey
                        (\name ty -> HLIR.MkExprVariable (HLIR.MkAnnotation name (Identity ty)) [])
                        environment
                }
        environmentVar =
            HLIR.MkExprVariable
                (HLIR.MkAnnotation "env" (Identity (HLIR.MkTyId environmentStructName)))
                []

    -- Adding the arguments to the local environment so that they can be referenced
    -- in the body. We also save the old local environment to restore it later
    -- so that the arguments don't leak outside the lambda body.
    oldLocals <- readIORef locals
    modifyIORef' locals (<> arguments)

    (newBody, ns2, newReturnType) <- convertExpression body

    writeIORef locals oldLocals

    -- Wrapping the body with let-bindings to extract the free variables
    -- from the environment structure:
    --
    -- let x0 = env->x0;
    -- let x1 = env->x1;
    -- ...
    -- let xn = env->xn;
    -- BODY
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

    -- Converting the argument types to their closure-converted form
    (ns1, typedArguments) <-
        sequence
            <$> forM
                args
                ( \(HLIR.MkAnnotation name (Identity ty)) -> do
                    (newTy, ns') <- convertType ty
                    pure (ns', HLIR.MkAnnotation name (Identity newTy))
                )

    -- Building the lambda structure declaration:
    --
    -- struct lambda {
    --   function: (env*, ARG_TYPES...) -> RET_TYPE,
    --   environment: env*
    -- }
    --
    -- Environment is passed as a pointer to avoid sizeof issues when
    -- the structure is empty (because of void* environment carried by
    -- generic closures).
    let lambdaStructure =
            HLIR.MkTopStructureDeclaration
                { HLIR.header = HLIR.MkAnnotation lambdaStructName []
                , HLIR.fields =
                    [
                        HLIR.MkStructField "function"
                            ( HLIR.MkTyFun
                                ( HLIR.MkTyPointer (HLIR.MkTyId environmentStructName)
                                    : map (runIdentity . (.typeValue)) typedArguments
                                )
                                newReturnType
                            )
                    , HLIR.MkStructField "environment" (HLIR.MkTyPointer (HLIR.MkTyId environmentStructName))
                    ]
                }

    -- Creating the environment allocation. We need this because closures
    -- may outlive their defining scope, so we need to allocate them on the heap.
    --
    -- This means for instance that a list of closures cannot be valid if closures
    -- are allocated on the stack.
    --
    -- So we do:
    -- let env_alloc_X = malloc(env);
    --    *env_alloc_X = env { ... };
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

    -- Creating the lambda structure creation expression:
    --
    -- <lambdaStructName> {
    --   function = \env args... -> BODY,
    --   environment = env_alloc_X
    -- }
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

    -- We do similarly for the lambda structure:
    --
    -- let lambda_alloc_X = malloc(lambda);
    --   *lambda_alloc_X = lambda { ... };
    -- return (lambda_alloc_X : void*);
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

    -- Casting the lambda structure pointer to void* to match every type
    -- of generic closure, as closures are passed by reference, by
    -- malloc-ing them on the heap.
    let castAndReference =
            HLIR.MkExprCast
                lambdaStructAllocVar
                (HLIR.MkTyPointer (HLIR.MkTyId "void"))

    -- We build a one-expression let-binding sequence to perform the allocations
    -- and updates, and return the lambda structure pointer casted to void*:
    let updates =
            HLIR.MkExprLetIn
                { HLIR.binding = HLIR.MkAnnotation "_" (Identity (HLIR.MkTyId "void"))
                , HLIR.value = envUpdate
                , HLIR.inExpr =
                    HLIR.MkExprLetIn
                        { HLIR.binding = HLIR.MkAnnotation "_" (Identity (HLIR.MkTyId "void"))
                        , HLIR.value = lambdaStructUpdate
                        , HLIR.inExpr = castAndReference
                        , HLIR.returnType = Identity (HLIR.MkTyPointer (HLIR.MkTyId lambdaStructName))
                        }
                , HLIR.returnType =
                    Identity (HLIR.MkTyPointer (HLIR.MkTyId environmentStructName))
                }

    -- We broke the code logic down to make it more readable, but
    -- this is effectively:
    --
    -- let env_alloc_X = malloc(env); *env_alloc_X = env { ... };
    -- let lambda_alloc_X = malloc(lambda); *lambda_alloc_X = lambda { ... };
    -- return (lambda_alloc_X : void*);
    let lets =
            HLIR.MkExprLetIn
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

    -- Returning the final expression, along with the new toplevel nodes
    -- that were created during the conversion (the environment and lambda
    -- structures) and the final type of the lambda (a pointer to the
    -- lambda structure)
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
structures :: IORef (Map Text [HLIR.StructureMember HLIR.Type])
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
