{-# LANGUAGE LambdaCase #-}

module Language.Reality.Frontend.Typechecker.Checker where

import Control.Monad.Except qualified as M
import Control.Monad.Result qualified as M
import Data.Map qualified as Map
import Language.Reality.Frontend.Typechecker.Monad qualified as M
import Language.Reality.Frontend.Typechecker.Unification qualified as M
import Language.Reality.Syntax.HLIR qualified as HLIR

-- | TYPECHECKER
-- | Typecheck a program.
-- | This function takes a list of toplevel nodes, and returns a list of toplevel
-- | nodes with types checked.
runTypechecker ::
    (MonadIO m, M.MonadError M.Error m) =>
    [HLIR.HLIR "toplevel"] ->
    m [HLIR.TLIR "toplevel"]
runTypechecker toplevels = do
    M.resetState

    mapM checkToplevelSingular toplevels

-- | Typecheck a singular HLIR toplevel node.
-- | This function takes a toplevel node, and returns a toplevel node with types
-- | checked.
-- | Toplevel checking is mainly about modifying the environment with new
-- | definitions, and checking function bodies.
checkToplevelSingular ::
    (MonadIO m, M.MonadError M.Error m) =>
    HLIR.HLIR "toplevel" ->
    m (HLIR.TLIR "toplevel")
checkToplevelSingular (HLIR.MkTopConstantDeclaration ann expr) = do
    -- Removing aliases from the expected type
    expectedType <- M.performAliasRemoval ann.typeValue

    -- Checking the expression against the expected type
    (typedExpr, cs) <- checkE expectedType expr

    -- Expression should not have any unresolved constraints as this
    -- means that there are function calls. But constants cannot have function calls.
    -- So we throw an error if there are any unresolved constraints.
    unless (null cs)
        $ M.throw (M.UnsolvedConstraints cs)

    -- Adding the constant to the environment
    modifyIORef' M.defaultCheckerState $ \s ->
        s
            { M.environment = Map.insert ann.name (HLIR.Forall [] expectedType) s.environment
            }

    pure (HLIR.MkTopConstantDeclaration ann typedExpr)
checkToplevelSingular (HLIR.MkTopFunctionDeclaration ann params ret body) = do
    -- Removing aliases from the parameter and return types
    paramTypes <- mapM (M.performAliasRemoval . (.typeValue)) params
    retType <- M.performAliasRemoval ret

    -- Building new parameters and function type
    let newParams = zipWith (\p ty -> p{HLIR.typeValue = ty}) params paramTypes
        funcType = paramTypes HLIR.:->: retType

    -- Adding the function to the environment, before adding arguments,
    -- to allow for restoration of the environment without deleting the
    -- function itself.
    modifyIORef' M.defaultCheckerState $ \s ->
        s
            { M.environment =
                Map.insert ann.name (HLIR.Forall ann.typeValue funcType) s.environment
            }

    -- Collecting old environment to restore it later
    oldEnv <- readIORef M.defaultCheckerState <&> M.environment

    -- Adding parameters to the environment
    modifyIORef' M.defaultCheckerState $ \s ->
        s
            { M.environment =
                foldr
                    ( \p env ->
                        Map.insert p.name (HLIR.Forall [] p.typeValue) env
                    )
                    s.environment
                    newParams
            }

    -- Checking the function body against the return type
    (typedBody, cs) <- checkE retType body

    -- Solving constraints generated during the body checking
    solveConstraints cs

    -- Restoring the old environment
    modifyIORef' M.defaultCheckerState $ \s ->
        s
            { M.environment = oldEnv
            }

    pure (HLIR.MkTopFunctionDeclaration ann newParams retType typedBody)
checkToplevelSingular (HLIR.MkTopTypeAlias ann aliased) = do
    -- Removing aliases from the aliased type
    realiasedType <- M.performAliasRemoval aliased

    -- Adding the type alias to the environment
    modifyIORef' M.defaultCheckerState $ \s ->
        s
            { M.typeAliases =
                Map.insert ann.name (HLIR.Forall ann.typeValue realiasedType) s.typeAliases
            }

    pure (HLIR.MkTopTypeAlias ann aliased)
checkToplevelSingular (HLIR.MkTopLocated p n) = do
    HLIR.pushPosition p
    typedNode <- checkToplevelSingular n
    void HLIR.popPosition
    pure (HLIR.MkTopLocated p typedNode)
checkToplevelSingular (HLIR.MkTopPublic node) = do
    typedNode <- checkToplevelSingular node
    pure (HLIR.MkTopPublic typedNode)
checkToplevelSingular (HLIR.MkTopModuleDeclaration{}) = M.throw (M.CompilerError "Modules are not supported in the typechecker.")
checkToplevelSingular (HLIR.MkTopStructureDeclaration ann fields) = do
    -- Removing aliases from the field types
    fieldTypes <- traverse M.performAliasRemoval fields

    -- Adding the structure to the environment
    modifyIORef' M.defaultCheckerState $ \s ->
        s
            { M.structures =
                Map.insert ann.name (HLIR.Forall ann.typeValue fieldTypes) s.structures
            }

    pure (HLIR.MkTopStructureDeclaration ann fieldTypes)
checkToplevelSingular (HLIR.MkTopExternalFunction ann params ret) = do
    -- Removing aliases from the parameter and return types
    paramTypes <- mapM (M.performAliasRemoval . (.typeValue)) params
    retType <- M.performAliasRemoval ret

    -- Building new parameters and function type
    let funcType = paramTypes HLIR.:->: retType
        newParams = zipWith (\p ty -> p{HLIR.typeValue = ty}) params paramTypes

    -- Adding the external function to the environment
    modifyIORef' M.defaultCheckerState $ \s ->
        s
            { M.environment =
                Map.insert ann.name (HLIR.Forall ann.typeValue funcType) s.environment
            }

    pure (HLIR.MkTopExternalFunction ann newParams retType)
checkToplevelSingular (HLIR.MkTopImport _) = M.throw (M.CompilerError "Imports are not supported in the typechecker.")
checkToplevelSingular (HLIR.MkTopProperty header params returnType) = do
    -- Removing aliases from the parameter and return types
    paramTypes <- mapM (M.performAliasRemoval . (.typeValue)) params
    retType <- M.performAliasRemoval returnType

    -- Building new parameters and function type
    let funcType = paramTypes HLIR.:->: retType
        newParams = zipWith (\p ty -> p{HLIR.typeValue = ty}) params paramTypes

    -- Adding the property to the environment
    modifyIORef' M.defaultCheckerState $ \s ->
        s
            { M.properties =
                Map.insert header.name (HLIR.Forall header.typeValue funcType) s.properties
            }

    pure (HLIR.MkTopProperty header newParams retType)
checkToplevelSingular (HLIR.MkTopImplementation forType header params returnType body) = do
    -- Find the property being implemented
    scheme <- findPropertyByName header.name

    -- Removing aliases from the parameter, return and for types
    aliasedReturnType <- M.performAliasRemoval returnType
    aliasedParamTypes <- mapM (M.performAliasRemoval . (.typeValue)) params
    aliasedForType <- M.performAliasRemoval forType.typeValue

    -- Building the implementation scheme
    let funcType = (aliasedForType : aliasedParamTypes) HLIR.:->: aliasedReturnType
        implScheme = HLIR.Forall header.typeValue funcType

    -- Checking if the implementation matches the property

    expectedPropType <- M.instantiate scheme >>= M.performAliasRemoval
    expectedImplType <- M.instantiate implScheme >>= M.performAliasRemoval

    void $ expectedPropType `M.isSubtypeOf` expectedImplType

    -- Adding the implementation to the environment
    modifyIORef' M.defaultCheckerState $ \s ->
        s
            { M.implementations =
                Map.insert (header.name, aliasedForType) implScheme s.implementations
            }

    -- Collecting old environment to restore it later
    paramTypes <- mapM (M.performAliasRemoval . (.typeValue)) params
    let newParams = zipWith (\p ty -> p{HLIR.typeValue = ty}) params paramTypes

    -- Building the environment for checking the body
    let env =
            Map.fromList (map ((HLIR.Forall [] <$>) . HLIR.unannotate) newParams)
                <> Map.singleton forType.name (HLIR.Forall [] aliasedForType)

    -- Checking the function body against the return type
    -- We use `withEnvironment` to temporarily extend the environment
    -- with the parameters and for type while checking the body.
    (newBody, cs) <- M.withEnvironment env $ checkE aliasedReturnType body

    -- Solving constraints generated during the body checking
    solveConstraints cs

    pure
        ( HLIR.MkTopImplementation
            forType{HLIR.typeValue = aliasedForType}
            header
            newParams
            aliasedReturnType
            newBody
        )

synthesizeE ::
    (MonadIO m, M.MonadError M.Error m) =>
    HLIR.HLIR "expression" ->
    m (HLIR.Type, HLIR.TLIR "expression", M.Constraints)
synthesizeE (HLIR.MkExprLocated p e) = do
    HLIR.pushPosition p

    (ty, expr, cs) <- synthesizeE e

    void HLIR.popPosition

    pure (ty, HLIR.MkExprLocated p expr, cs)
synthesizeE (HLIR.MkExprLiteral lit) = case lit of
    -- Literal values have known types, so we can directly return them
    -- along with their types and no constraints

    -- Int have type i32 by default
    HLIR.MkLitInt n -> pure (HLIR.MkTyInt, HLIR.MkExprLiteral (HLIR.MkLitInt n), mempty)
    -- Float have type f32 by default
    HLIR.MkLitFloat f -> pure (HLIR.MkTyFloat, HLIR.MkExprLiteral (HLIR.MkLitFloat f), mempty)
    -- Bool have type bool
    HLIR.MkLitBool b -> pure (HLIR.MkTyBool, HLIR.MkExprLiteral (HLIR.MkLitBool b), mempty)
    -- String have type *char
    HLIR.MkLitString s -> pure (HLIR.MkTyString, HLIR.MkExprLiteral (HLIR.MkLitString s), mempty)
    -- Char have type char
    HLIR.MkLitChar c -> pure (HLIR.MkTyChar, HLIR.MkExprLiteral (HLIR.MkLitChar c), mempty)
synthesizeE (HLIR.MkExprVariable ann types) = do
    -- Looking up the variable in the environment
    env <- readIORef M.defaultCheckerState
    let variables = env.environment
        properties = env.properties

    case Map.lookup ann.name variables of
        Just scheme -> do
            -- If variable is found, instantiate its type scheme, and
            -- remove aliases from the instantiated type
            ty <- M.instantiateWithSub scheme types >>= M.performAliasRemoval

            pure (ty, HLIR.MkExprVariable ann{HLIR.typeValue = Identity ty} types, mempty)
        Nothing -> case Map.lookup ann.name properties of
            Just scheme -> do
                -- If variable is a property, we treat it as a regular
                -- variable, but we also add a constraint that it must
                -- be implemented for the given types
                ty <- M.instantiateWithSub scheme types >>= M.performAliasRemoval
                pos <- HLIR.peekPosition'
                pure
                    ( ty
                    , HLIR.MkExprVariable ann{HLIR.typeValue = Identity ty} types
                    , [M.MkImplConstraint ann.name ty pos]
                    )
            Nothing -> M.throw (M.VariableNotFound ann.name)
synthesizeE (HLIR.MkExprCondition cond thenB elseB _) = do
    -- Condition must be of type bool
    (condExpr, cs1) <- checkE HLIR.MkTyBool cond

    -- Then and else branches must have the same type
    -- We first synthesize the type of the then branch,
    -- then check the else branch against that type
    (thenTy, thenExpr, cs2) <- synthesizeE thenB
    (elseExpr, cs3) <- checkE thenTy elseB

    -- Collecting all constraints
    let cs = cs1 <> cs2 <> cs3

    pure
        (thenTy, HLIR.MkExprCondition condExpr thenExpr elseExpr (Identity thenTy), cs)
synthesizeE (HLIR.MkExprLetIn binding value inExpr _) = do
    -- Collecting old environment to restore it later
    oldEnv <- readIORef M.defaultCheckerState <&> M.environment

    -- If the binding has a type annotation, we use it as the expected type
    -- and remove aliases from it. Otherwise, we create a new type variable
    expectedType <- maybe M.newType M.performAliasRemoval binding.typeValue

    -- Adding the binding to the environment
    modifyIORef' M.defaultCheckerState $ \s ->
        s
            { M.environment =
                Map.insert binding.name (HLIR.Forall [] expectedType) s.environment
            }

    -- Checking the value against the expected type
    (valueExpr, cs1) <- checkE expectedType value

    -- Ensuring that the type of the binding is a subtype of the expected type
    forM_ binding.typeValue $ \t ->
        void $ t `M.isSubtypeOf` expectedType

    -- Synthesizing the type of the in expression
    -- The type of the in expression is the type of the whole let-in expression
    -- so we return it as the type of the let-in expression
    (inTy, typedInExpr, cs2) <- synthesizeE inExpr

    -- Restoring the old environment
    modifyIORef' M.defaultCheckerState $ \s ->
        s{M.environment = oldEnv}

    -- Collecting all constraints
    let cs = cs1 <> cs2

    pure
        ( inTy
        , HLIR.MkExprLetIn
            binding{HLIR.typeValue = Identity expectedType}
            valueExpr
            typedInExpr
            (Identity inTy)
        , cs
        )
synthesizeE (HLIR.MkExprLambda params ret body) = do
    -- Removing aliases from the parameter and return types
    paramTypes <- mapM (maybe M.newType M.performAliasRemoval . (.typeValue)) params
    retType <- maybe M.newType M.performAliasRemoval ret

    -- Building parameter annotations with resolved types
    let paramAnnotations = zipWith (\p ty -> p{HLIR.typeValue = Identity ty}) params paramTypes

    -- Collecting old environment to restore it later
    oldEnv <- readIORef M.defaultCheckerState <&> M.environment

    -- Adding parameters to the environment
    modifyIORef' M.defaultCheckerState $ \s ->
        s
            { M.environment =
                foldr
                    ( \p env ->
                        Map.insert p.name (HLIR.Forall [] p.typeValue.runIdentity) env
                    )
                    s.environment
                    paramAnnotations
            }

    -- Checking the body against the return type
    (bodyExpr, cs) <- checkE retType body

    -- Restoring the old environment
    modifyIORef' M.defaultCheckerState $ \s ->
        s
            { M.environment = oldEnv
            }

    -- Building the function type
    let funcType = paramTypes HLIR.:->: retType

    pure
        (funcType, HLIR.MkExprLambda paramAnnotations (Identity retType) bodyExpr, cs)
synthesizeE (HLIR.MkExprApplication callee args) = do
    -- Synthesizing the type of the callee
    (calleeTy, calleeExpr, cs1) <- synthesizeE callee

    case calleeTy of
        HLIR.MkTyFun paramTypes retType -> do
            -- If the callee is a function type, we check that the number of
            -- arguments matches the number of parameters.
            if length paramTypes /= length args
                then M.throw (M.InvalidArgumentQuantity (length paramTypes) (length args))
                else do
                    -- Checking each argument against the corresponding parameter type
                    -- and collecting constraints from each argument.
                    (checkedArgs, cs2) <- unzip <$> zipWithM checkE paramTypes args

                    -- Collecting all constraints
                    let cs = cs1 <> mconcat cs2

                    pure (retType, HLIR.MkExprApplication calleeExpr checkedArgs, cs)
        _ -> do
            -- If the callee is not a function type, we create new type variables
            newParamTypes <- mapM (const M.newType) args
            newRetType <- M.newType

            -- We expect the callee to be a function type with the new parameter
            -- types and new return type.
            let expectedCalleeType = HLIR.MkTyFun newParamTypes newRetType

            -- Checking each argument against the new parameter types
            -- and collecting constraints from each argument.
            (checkedArgs, cs2) <- unzip <$> zipWithM checkE newParamTypes args

            -- We add a constraint that the callee type must be a subtype of
            -- the expected function type.
            void $ calleeTy `M.isSubtypeOf` expectedCalleeType

            -- Collecting all constraints
            let cs = cs1 <> mconcat cs2

            pure (newRetType, HLIR.MkExprApplication calleeExpr checkedArgs, cs)
synthesizeE (HLIR.MkExprStructureCreation ty fields) = do
    -- Removing aliases from the annotated type
    aliasedTy <- M.performAliasRemoval ty

    -- Finding the structure definition by its header, and collecting
    -- it applied type variables.
    let annHeader = getHeader aliasedTy
        annTypes = getTypeArgs aliasedTy

    -- If the structure is not found, we throw an error.
    -- If found, we do not instantiate it, because we need to
    -- manually substitute the type variables with the applied types.
    findStructureMaybeById annHeader >>= \case
        Nothing -> M.throw M.InvalidHeader
        Just (HLIR.Forall qvars structType) -> do
            -- Building a substitution map from the structure's quantified
            -- variables to the applied types. If there are more quantified
            -- variables than applied types, we create new type variables
            -- for the remaining quantified variables.
            let subst = zip qvars annTypes
                rest = drop (length annTypes) qvars
            newVars <- forM rest $ const M.newType
            let substMap = Map.fromList (subst ++ zip rest newVars)

            -- Applying the substitution to the structure's field types
            structTy <-
                Map.traverseWithKey (\_ t -> M.applySubstitution substMap t) structType

            -- Checking each field against the corresponding field type
            -- and collecting constraints from each field.
            checkedFields <- forM (Map.toList fields) $ \(name, expr) -> do
                case Map.lookup name structTy of
                    Just fieldTy -> do
                        (checkedExpr, cs) <- checkE fieldTy expr
                        pure ((name, checkedExpr), cs)
                    Nothing -> M.throw (M.FieldNotFound name)

            -- Collecting all constraints
            let (unzippedFields, cs) = unzip checkedFields

            pure
                ( aliasedTy
                , HLIR.MkExprStructureCreation aliasedTy (Map.fromList unzippedFields)
                , concat cs
                )
synthesizeE (HLIR.MkExprStructureAccess struct field) = do
    -- Synthesizing the type of the structure expression
    (structTy, structExpr, cs) <- synthesizeE struct

    -- Finding the structure definition by its header, and collecting
    -- it applied type variables.
    annHeader <- getHeader <$> M.removeAliases structTy
    findStructureMaybeById annHeader >>= \case
        Nothing -> do
            fieldTy <- M.newType
            pos <- HLIR.peekPosition'

            pure
                ( fieldTy
                , HLIR.MkExprStructureAccess structExpr field
                , cs <> [M.MkFieldConstraint structTy field fieldTy pos]
                )
        Just (HLIR.Forall qvars ty) -> do
            -- Building a substitution map from the structure's quantified
            -- variables to the applied types. And applying the substitution to
            -- the structure's field types.
            let substMap = Map.fromList (zip qvars (getTypeArgs structTy))
            structMap <- Map.traverseWithKey (\_ t -> M.applySubstitution substMap t) ty

            -- Looking up the field in the structure's field types
            -- If found, we return its type and the typed expression.
            -- If not found, we throw an error.
            case Map.lookup field structMap of
                Just fieldTy -> pure (fieldTy, HLIR.MkExprStructureAccess structExpr field, cs)
                Nothing -> M.throw (M.FieldNotFound field)
synthesizeE (HLIR.MkExprDereference e _) = do
    -- Synthesizing the type of the expression to dereference
    -- It must be a pointer type. We also create a fresh type variable
    -- to represent the type being pointed to.
    newType <- M.newType
    (eTy, eExpr, cs) <- synthesizeE e

    -- The expected type is a pointer to the new type variable
    let expectedType = HLIR.MkTyPointer newType

    -- Adding a constraint that the expression type must be a subtype
    -- of the expected pointer type.
    void $ eTy `M.isSubtypeOf` expectedType

    pure (newType, HLIR.MkExprDereference eExpr (Identity newType), cs)
synthesizeE (HLIR.MkExprReference e _) = do
    -- Synthesizing the type of the expression to reference
    -- The resulting type is a pointer to that type.
    (eTy, eExpr, cs) <- synthesizeE e
    let refType = HLIR.MkTyPointer eTy

    pure (refType, HLIR.MkExprReference eExpr (Identity refType), cs)
synthesizeE (HLIR.MkExprUpdate update value _) = do
    -- Synthesizing the type of the update expression
    -- The update expression must be of a type that can be updated,
    -- so we just synthesize its type and use it as the expected type
    -- for the value expression.
    (updateTy, updateExpr, cs1) <- synthesizeE update
    (valueExpr, cs2) <- checkE updateTy value

    -- Collecting all constraints
    let cs = cs1 <> cs2

    pure (updateTy, HLIR.MkExprUpdate updateExpr valueExpr (Identity updateTy), cs)
synthesizeE (HLIR.MkExprSizeOf t) = do
    -- Sizeof expressions have type u64, and we just need to
    -- remove aliases from the type being measured.
    aliasedType <- M.performAliasRemoval t

    pure (HLIR.MkTyId "u64", HLIR.MkExprSizeOf aliasedType, mempty)
synthesizeE (HLIR.MkExprSingleIf cond thenBranch _) = do
    -- Condition must be of type bool
    (condExpr, cs) <- checkE HLIR.MkTyBool cond

    (thenTy, typedThenBranch, cs2) <- synthesizeE thenBranch

    -- The type of the single-if expression is the type of the then branch
    pure
        ( thenTy
        , HLIR.MkExprSingleIf condExpr typedThenBranch (Identity thenTy)
        , cs <> cs2
        )
synthesizeE (HLIR.MkExprCast expr targetTy) = do
    -- Synthesizing the type of the expression to cast
    (exprTy, exprExpr, cs) <- synthesizeE expr

    -- Removing aliases from the target type
    aliasedTargetTy <- M.performAliasRemoval targetTy

    -- Adding a constraint that the expression type must be a subtype
    -- of the target type.
    void $ exprTy `M.isSubtypeOf` aliasedTargetTy

    pure (aliasedTargetTy, HLIR.MkExprCast exprExpr aliasedTargetTy, cs)
synthesizeE (HLIR.MkExprWhile cond body _ inExpr) = do
    -- Condition must be of type bool
    (condExpr, cs1) <- checkE HLIR.MkTyBool cond

    -- Synthesizing the type of the body
    (bodyTy, bodyExpr, cs2) <- synthesizeE body

    -- Synthesizing the type of the in expression
    (inTy, inExprTyped, cs3) <- synthesizeE inExpr

    -- Collecting all constraints
    let cs = cs1 <> cs2 <> cs3

    -- The type of the while expression is the type of the in expression
    pure
        (inTy, HLIR.MkExprWhile condExpr bodyExpr (Identity bodyTy) inExprTyped, cs)

-- | CHECK EXPRESSION
-- | Check an expression against an expected type.
-- | This function synthesizes the type of the expression, and then
-- | checks if the synthesized type is a subtype of the expected type.
-- | It returns the typed expression and any constraints generated during
-- | the synthesis.
checkE ::
    (MonadIO m, M.MonadError M.Error m) =>
    HLIR.Type ->
    HLIR.HLIR "expression" ->
    m (HLIR.TLIR "expression", M.Constraints)
checkE expected expr = do
    (inferredTy, typedExpr, cs) <- synthesizeE expr

    void $ inferredTy `M.isSubtypeOf` expected

    pure (typedExpr, cs)

-- | Find a structure definition by its name.
-- | If the name is `Nothing`, we throw an `InvalidHeader` error.
-- | If the structure is not found, we throw a `StructureNotFound` error.
-- | If found, we return its type scheme.
findStructureMaybeById ::
    (MonadIO m, M.MonadError M.Error m) =>
    Maybe Text -> m (Maybe (HLIR.Scheme (Map Text HLIR.Type)))
findStructureMaybeById name = do
    structTypes <- readIORef M.defaultCheckerState <&> M.structures

    case name of
        Just n -> pure $ Map.lookup n structTypes
        Nothing -> pure Nothing

-- | SOLVE CONSTRAINTS
-- | Solve a list of constraints by finding suitable implementations
-- | from the environment and ensuring that the implementation types
-- | are subtypes of the required types.
-- |
-- | This function modifies the type checker state by checking the
-- | implementations.
-- |
-- | If a constraint cannot be satisfied, it is ignored, as it may
-- | be handled later during code generation or runtime.
solveConstraints :: (MonadIO m, M.MonadError M.Error m) => M.Constraints -> m ()
solveConstraints constraints = do
    -- Getting the current implementations from the state
    implementations <-
        Map.toList <$> (readIORef M.defaultCheckerState <&> M.implementations)
    structures <- readIORef M.defaultCheckerState <&> Map.toList . M.structures

    -- For each constraint, we try to find a matching implementation
    -- If found, we affine the types by ensuring that the implementation
    -- type is a subtype of the required type.
    forM_ constraints $ \case
        M.MkImplConstraint name ty pos -> do
            mImplTy <- findImplementationMatching implementations name ty pos
            case mImplTy of
                Just implTy -> void $ implTy `M.isSubtypeOf` ty
                Nothing -> pure () -- Ignoring unsolved constraints
        M.MkFieldConstraint structTy fieldName fieldTy pos -> do
            header <- getHeader <$> M.removeAliases structTy

            case header of
                Just resolved ->
                    findStructureMatching structures resolved pos >>= \case
                        Just structMap -> case Map.lookup fieldName structMap of
                            Just expectedFieldTy -> void $ expectedFieldTy `M.isSubtypeOf` fieldTy
                            Nothing -> pure () -- Ignoring unsolved constraints
                        Nothing -> pure () -- Ignoring unsolved constraints
                Nothing -> pure () -- Ignoring unsolved constraints
  where
    findStructureMatching ::
        (MonadIO m, M.MonadError M.Error m) =>
        [(Text, HLIR.Scheme (Map Text HLIR.Type))] ->
        Text ->
        HLIR.Position ->
        m (Maybe (Map Text HLIR.Type))
    findStructureMatching [] _ _ = pure Nothing
    findStructureMatching ((structName, scheme) : xs) name pos
        | structName == name = do
            -- We instantiate the structure scheme to get a concrete type
            structTy <- M.instantiateMap scheme >>= mapM M.performAliasRemoval
            pure (Just structTy)
        | otherwise = findStructureMatching xs name pos

    findImplementationMatching ::
        (MonadIO m, M.MonadError M.Error m) =>
        [((Text, HLIR.Type), HLIR.Scheme HLIR.Type)] ->
        Text ->
        HLIR.Type ->
        HLIR.Position ->
        m (Maybe HLIR.Type)
    findImplementationMatching [] _ _ _ = pure Nothing
    findImplementationMatching (((implName, _), scheme) : xs) name ty pos
        | implName == name = do
            -- We instantiate the implementation scheme to get a concrete type
            -- and remove aliases from it.
            implTy <- M.instantiate scheme >>= M.performAliasRemoval

            -- We check if the implementation type is a subtype of the required type
            -- If it is, we return it as a matching implementation.
            result <- runExceptT $ M.applySubtypeRelation False implTy ty

            case result of
                Right _ -> pure (Just implTy)
                Left _ -> findImplementationMatching xs name ty pos
        | otherwise = findImplementationMatching xs name ty pos

-- | Extract the header (base type) from a type.
-- | For example, for `MyStruct[i32, bool]`, it returns `Just "MyStruct"`.
-- | For `MyStruct`, it returns `Just "MyStruct"`.
-- | For `i32`, it returns `Just "i32"`.
-- | For `i32[i32]`, it returns `Just "i32"`.
-- | For `i32 -> bool`, it returns `Nothing`.
getHeader :: HLIR.Type -> Maybe Text
getHeader (HLIR.MkTyApp (HLIR.MkTyId name) _) = Just name
getHeader (HLIR.MkTyId name) = Just name
getHeader _ = Nothing

-- | Extract type arguments from a type application.
-- | If the type is not a type application, return an empty list.
-- | This is used to get the applied types of a structure type.
-- | For example, for `MyStruct[i32, bool]`, it returns `[i32, bool]`.
-- | For `MyStruct`, it returns `[]`.
-- | For `i32`, it returns `[]`.
getTypeArgs :: HLIR.Type -> [HLIR.Type]
getTypeArgs (HLIR.MkTyApp _ args) = args
getTypeArgs _ = []

-- | Find a property definition by its name.
-- | If the property is not found, we throw a `PropertyNotFound` error.
-- | If found, we return its type scheme.
findPropertyByName ::
    (MonadIO m, M.MonadError M.Error m) => Text -> m (HLIR.Scheme HLIR.Type)
findPropertyByName name = do
    -- Looking up the property in the environment
    properties <- readIORef M.defaultCheckerState <&> M.properties

    -- Throwing an error if the property is not found
    case Map.lookup name properties of
        Just scheme -> pure scheme
        Nothing -> M.throw (M.PropertyNotFound name)

-- | Remove the third element from a triple.
-- | This is used to convert constraints from (name, type, position)
-- | to (name, type).
removeThird :: (a, b, c) -> (a, b)
removeThird (x, y, _) = (x, y)
