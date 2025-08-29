{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Reality.Backend.Specialization.Resolver where

import Control.Monad.Except qualified as M
import Control.Monad.Result qualified as M
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Traversable qualified as Trav
import GHC.IO qualified as IO
import Language.Reality.Frontend.Typechecker.Monad qualified as M
import Language.Reality.Frontend.Typechecker.Unification qualified as M
import Language.Reality.Syntax.HLIR qualified as HLIR

-- | SPECIALIZATION RESOLVER
-- | Resolve specializations in a program.
-- | This function takes a list of toplevel nodes, and returns a list of toplevel
-- | nodes with specializations resolved.
-- | It replaces specialized function calls with their specialized versions.
-- | If a specialized version does not exist, it throws an error.
runSpecializationResolver ::
    (MonadIO m, M.MonadError M.Error m) =>
    [HLIR.TLIR "toplevel"] ->
    m [HLIR.TLIR "toplevel"]
runSpecializationResolver toplevels = do
    resolved <- forM toplevels $ \n -> do
        (resolvedNode, newDefs) <- resolveSpecializationSingular n
        pure (newDefs ++ maybeToList resolvedNode)

    pure (concat resolved)

-- | Resolve specialization in singular toplevel nodes.
-- | This function takes a toplevel node, and returns a toplevel node with
-- | specializations resolved.
resolveSpecializationSingular ::
    (MonadIO m, M.MonadError M.Error m) =>
    HLIR.TLIR "toplevel" ->
    m (Maybe (HLIR.TLIR "toplevel"), [HLIR.TLIR "toplevel"])
resolveSpecializationSingular (HLIR.MkTopLocated p n) = do
    HLIR.pushPosition p
    (resolved, newDefs) <- resolveSpecializationSingular n
    void HLIR.popPosition
    pure (HLIR.MkTopLocated p <$> resolved, newDefs)
resolveSpecializationSingular (HLIR.MkTopConstantDeclaration ann expr) = do
    (typedExpr, newDefs) <- resolveSpecializationInExpr expr
    pure (Just $ HLIR.MkTopConstantDeclaration ann typedExpr, newDefs)
resolveSpecializationSingular node@(HLIR.MkTopFunctionDeclaration ann params ret body)
    -- If no generics are found, we can directly resolve the function body
    -- and return the typed function declaration.
    | null ann.typeValue = do
        (specParams, newDefs) <-
            mapAndUnzipM
                ( \case
                    HLIR.MkAnnotation name ty -> do
                        (specTy, newDefs) <- resolveSpecializationInType ty
                        pure (HLIR.MkAnnotation name specTy, newDefs)
                )
                params
        (specRet, newDefs2) <- resolveSpecializationInType ret
        (typedBody, newDefs3) <- resolveSpecializationInExpr body

        let allNewDefs = concat newDefs ++ newDefs2 ++ newDefs3

        pure
            ( Just $ HLIR.MkTopFunctionDeclaration ann specParams specRet typedBody
            , allNewDefs
            )

    -- Otherwise, we register the function in the specialization state
    -- to be used later when resolving specialized calls.
    | otherwise = do
        let paramTypes = map (.typeValue) params
            funcType = paramTypes HLIR.:->: ret

        let scheme = HLIR.Forall ann.typeValue funcType

        modifyIORef' defaultSpecializer $ \s ->
            s{variables = Map.insert ann.name (scheme, node) s.variables}

        pure (Nothing, [])
resolveSpecializationSingular (HLIR.MkTopPublic n) = do
    (resolved, newDefs) <- resolveSpecializationSingular n
    pure (HLIR.MkTopPublic <$> resolved, newDefs)
resolveSpecializationSingular (HLIR.MkTopImplementation forType header parameters returnType body) = do
    let scheme =
            HLIR.Forall
                header.typeValue
                ((forType.typeValue : map (.typeValue) parameters) HLIR.:->: returnType)

    let newParams = forType : parameters

    modifyIORef' defaultSpecializer $ \s ->
        s
            { implementations =
                Map.insert
                    (header.name, scheme)
                    (HLIR.MkTopFunctionDeclaration header newParams returnType body)
                    s.implementations
            }

    pure (Nothing, [])
resolveSpecializationSingular (HLIR.MkTopProperty header parameters returnType) = do
    let scheme =
            HLIR.Forall header.typeValue (map (.typeValue) parameters HLIR.:->: returnType)

    modifyIORef' defaultSpecializer $ \s ->
        s{properties = Map.insert header.name scheme s.properties}

    pure (Just $ HLIR.MkTopProperty header parameters returnType, [])
resolveSpecializationSingular other = pure (Just other, [])

-- | Resolve specialization in expressions.
-- | This function takes an expression, and returns an expression with
-- | specializations resolved.
-- | It replaces specialized function calls with their specialized versions.
-- | If a specialized version does not exist, it throws an error.
resolveSpecializationInExpr ::
    (MonadIO m, M.MonadError M.Error m) =>
    HLIR.TLIR "expression" ->
    m (HLIR.TLIR "expression", [HLIR.TLIR "toplevel"])
resolveSpecializationInExpr (HLIR.MkExprLocated p e) = do
    HLIR.pushPosition p
    (resolved, newDefs) <- resolveSpecializationInExpr e
    void HLIR.popPosition
    pure (HLIR.MkExprLocated p resolved, newDefs)
resolveSpecializationInExpr (HLIR.MkExprVariable ann _) = do
    (specAnn, defs) <- resolveSpecializationForIdentifier ann

    pure (HLIR.MkExprVariable specAnn [], defs)
resolveSpecializationInExpr (HLIR.MkExprLiteral lit) = do
    pure (HLIR.MkExprLiteral lit, [])
resolveSpecializationInExpr (HLIR.MkExprLambda params ret body) = do
    (specParams, newDefs) <-
        mapAndUnzipM
            ( \case
                HLIR.MkAnnotation name ty -> do
                    (specTy, newDefs) <- resolveSpecializationInType ty.runIdentity
                    pure (HLIR.MkAnnotation name (Identity specTy), newDefs)
            )
            params
    (specRet, newDefs2) <-
        first Identity <$> resolveSpecializationInType ret.runIdentity

    (typedBody, newDefs3) <- resolveSpecializationInExpr body

    let allNewDefs = concat newDefs ++ newDefs2 ++ newDefs3

    pure (HLIR.MkExprLambda specParams specRet typedBody, allNewDefs)
resolveSpecializationInExpr (HLIR.MkExprLetIn binding value inExpr) = do
    (specBinding, newDefs) <- case binding of
        HLIR.MkAnnotation name ty -> do
            (specTy, newDefs) <- resolveSpecializationInType ty.runIdentity
            pure (HLIR.MkAnnotation name (Identity specTy), newDefs)
    (typedValue, newDefs1) <- resolveSpecializationInExpr value
    (typedInExpr, newDefs2) <- resolveSpecializationInExpr inExpr

    let allNewDefs = newDefs ++ newDefs1 ++ newDefs2

    pure (HLIR.MkExprLetIn specBinding typedValue typedInExpr, allNewDefs)
resolveSpecializationInExpr (HLIR.MkExprCondition cond thenB elseB) = do
    (typedCond, newDefs1) <- resolveSpecializationInExpr cond
    (typedThen, newDefs2) <- resolveSpecializationInExpr thenB
    (typedElse, newDefs3) <- resolveSpecializationInExpr elseB

    pure
        ( HLIR.MkExprCondition typedCond typedThen typedElse
        , newDefs1 ++ newDefs2 ++ newDefs3
        )
resolveSpecializationInExpr (HLIR.MkExprApplication callee args) = do
    (typedCallee, newDefs1) <- resolveSpecializationInExpr callee
    (typedArgs, newDefs2) <- mapAndUnzipM resolveSpecializationInExpr args

    pure (HLIR.MkExprApplication typedCallee typedArgs, newDefs1 ++ concat newDefs2)
resolveSpecializationInExpr (HLIR.MkExprStructureAccess struct field) = do
    (typedStruct, newDefs) <- resolveSpecializationInExpr struct
    pure (HLIR.MkExprStructureAccess typedStruct field, newDefs)
resolveSpecializationInExpr (HLIR.MkExprStructureCreation ann fields) = do
    (specAnn, newDefs) <- resolveSpecializationInType ann
    (typedFields, newDefs2) <-
        mapAndUnzipM
            ( \(name, expr) -> do
                (typedExpr, defs) <- resolveSpecializationInExpr expr
                pure ((name, typedExpr), defs)
            )
            (Map.toList fields)

    let fieldMap = Map.fromList typedFields
        allNewDefs2 = concat newDefs2
        allNewDefs = newDefs ++ allNewDefs2

    pure (HLIR.MkExprStructureCreation specAnn fieldMap, allNewDefs)
resolveSpecializationInExpr (HLIR.MkExprDereference e) = do
    (typedE, newDefs) <- resolveSpecializationInExpr e
    pure (HLIR.MkExprDereference typedE, newDefs)
resolveSpecializationInExpr (HLIR.MkExprReference e) = do
    (typedE, newDefs) <- resolveSpecializationInExpr e
    pure (HLIR.MkExprReference typedE, newDefs)
resolveSpecializationInExpr (HLIR.MkExprUpdate update value) = do
    (typedUpdate, newDefs1) <- resolveSpecializationInExpr update
    (typedValue, newDefs2) <- resolveSpecializationInExpr value
    pure (HLIR.MkExprUpdate typedUpdate typedValue, newDefs1 ++ newDefs2)
resolveSpecializationInExpr (HLIR.MkExprSizeOf t) = do
    (specTy, newDefs) <- resolveSpecializationInType t
    pure (HLIR.MkExprSizeOf specTy, newDefs)

-- | Apply a substitution to all types in an expression.
-- | This function takes a substitution map and an expression, and returns
-- | an expression with the substitution applied to all types.
-- | The expected behavior is that all types in the expression are replaced
-- | according to the substitution map.
applySubstInExpr ::
    (MonadIO m) =>
    Map Text HLIR.Type -> HLIR.TLIR "expression" -> m (HLIR.TLIR "expression")
applySubstInExpr subst (HLIR.MkExprLocated p e) =
    HLIR.MkExprLocated p <$> applySubstInExpr subst e
applySubstInExpr subst (HLIR.MkExprVariable ann@(HLIR.MkAnnotation _ ty) types) = do
    newType <- M.applySubstitution subst ty.runIdentity
    pure (HLIR.MkExprVariable (ann{HLIR.typeValue = Identity newType}) types)
applySubstInExpr _ (HLIR.MkExprLiteral lit) = pure (HLIR.MkExprLiteral lit)
applySubstInExpr subst (HLIR.MkExprLambda params ret body) = do
    newParams <- forM params $ \(HLIR.MkAnnotation name ty) -> do
        newTy <- M.applySubstitution subst ty.runIdentity
        pure (HLIR.MkAnnotation name (Identity newTy))
    newRet <- M.applySubstitution subst ret.runIdentity
    newBody <- applySubstInExpr subst body
    pure (HLIR.MkExprLambda newParams (Identity newRet) newBody)
applySubstInExpr subst (HLIR.MkExprLetIn binding value inExpr) =
    do
        newBinding <- case binding of
            HLIR.MkAnnotation name ty -> do
                newTy <- M.applySubstitution subst ty.runIdentity
                pure (HLIR.MkAnnotation name (Identity newTy))
        newValue <- applySubstInExpr subst value
        newInExpr <- applySubstInExpr subst inExpr
        pure (HLIR.MkExprLetIn newBinding newValue newInExpr)
applySubstInExpr subst (HLIR.MkExprCondition cond thenB elseB) =
    do
        newCond <- applySubstInExpr subst cond
        newThenB <- applySubstInExpr subst thenB
        newElseB <- applySubstInExpr subst elseB

        pure (HLIR.MkExprCondition newCond newThenB newElseB)
applySubstInExpr subst (HLIR.MkExprApplication callee args) = do
    newCallee <- applySubstInExpr subst callee
    newArgs <- mapM (applySubstInExpr subst) args
    pure (HLIR.MkExprApplication newCallee newArgs)
applySubstInExpr subst (HLIR.MkExprStructureAccess struct field) = do
    newStruct <- applySubstInExpr subst struct
    pure (HLIR.MkExprStructureAccess newStruct field)
applySubstInExpr subst (HLIR.MkExprStructureCreation ann fields) = do
    newAnn <- M.applySubstitution subst ann
    newFields <- mapM (applySubstInExpr subst) fields
    pure (HLIR.MkExprStructureCreation newAnn newFields)
applySubstInExpr subst (HLIR.MkExprDereference e) = do
    newE <- applySubstInExpr subst e
    pure (HLIR.MkExprDereference newE)
applySubstInExpr subst (HLIR.MkExprReference e) = do
    newE <- applySubstInExpr subst e
    pure (HLIR.MkExprReference newE)
applySubstInExpr subst (HLIR.MkExprUpdate update value) = do
    newUpdate <- applySubstInExpr subst update
    newValue <- applySubstInExpr subst value
    pure (HLIR.MkExprUpdate newUpdate newValue)
applySubstInExpr subst (HLIR.MkExprSizeOf t) = do
    newT <- M.applySubstitution subst t
    pure (HLIR.MkExprSizeOf newT)

-- | Resolve specialization for identifiers.
-- | This function takes an identifier and a associated type
-- | (which may be specialized), and returns an expression with the
-- | specialization resolved.
resolveSpecializationForIdentifier ::
    (MonadIO m, M.MonadError M.Error m) =>
    HLIR.Annotation (Identity HLIR.Type) ->
    m (HLIR.Annotation (Identity HLIR.Type), [HLIR.TLIR "toplevel"])
resolveSpecializationForIdentifier (HLIR.MkAnnotation name (Identity ty)) = do
    specState <- liftIO $ readIORef defaultSpecializer

    -- Fetching the variable scheme if it exists, and also the toplevel
    -- declaration associated with it.
    case Map.lookup name (variables specState) of
        Just (scheme@(HLIR.Forall qvars _), toplevel) -> do
            -- Instantiating the scheme to get a concrete type and a substitution
            -- that maps the quantified variables to concrete types.
            (schemeType, subst) <- M.instantiateAndSub scheme
            void $ ty `M.isSubtypeOf` schemeType

            -- Checking if the toplevel is a function declaration.
            -- If it is not, there must be an error somewhere else, because
            -- only functions can be specialized as they're the only nodes
            -- to capture generics.
            case toplevel of
                HLIR.MkTopFunctionDeclaration
                    { parameters
                    , returnType
                    , body
                    , name = _
                    } -> do
                        -- Re-ordering the resolved types according to the scheme quantified
                        -- variables to create a consistent name
                        -- (e.g., f_a_b and f_b_a should not refer to the same specialization)
                        -- This is important to avoid duplicating work and creating
                        -- wrong specializations.
                        let orderedVars = flip map qvars $ \var -> Map.findWithDefault (HLIR.MkTyQuantified var) var subst
                            newName = name <> "_" <> Text.intercalate "_" (map toText orderedVars)

                        -- Checking if we already created this specialization
                        -- to avoid duplicating work.
                        if Set.member newName specState.rememberedVariables
                            then do
                                let newAnn = HLIR.MkAnnotation newName (Identity ty)
                                pure (newAnn, [])
                            else do
                                -- Marking this specialization as created
                                modifyIORef' defaultSpecializer $ \s ->
                                    s{rememberedVariables = Set.insert newName s.rememberedVariables}

                                -- Applying the substitution to the parameters, return type, and body
                                -- of the function.
                                -- This creates the specialized version of the function.
                                -- Note that we also need to recursively resolve specializations
                                -- in the body, as it may contain calls to other specialized functions.
                                -- This ensures that the entire function is correctly specialized.
                                specParameters <- Trav.for parameters $ \(HLIR.MkAnnotation paramName paramType) -> do
                                    newParamType <- M.applySubstitution subst paramType
                                    pure (HLIR.MkAnnotation paramName newParamType)

                                specReturnType <- M.applySubstitution subst returnType

                                newBody <- applySubstInExpr subst body
                                (specBody, newDefs) <- resolveSpecializationInExpr newBody

                                -- Creating the new specialized function declaration
                                -- with the new name and specialized types.
                                let newFunction =
                                        HLIR.MkTopFunctionDeclaration
                                            { HLIR.name = HLIR.MkAnnotation newName []
                                            , HLIR.parameters = specParameters
                                            , HLIR.returnType = specReturnType
                                            , HLIR.body = specBody
                                            }

                                -- Combining all new definitions obtained during the resolution
                                -- including the new specialized function.
                                let allNewDefs = newDefs ++ [newFunction]

                                -- Building the new annotation type for the specialized function.
                                -- This is important for further type checking and resolution.
                                let funcType = map (.typeValue) specParameters HLIR.:->: specReturnType

                                pure (HLIR.MkAnnotation newName (Identity funcType), allNewDefs)
                _ -> M.throw (M.VariableNotFound name)
        Nothing -> resolveSpecializationForImplementation name ty

resolveSpecializationForImplementation ::
    (MonadIO m, M.MonadError M.Error m) =>
    Text ->
    HLIR.Type ->
    m (HLIR.Annotation (Identity HLIR.Type), [HLIR.TLIR "toplevel"])
resolveSpecializationForImplementation name ty = do
    specState <- liftIO $ readIORef defaultSpecializer

    -- Look for an implementation that matches the name and type
    -- Algorithm:
    -- 1. For each implementation with the same name
    -- 2. Instantiate its scheme to get a concrete type and a substitution
    -- 3. Check if the concrete type is a subtype of the given type
    -- 4. If it is, return the implementation, substitution, and scheme
    -- 5. If none match, return Nothing
    result1 <-
        findImplementationMatching (Map.toList specState.implementations) name ty

    -- Also look for a property with the same name
    -- This is used to create the correct name according to the
    -- property type variables.
    let result2 = Map.lookup name specState.properties

    -- Combining both results to get the final result
    let result = case (result1, result2) of
            (Just (node, subst, scheme), Just propScheme) ->
                Just (node, subst, scheme, propScheme)
            _ -> Nothing

    case result of
        Just (node, implSubst, _, propScheme@(HLIR.Forall qvars _)) -> do
            -- Creating the final substitution by instantiating the property scheme
            -- and ensuring that the implementation type is a subtype of the property type
            (propTy, subst) <- M.instantiateAndSub propScheme
            void $ ty `M.isSubtypeOf` propTy

            -- Re-ordering the resolved types according to the property scheme quantified
            -- variables to create a consistent name
            let orderedVars = flip map qvars $ \var -> Map.findWithDefault (HLIR.MkTyQuantified var) var subst
                newName = name <> "_" <> Text.intercalate "_" (map toText orderedVars)

            -- Checking if the node is a function declaration, otherwise there must
            -- be an error somewhere else. We just handle it gracefully here.
            case node of
                HLIR.MkTopFunctionDeclaration
                    { parameters
                    , returnType
                    , body
                    , name = _
                    } -> do
                        -- Checking if we already created this specialization
                        -- to avoid duplicating work.
                        if Set.member newName specState.rememberedImplementations
                            then pure (HLIR.MkAnnotation newName (Identity ty), [])
                            else do
                                -- Marking this specialization as created
                                -- to avoid duplicating work in the future.
                                modifyIORef' defaultSpecializer $ \s ->
                                    s{rememberedImplementations = Set.insert newName s.rememberedImplementations}

                                -- Applying the implementation substitution to the parameters,
                                -- return type, and body of the function.
                                specParameters <- Trav.for parameters $ \(HLIR.MkAnnotation paramName paramType) -> do
                                    newParamType <- M.applySubstitution implSubst paramType
                                    pure (HLIR.MkAnnotation paramName newParamType)
                                specReturnType <- M.applySubstitution implSubst returnType

                                newBody <- applySubstInExpr implSubst body

                                -- Recursively resolving specializations in the body
                                -- in case there are nested specializations.
                                -- This also ensures that any new toplevel definitions
                                -- created during the resolution are collected.
                                -- This is important to maintain the integrity of the program.
                                (specBody, newDefs) <- resolveSpecializationInExpr newBody

                                -- Creating the new specialized function declaration
                                -- with the new name and specialized types.
                                -- Note that implementations become regular functions,
                                -- ensuring better performance and inlining opportunities.
                                let newImpl =
                                        HLIR.MkTopFunctionDeclaration
                                            { HLIR.name = HLIR.MkAnnotation newName []
                                            , HLIR.parameters = specParameters
                                            , HLIR.returnType = specReturnType
                                            , HLIR.body = specBody
                                            }

                                -- Combining all new definitions obtained during the resolution
                                -- including the new specialized function.
                                let allNewDefs = newDefs ++ [newImpl]

                                -- Building the new annotation type for the specialized function.
                                -- This is important for further type checking and resolution.
                                let funcType = map (.typeValue) specParameters HLIR.:->: specReturnType

                                pure (HLIR.MkAnnotation newName (Identity funcType), allNewDefs)
                _ -> M.throw (M.ImplementationNotFound name ty)
        Nothing -> pure (HLIR.MkAnnotation name (Identity ty), [])
  where
    -- The actual implementation matching algorithm
    -- as described above.
    -- It recursively checks each implementation
    -- until it finds a match or exhausts the list.
    -- If a match is found, it returns the implementation,
    -- the substitution, and the scheme.
    -- If none match, it returns Nothing.
    --
    -- This function is crucial for resolving
    -- specialized function calls correctly.
    -- It ensures that the most appropriate implementation
    -- is selected based on the provided type.
    findImplementationMatching ::
        (MonadIO m, M.MonadError M.Error m) =>
        [((Text, HLIR.Scheme HLIR.Type), HLIR.TLIR "toplevel")] ->
        Text ->
        HLIR.Type ->
        m (Maybe (HLIR.TLIR "toplevel", M.Substitution, HLIR.Scheme HLIR.Type))
    findImplementationMatching [] _ _ = pure Nothing
    findImplementationMatching (((implName, implScheme), toplevel) : xs) varName ty'
        | implName == varName = do
            -- Instantiating the implementation scheme to get a concrete type
            -- and a substitution map.
            -- Then performing alias removal to simplify the type.
            (implTy, sub) <- M.instantiateAndSub implScheme
            aliasedImplTy <- M.performAliasRemoval implTy

            -- Checking if the aliased implementation type is a subtype of the given type
            -- without performing any unification.
            -- This is done to ensure that the implementation can be used
            -- for the specialized function call.
            result <- runExceptT $ M.applySubtypeRelation False aliasedImplTy ty'

            case result of
                Right _ -> do
                    -- If it is a subtype, we have found a matching implementation.
                    -- We can fully apply subtype relation to get the final substitution.
                    void $ aliasedImplTy `M.isSubtypeOf` ty'
                    pure $ Just (toplevel, sub, implScheme)
                Left _ -> findImplementationMatching xs name ty'
        | otherwise = findImplementationMatching xs name ty'

-- | Resolve specialization in types.
-- | This function takes a type, and returns a type with specializations resolved.
-- | It replaces specialized type applications with their specialized versions.
-- | If a specialized version does not exist, it throws an error.
resolveSpecializationInType ::
    (MonadIO m, M.MonadError M.Error m) =>
    HLIR.Type ->
    m (HLIR.Type, [HLIR.TLIR "toplevel"])
resolveSpecializationInType (HLIR.MkTyApp (HLIR.MkTyId base) args) =
    maybeResolveStructure base args
resolveSpecializationInType (HLIR.MkTyId name) =
    maybeResolveStructure name []
resolveSpecializationInType (HLIR.MkTyApp base args) = do
    (typedBase, newDefs1) <- resolveSpecializationInType base
    (typedArgs, newDefs2) <- mapAndUnzipM resolveSpecializationInType args

    pure (HLIR.MkTyApp typedBase typedArgs, newDefs1 ++ concat newDefs2)
resolveSpecializationInType (HLIR.MkTyVar ref) = do
    ty <- liftIO $ readIORef ref
    case ty of
        HLIR.Link ty' -> resolveSpecializationInType ty'
        HLIR.Unbound{} -> pure (HLIR.MkTyVar ref, [])
resolveSpecializationInType (HLIR.MkTyQuantified name) = do
    pure (HLIR.MkTyQuantified name, [])
resolveSpecializationInType (HLIR.MkTyAnonymousStructure fields) = do
    (typedFields, newDefs) <-
        mapAndUnzipM
            ( \(name, ty) -> do
                (typedTy, defs) <- resolveSpecializationInType ty
                pure ((name, typedTy), defs)
            )
            (Map.toList fields)
    let fieldMap = Map.fromList typedFields
        allNewDefs = concat newDefs

    pure (HLIR.MkTyAnonymousStructure fieldMap, allNewDefs)

maybeResolveStructure ::
    (MonadIO m, M.MonadError M.Error m) =>
    Text ->
    [HLIR.Type] ->
    m (HLIR.Type, [HLIR.TLIR "toplevel"])
maybeResolveStructure name args = do
    specState <- liftIO $ readIORef defaultSpecializer

    let header
            | null args = HLIR.MkTyId name
            | otherwise = HLIR.MkTyApp (HLIR.MkTyId name) args

    case Map.lookup name specState.structures of
        Just scheme -> do
            (instMap, sub) <- M.instantiateMapAndSub scheme

            when (Map.size instMap /= length args)
                $ M.throw (M.InvalidArgumentQuantity (Map.size instMap) (length args))

            let s = Map.fromList (zip (Map.keys instMap) args) <> sub

            specializedFields <-
                Map.traverseWithKey (\_ ty -> M.applySubstitution s ty) instMap

            let specName = name <> "<" <> Text.intercalate "," (map toText args) <> ">"

            if Set.member specName specState.rememberedStructures
                then pure (HLIR.MkTyId specName, [])
                else do
                    let newStruct =
                            HLIR.MkTopStructureDeclaration
                                { HLIR.header = HLIR.MkAnnotation specName []
                                , HLIR.fields = specializedFields
                                }

                    let newSpecializer =
                            specState
                                { structures =
                                    Map.insert specName (HLIR.Forall [] specializedFields) specState.structures
                                , rememberedStructures = Set.insert specName specState.rememberedStructures
                                }

                    liftIO $ writeIORef defaultSpecializer newSpecializer

                    pure (HLIR.MkTyId specName, [newStruct])
        Nothing -> pure (header, [])

-- | Utility types
data Specializer = Specializer
    { variables :: Map Text (HLIR.Scheme HLIR.Type, HLIR.TLIR "toplevel")
    , structures :: Map Text (HLIR.Scheme (Map Text HLIR.Type))
    , implementations :: Map (Text, HLIR.Scheme HLIR.Type) (HLIR.TLIR "toplevel")
    , properties :: Map Text (HLIR.Scheme HLIR.Type)
    , rememberedVariables :: Set Text
    , rememberedStructures :: Set Text
    , rememberedImplementations :: Set Text
    }
    deriving (Ord, Eq)

emptySpecializer :: Specializer
emptySpecializer =
    Specializer
        Map.empty
        Map.empty
        Map.empty
        Map.empty
        Set.empty
        Set.empty
        Set.empty

defaultSpecializer :: IORef Specializer
defaultSpecializer = IO.unsafePerformIO $ newIORef emptySpecializer
