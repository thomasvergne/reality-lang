{-# LANGUAGE LambdaCase #-}

module Language.Reality.Frontend.Typechecker.Checker where

import Control.Monad.Except qualified as M
import Control.Monad.Result qualified as M
import Data.Map qualified as Map
import Language.Reality.Frontend.Typechecker.Monad qualified as M
import Language.Reality.Frontend.Typechecker.Unification qualified as M
import Language.Reality.Syntax.HLIR qualified as HLIR
import qualified Data.List as List
import qualified Data.Foldable as List
import qualified Data.Set as Set

withTypeVars ::
    (MonadIO m) =>
    [Text] ->
    m a ->
    m a
withTypeVars typeVars action = do   
    oldState <- readIORef M.defaultCheckerState

    -- Adding type variables to the state
    modifyIORef' M.defaultCheckerState $ \s ->
        s{M.typeVariables = Set.union (Set.fromList typeVars) s.typeVariables}

    result <- action

    -- Restoring old state
    modifyIORef' M.defaultCheckerState $ \s ->
        s{M.typeVariables = oldState.typeVariables}

    pure result

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

isArgsAnnotation :: [HLIR.Annotation HLIR.Type] -> Bool
isArgsAnnotation [x] | x.name == "args" && x.typeValue == HLIR.MkTyList (HLIR.MkTyId "String") = True
isArgsAnnotation _ = False

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
    (typedExpr, cs, _) <- checkE expectedType expr

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
checkToplevelSingular (HLIR.MkTopFunctionDeclaration ann params ret body) = withTypeVars ann.typeValue $ do
    -- Removing aliases from the parameter and return types
    paramTypes <- mapM (M.performAliasRemoval . (.typeValue)) params
    retType <- M.performAliasRemoval ret

    -- Building new parameters and function type
    let newParams =
            if ann.name == "main"
                then
                    [ HLIR.MkAnnotation "argc" HLIR.MkTyInt
                    , HLIR.MkAnnotation
                        "argv"
                        (HLIR.MkTyPointer (HLIR.MkTyPointer HLIR.MkTyChar))
                    ]
                else zipWith (\p ty -> p{HLIR.typeValue = ty}) params paramTypes
        funcType = paramTypes HLIR.:->: retType

    let argListType = HLIR.MkTyList (HLIR.MkTyId "String")
        argc = HLIR.MkExprVariable (HLIR.MkAnnotation "argc" (Just HLIR.MkTyInt)) []
        argv =
            HLIR.MkExprVariable
                ( HLIR.MkAnnotation
                    "argv"
                    (Just (HLIR.MkTyPointer (HLIR.MkTyPointer HLIR.MkTyChar)))
                )
                []
        getArgsType =
            [HLIR.MkTyInt, HLIR.MkTyPointer (HLIR.MkTyPointer HLIR.MkTyChar)]
                HLIR.:->: argListType

    let body'
            | ann.name == "main" && isArgsAnnotation params =
                HLIR.MkExprLetIn
                    (HLIR.MkAnnotation "args" (Just argListType))
                    ( HLIR.MkExprApplication
                        ( HLIR.MkExprVariable
                            (HLIR.MkAnnotation "getArgs" (Just getArgsType))
                            []
                        )
                        [argc, argv]
                        (Just argListType)
                    )
                    body
                    Nothing
            | otherwise = body

    -- Adding the function to the environment, before adding arguments,
    -- to allow for restoration of the environment without deleting the
    -- function itself.
    modifyIORef' M.defaultCheckerState $ \s ->
        s
            { M.environment =
                Map.insert ann.name (HLIR.Forall ann.typeValue funcType) s.environment
            }

    -- Collecting old environment to restore it later
    oldEnv <- readIORef M.defaultCheckerState

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
            , M.returnType = Just retType
            }

    -- Checking the function body against the return type
    (typedBody, cs, _) <- checkE retType body'

    -- Solving constraints generated during the body checking
    solveConstraints cs

    -- Restoring the old environment
    modifyIORef' M.defaultCheckerState $ \s ->
        s
            { M.environment = oldEnv.environment
            , M.returnType = oldEnv.returnType
            }

    pure (HLIR.MkTopFunctionDeclaration ann newParams retType typedBody)
checkToplevelSingular (HLIR.MkTopTypeAlias ann aliased) = withTypeVars ann.typeValue $ do
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
checkToplevelSingular (HLIR.MkTopStructureDeclaration ann fields) = withTypeVars ann.typeValue $ do
    -- Removing aliases from the field types
    fieldTypes <- mapM (mapM M.performAliasRemoval) fields

    -- Adding the structure to the environment
    modifyIORef' M.defaultCheckerState $ \s ->
        s
            { M.structures =
                Map.insert ann.name (HLIR.Forall ann.typeValue (buildFields fieldTypes)) s.structures
            }

    pure (HLIR.MkTopStructureDeclaration ann fieldTypes)

    where
        buildStructureType :: HLIR.StructureMember HLIR.Type -> Map Text HLIR.Type
        buildStructureType (HLIR.MkStructField name ty) = Map.singleton name ty
        buildStructureType (HLIR.MkStructStruct name fields') = Map.singleton name (HLIR.MkTyAnonymousStructure False (HLIR.MkTyId name) (buildFields fields'))
        buildStructureType (HLIR.MkStructUnion name fields') = Map.singleton name (HLIR.MkTyAnonymousStructure False (HLIR.MkTyId name) (buildFields fields'))

        buildFields :: [HLIR.StructureMember HLIR.Type] -> Map Text HLIR.Type
        buildFields = foldr (Map.union . buildStructureType) Map.empty

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
checkToplevelSingular (HLIR.MkTopProperty header params returnType) = withTypeVars header.typeValue $ do
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
checkToplevelSingular (HLIR.MkTopImplementation forType header params returnType body) = withTypeVars header.typeValue $ do
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

    void $ expectedImplType `M.isSubtypeOf` expectedPropType

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

    oldEnv <- readIORef M.defaultCheckerState
    modifyIORef' M.defaultCheckerState $ \s ->
        s
            { M.environment = env <> s.environment
            , M.returnType = Just aliasedReturnType
            }

    -- Checking the function body against the return type
    -- We use `withEnvironment` to temporarily extend the environment
    -- with the parameters and for type while checking the body.
    (newBody, cs, _) <- checkE aliasedReturnType body

    modifyIORef' M.defaultCheckerState $ \s ->
        s
            { M.environment = oldEnv.environment
            , M.returnType = oldEnv.returnType
            }

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
checkToplevelSingular (HLIR.MkTopAnnotation args node) = do
    args' <- map (\(_, e, _, _) -> e) <$> forM args synthesizeE

    typedNode <- checkToplevelSingular node

    pure (HLIR.MkTopAnnotation args' typedNode)
checkToplevelSingular (HLIR.MkTopExternLet ann) = do
    -- Removing aliases from the expected type
    expectedType <- M.performAliasRemoval ann.typeValue

    -- Adding the extern let to the environment
    modifyIORef' M.defaultCheckerState $ \s ->
        s
            { M.environment = Map.insert ann.name (HLIR.Forall [] expectedType) s.environment
            }

    pure (HLIR.MkTopExternLet ann)
checkToplevelSingular (HLIR.MkTopEnumeration ann constructors) = withTypeVars ann.typeValue $ do
    -- Removing aliases from the constructor types
    constructorTypes <-
        traverse
            ( \case
                Just ts -> Just <$> mapM M.performAliasRemoval ts
                Nothing -> pure Nothing
            )
            constructors

    -- Formatting the enumeration types
    -- Each constructor type is a function type that takes the
    -- constructor arguments and returns the enumeration type.
    -- For example, for `enum Option<T> { Some(T), None }`,
    -- the constructor `Some` has type `T -> Option<T>`,
    -- and `None` has type `Option<T>`.

    let enumType
            | null ann.typeValue = HLIR.MkTyId ann.name
            | otherwise =
                HLIR.MkTyApp (HLIR.MkTyId ann.name) (map HLIR.MkTyQuantified ann.typeValue)
        formatConstructor (cName, Nothing) = (cName, enumType)
        formatConstructor (cName, Just cType) = (cName, cType HLIR.:->: enumType)

    let formattedConstructors = map formatConstructor (Map.toList constructorTypes)

    -- Adding the enumeration to the environment
    -- Each constructor is added as a separate entry in the environment
    -- with its corresponding type.

    modifyIORef' M.defaultCheckerState $ \s ->
        s
            { M.environment =
                foldr
                    ( \(name, ty) env ->
                        Map.insert name (HLIR.Forall ann.typeValue ty) env
                    )
                    s.environment
                    formattedConstructors
            }

    pure (HLIR.MkTopEnumeration ann constructorTypes)

synthesizeE ::
    (MonadIO m, M.MonadError M.Error m) =>
    HLIR.HLIR "expression" ->
    m (HLIR.Type, HLIR.TLIR "expression", M.Constraints, Map Text (HLIR.Scheme HLIR.Type))
synthesizeE (HLIR.MkExprLocated p e) = do
    HLIR.pushPosition p

    (ty, expr, cs, b) <- synthesizeE e

    void HLIR.popPosition

    pure (ty, HLIR.MkExprLocated p expr, cs, b)
synthesizeE (HLIR.MkExprLiteral lit) = case lit of
    -- Literal values have known types, so we can directly return them
    -- along with their types and no constraints

    -- Int have type i32 by default
    HLIR.MkLitInt n -> pure (HLIR.MkTyInt, HLIR.MkExprLiteral (HLIR.MkLitInt n), mempty, mempty)
    -- Float have type f32 by default
    HLIR.MkLitFloat f -> pure (HLIR.MkTyFloat, HLIR.MkExprLiteral (HLIR.MkLitFloat f), mempty, mempty)
    -- Bool have type bool
    HLIR.MkLitBool b -> pure (HLIR.MkTyBool, HLIR.MkExprLiteral (HLIR.MkLitBool b), mempty, mempty)
    -- String have type *char
    HLIR.MkLitString s -> pure (HLIR.MkTyString, HLIR.MkExprLiteral (HLIR.MkLitString s), mempty, mempty)
    -- Char have type char
    HLIR.MkLitChar c -> pure (HLIR.MkTyChar, HLIR.MkExprLiteral (HLIR.MkLitChar c), mempty, mempty)
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

            pure (ty, HLIR.MkExprVariable ann{HLIR.typeValue = Identity ty} types, mempty, mempty)
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
                    , [M.MkImplConstraint ann.name ty pos types]
                    , mempty
                    )
            Nothing -> M.throw (M.VariableNotFound ann.name)
synthesizeE (HLIR.MkExprCondition cond thenB elseB _ _) = do
    -- Condition must be of type bool
    (condExpr, cs1, b1) <- checkE HLIR.MkTyBool cond

    -- Then and else branches must have the same type
    -- We first synthesize the type of the then branch,
    -- then check the else branch against that type
    (thenTy, thenExpr, cs2, _) <- M.withEnvironment b1 $ synthesizeE thenB
    (elseExpr, cs3, _) <- checkE thenTy elseB

    -- Collecting all constraints
    let cs = cs1 <> cs2 <> cs3

    pure
        (thenTy, HLIR.MkExprCondition condExpr thenExpr elseExpr (Identity thenTy) (Identity thenTy), cs, mempty)
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
    (valueExpr, cs1, _) <- checkE expectedType value

    -- Ensuring that the type of the binding is a subtype of the expected type
    forM_ binding.typeValue $ \t ->
        void $ t `M.isSubtypeOf` expectedType

    -- Synthesizing the type of the in expression
    -- The type of the in expression is the type of the whole let-in expression
    -- so we return it as the type of the let-in expression
    (inTy, typedInExpr, cs2, _) <- synthesizeE inExpr

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
        , mempty
        )
synthesizeE (HLIR.MkExprLambda params ret body) = do
    -- Removing aliases from the parameter and return types
    paramTypes <- mapM (maybe M.newType M.performAliasRemoval . (.typeValue)) params
    retType <- maybe M.newType M.performAliasRemoval ret

    -- Building parameter annotations with resolved types
    let paramAnnotations = zipWith (\p ty -> p{HLIR.typeValue = Identity ty}) params paramTypes

    -- Collecting old environment to restore it later
    oldEnv <- readIORef M.defaultCheckerState

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
            , M.returnType = Just retType
            }

    -- Checking the body against the return type
    (bodyExpr, cs, _) <- checkE retType body

    -- Restoring the old environment
    modifyIORef' M.defaultCheckerState $ \s ->
        s
            { M.environment = oldEnv.environment
            , M.returnType = oldEnv.returnType
            }

    -- Building the function type
    let funcType = paramTypes HLIR.:->: retType

    pure
        (funcType, HLIR.MkExprLambda paramAnnotations (Identity retType) bodyExpr, cs, mempty)
synthesizeE (HLIR.MkExprApplication callee args _) = do
    -- Synthesizing the type of the callee
    (calleeTy, calleeExpr, cs1, b) <- synthesizeE callee

    case calleeTy of
        HLIR.MkTyFun paramTypes retType -> do
            -- If the callee is a function type, we check that the number of
            -- arguments matches the number of parameters.
            if length paramTypes /= length args
                then M.throw (M.InvalidArgumentQuantity (length paramTypes) (length args))
                else do
                    -- Checking each argument against the corresponding parameter type
                    -- and collecting constraints from each argument.
                    (checkedArgs, cs2, bs) <- List.foldlM 
                        ( \(accArgs, accCs, accBs) (paramTy, argExpr) -> do
                            (checkedArg, csArg, bArg) <- 
                                M.withEnvironment accBs $ 
                                    checkE paramTy argExpr
                            pure (accArgs ++ [checkedArg], accCs <> csArg, accBs <> bArg)
                        )
                        ([], [], mempty)
                        (zip paramTypes args)
    
                    -- Collecting all constraints
                    let cs = cs1 <> cs2

                    pure
                        (retType, HLIR.MkExprApplication calleeExpr checkedArgs (Identity retType), cs, bs <> b)
        _ -> do
            -- If the callee is not a function type, we create new type variables
            newParamTypes <- mapM (const M.newType) args
            newRetType <- M.newType

            -- We expect the callee to be a function type with the new parameter
            -- types and new return type.
            let expectedCalleeType = HLIR.MkTyFun newParamTypes newRetType

            -- Checking each argument against the new parameter types
            -- and collecting constraints from each argument.
            (checkedArgs, cs2, bs) <- unzip3 <$> zipWithM checkE newParamTypes args

            -- We add a constraint that the callee type must be a subtype of
            -- the expected function type.
            void $ calleeTy `M.isSubtypeOf` expectedCalleeType

            -- Collecting all constraints
            let cs = cs1 <> mconcat cs2

            pure
                ( newRetType
                , HLIR.MkExprApplication calleeExpr checkedArgs (Identity newRetType)
                , cs
                , mconcat bs <> b
                )
synthesizeE (HLIR.MkExprStructureCreation ty fields) = do
    -- Removing aliases from the annotated type
    aliasedTy <- M.performAliasRemoval ty

    -- Finding the structure definition by its header, and collecting
    -- it applied type variables.
    let annHeader = getHeader aliasedTy
    annTypes <- getTypeArgs aliasedTy

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
                        (checkedExpr, cs, b) <- checkE fieldTy expr
                        pure ((name, checkedExpr), cs, b)
                    Nothing -> M.throw (M.FieldNotFound name)

            -- Collecting all constraints
            let (unzippedFields, cs, bs) = unzip3 checkedFields

            pure
                ( aliasedTy
                , HLIR.MkExprStructureCreation aliasedTy (Map.fromList unzippedFields)
                , concat cs
                , mconcat bs
                )
synthesizeE (HLIR.MkExprStructureAccess struct field) = do
    -- Synthesizing the type of the structure expression
    (structTy, structExpr, cs, b) <- synthesizeE struct

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
                , b
                )
        Just sch -> do
            -- Building a substitution map from the structure's quantified
            -- variables to the applied types. And applying the substitution to
            -- the structure's field types.
            args <- getTypeArgs structTy
            structMap <- M.instantiateMapAndSub sch args

            -- Looking up the field in the structure's field types
            -- If found, we return its type and the typed expression.
            -- If not found, we throw an error.
            case Map.lookup field structMap of
                Just fieldTy -> pure (fieldTy, HLIR.MkExprStructureAccess structExpr field, cs, b)
                Nothing -> M.throw (M.FieldNotFound field)
synthesizeE (HLIR.MkExprDereference e _) = do
    -- Synthesizing the type of the expression to dereference
    -- It must be a pointer type. We also create a fresh type variable
    -- to represent the type being pointed to.
    newType <- M.newType
    (eTy, eExpr, cs, b) <- synthesizeE e

    -- The expected type is a pointer to the new type variable
    let expectedType = HLIR.MkTyPointer newType

    -- Adding a constraint that the expression type must be a subtype
    -- of the expected pointer type.
    void $ eTy `M.isSubtypeOf` expectedType

    pure (newType, HLIR.MkExprDereference eExpr (Identity newType), cs, b)
synthesizeE (HLIR.MkExprReference e _) = do
    -- Synthesizing the type of the expression to reference
    -- The resulting type is a pointer to that type.
    (eTy, eExpr, cs, b) <- synthesizeE e
    let refType = HLIR.MkTyPointer eTy

    pure (refType, HLIR.MkExprReference eExpr (Identity refType), cs, b)
synthesizeE (HLIR.MkExprUpdate update value _) = do
    -- Synthesizing the type of the update expression
    -- The update expression must be of a type that can be updated,
    -- so we just synthesize its type and use it as the expected type
    -- for the value expression.
    (updateTy, updateExpr, cs1, b1) <- synthesizeE update
    (valueExpr, cs2, b2) <- checkE updateTy value

    -- Collecting all constraints
    let cs = cs1 <> cs2

    pure (updateTy, HLIR.MkExprUpdate updateExpr valueExpr (Identity updateTy), cs, b1 <> b2)
synthesizeE (HLIR.MkExprSizeOf t) = do
    -- Sizeof expressions have type u64, and we just need to
    -- remove aliases from the type being measured.
    aliasedType <- M.performAliasRemoval t

    pure (HLIR.MkTyId "int", HLIR.MkExprSizeOf aliasedType, mempty, mempty)
synthesizeE (HLIR.MkExprSingleIf cond thenBranch _) = do
    -- Condition must be of type bool
    (condExpr, cs, b1) <- checkE HLIR.MkTyBool cond

    (thenTy, typedThenBranch, cs2, _) <- M.withEnvironment b1 $ synthesizeE thenBranch

    -- The type of the single-if expression is the type of the then branch
    pure
        ( thenTy
        , HLIR.MkExprSingleIf condExpr typedThenBranch (Identity thenTy)
        , cs <> cs2
        , mempty
        )
synthesizeE (HLIR.MkExprCast expr targetTy) = do
    -- Synthesizing the type of the expression to cast
    (exprTy, exprExpr, cs, b) <- synthesizeE expr

    -- Removing aliases from the target type
    aliasedTargetTy <- M.performAliasRemoval targetTy

    -- Adding a constraint that the expression type must be a subtype
    -- of the target type.
    void $ exprTy `M.isSubtypeOf` aliasedTargetTy

    pure (aliasedTargetTy, HLIR.MkExprCast exprExpr aliasedTargetTy, cs, b)
synthesizeE (HLIR.MkExprWhile cond body _ inExpr) = do
    -- Condition must be of type bool
    (condExpr, cs1, b1) <- checkE HLIR.MkTyBool cond

    -- Synthesizing the type of the body
    (bodyTy, bodyExpr, cs2, _) <- M.withEnvironment b1 $ synthesizeE body

    -- Synthesizing the type of the in expression
    (inTy, inExprTyped, cs3, b3) <- synthesizeE inExpr

    -- Collecting all constraints
    let cs = cs1 <> cs2 <> cs3

    -- The type of the while expression is the type of the in expression
    pure
        (inTy, HLIR.MkExprWhile condExpr bodyExpr (Identity bodyTy) inExprTyped, cs, b3)
synthesizeE (HLIR.MkExprFunctionAccess field thisExpr typeValues args) = do
    -- Synthesizing the type of the `this` expression
    (thisTy, thisExprTyped, cs1, bindings1) <- synthesizeE thisExpr
    (argTys, typedArgs, cs2, bindings2) <- List.unzip4 <$> mapM synthesizeE args

    retType <- M.newType

    -- Looking up the function in the environment
    state' <- readIORef M.defaultCheckerState
    let env = Map.toList state'.implementations

    pos <- HLIR.peekPosition'

    funcType <-
        findImplementationMatching env typeValues field ((thisTy : argTys) HLIR.:->: retType) pos

    -- Collecting all constraints
    let cs = cs1 <> mconcat cs2

    case funcType of
        Just func@(HLIR.MkTyFun (_ : fParamTypes) fRetType) -> do
            if length fParamTypes /= length args
                then M.throw (M.InvalidArgumentQuantity (length fParamTypes) (length args))
                else do
                    void $ func `M.isSubtypeOf` ((thisTy : argTys) HLIR.:->: retType)

                    pure
                        ( fRetType
                        , HLIR.MkExprApplication
                            (HLIR.MkExprVariable (HLIR.MkAnnotation field (Identity func)) [])
                            (thisExprTyped : typedArgs)
                            (Identity fRetType)
                        , cs <> [M.MkImplConstraint field func pos typeValues]
                        , bindings1 <> mconcat bindings2
                        )
        Just ty -> M.throw (M.ExpectedFunction ty)
        Nothing -> do
            let func = (thisTy : argTys) HLIR.:->: retType

            pure
                ( retType
                , HLIR.MkExprApplication
                    (HLIR.MkExprVariable (HLIR.MkAnnotation field (Identity func)) [])
                    (thisExprTyped : typedArgs)
                    (Identity retType)
                , cs <> [M.MkImplConstraint field func pos typeValues]
                , bindings1 <> mconcat bindings2
                )
synthesizeE (HLIR.MkExprReturn e) = do
    -- Synthesizing the type of the return expression
    (eTy, eExpr, cs, bindings) <- synthesizeE e

    -- Finding the expected return type from the environment
    state' <- readIORef M.defaultCheckerState
    let expectedRetTy = state'.returnType

    case expectedRetTy of
        Just retTy -> do
            -- Adding a constraint that the return expression type must be
            -- a subtype of the expected return type.
            void $ eTy `M.isSubtypeOf` retTy

            pure (retTy, HLIR.MkExprReturn eExpr, cs, bindings)
        Nothing -> M.throw M.ReturnOutsideFunction
synthesizeE HLIR.MkExprBreak = do
    freshType <- M.newType

    -- Break expressions have type unit
    -- We just return unit type and no constraints
    pure (freshType, HLIR.MkExprBreak, mempty, mempty)
synthesizeE HLIR.MkExprContinue = do
    freshType <- M.newType

    -- Continue expressions have type unit
    -- We just return unit type and no constraints
    pure (freshType, HLIR.MkExprContinue, mempty, mempty)
synthesizeE (HLIR.MkExprIs e p _) = do
    -- Synthesizing the type of the expression to match
    (eTy, eExpr, cs, bindings1) <- synthesizeE e

    -- Checking the pattern against the expression type
    (typedPat, csPat, bindings2) <- checkP eTy p

    -- The type of the is expression is bool
    pure
        ( HLIR.MkTyBool
        , HLIR.MkExprIs eExpr typedPat (Identity eTy)
        , cs <> csPat
        , bindings1 <> bindings2
        )
synthesizeE (HLIR.MkExprLetPatternIn {}) = M.throw (M.CompilerError "Let-pattern expressions are not supported in the typechecker.")

-- | CHECK PATTERN
-- | Check a pattern against an expected type.
-- | This function returns the typed pattern and any constraints generated
-- | during the checking.
-- | It also extends the environment with any new bindings introduced by
-- | the pattern.
checkP ::
    (MonadIO m, M.MonadError M.Error m) =>
    HLIR.Type ->
    HLIR.HLIR "pattern" ->
    m (HLIR.TLIR "pattern", M.Constraints, Map Text (HLIR.Scheme HLIR.Type))
checkP expected (HLIR.MkPatternLocated p pat) = do
    HLIR.pushPosition p

    (typedPat, cs, bindings) <- checkP expected pat

    void HLIR.popPosition

    pure (HLIR.MkPatternLocated p typedPat, cs, bindings)
checkP _ HLIR.MkPatternWildcard = do
    -- Wildcard patterns match any type, so we just return the expected type
    pure (HLIR.MkPatternWildcard, mempty, mempty)
checkP expected (HLIR.MkPatternLiteral lit) = do
    -- Literal patterns have known types, so we just check if the
    -- literal type is a subtype of the expected type
    let litType = case lit of
            HLIR.MkLitInt _ -> HLIR.MkTyInt
            HLIR.MkLitFloat _ -> HLIR.MkTyFloat
            HLIR.MkLitBool _ -> HLIR.MkTyBool
            HLIR.MkLitString _ -> HLIR.MkTyString
            HLIR.MkLitChar _ -> HLIR.MkTyChar

    void $ litType `M.isSubtypeOf` expected

    pure (HLIR.MkPatternLiteral lit, mempty, mempty)
checkP expected (HLIR.MkPatternVariable (HLIR.MkAnnotation name _)) = do
    -- We check if the variable type is a subtype of the expected type

    state' <- readIORef M.defaultCheckerState
    let env = state'.environment

    case Map.lookup name env of
        Just scheme -> do
            varType <- M.instantiate scheme >>= M.performAliasRemoval

            void $ varType `M.isSubtypeOf` expected

            pure
                ( HLIR.MkPatternVariable (HLIR.MkAnnotation name (Identity varType))
                , mempty
                , mempty
                )
        Nothing -> M.throw (M.VariableNotFound name)
checkP expected (HLIR.MkPatternStructure ty fields) = do
    -- Removing aliases from the annotated type
    aliasedTy <- M.performAliasRemoval ty

    void $ aliasedTy `M.isSubtypeOf` expected

    -- Finding the structure definition by its header, and collecting
    -- it applied type variables.
    let annHeader = getHeader aliasedTy
    annTypes <- getTypeArgs aliasedTy

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

            -- Checking each field pattern against the corresponding field type
            -- and collecting constraints and bindings from each field pattern.
            checkedFields <- forM (Map.toList fields) $ \(name, pat) -> do
                case Map.lookup name structTy of
                    Just fieldTy -> do
                        (checkedPat, cs, bindings) <- checkP fieldTy pat
                        pure ((name, checkedPat), cs, bindings)
                    Nothing -> M.throw (M.FieldNotFound name)

            -- Collecting all constraints and bindings
            let (unzippedFields, csList, bindingsList) = unzip3 checkedFields
                cs = mconcat csList
                bindings = mconcat bindingsList

            pure
                ( HLIR.MkPatternStructure aliasedTy (Map.fromList unzippedFields)
                , cs
                , bindings
                )
checkP expected (HLIR.MkPatternConstructor name constructors _) = do
    -- Pattern constructors acts like function calls in patterns.
    -- We find the constructor in the environment, instantiate its type,
    -- and check if the resulting type is a subtype of the expected type.

    state' <- readIORef M.defaultCheckerState
    let env = state'.environment

    case Map.lookup name env of
        Just scheme -> do
            ctorType <- M.instantiate scheme >>= M.performAliasRemoval

            case ctorType of
                HLIR.MkTyFun paramTypes retType -> do
                    (pats, cs, bindings) <- unzip3 <$> zipWithM checkP paramTypes constructors

                    void $ retType `M.isSubtypeOf` expected

                    pure
                        ( HLIR.MkPatternConstructor name pats (Identity ctorType)
                        , concat cs
                        , Map.unions bindings
                        )
                _ -> M.throw M.InvalidHeader
        Nothing -> M.throw (M.VariableNotFound name)
checkP expected (HLIR.MkPatternLet (HLIR.MkAnnotation name _)) = do
    -- We treat let patterns as variable declaring patterns.
    pure
        ( HLIR.MkPatternLet (HLIR.MkAnnotation name (Identity expected))
        , mempty
        , Map.singleton name (HLIR.Forall [] expected)
        )

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
    m (HLIR.TLIR "expression", M.Constraints, Map Text (HLIR.Scheme HLIR.Type))
checkE expected expr = do
    (inferredTy, typedExpr, cs, bindings) <- synthesizeE expr

    void $ inferredTy `M.isSubtypeOf` expected

    pure (typedExpr, cs, bindings)

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
        M.MkImplConstraint name ty pos tys -> do
            mImplTy <- findImplementationMatching implementations tys name ty pos
            case mImplTy of
                Just implTy -> void $ implTy `M.isSubtypeOf` ty
                Nothing -> pure () -- Ignoring unsolved constraints
        M.MkFieldConstraint structTy fieldName fieldTy pos -> do
            header <- getHeader <$> M.removeAliases structTy
            args <- getTypeArgs structTy

            case header of
                Just resolved ->
                    findStructureMatching args structures resolved pos >>= \case
                        Just structMap -> case Map.lookup fieldName structMap of
                            Just expectedFieldTy -> void $ expectedFieldTy `M.isSubtypeOf` fieldTy
                            Nothing -> M.throwError (M.FieldNotFound fieldName, pos)
                        Nothing -> M.throwError (M.StructureNotFound structTy, pos)
                Nothing -> M.throwError (M.InvalidHeader, pos)
  where
    findStructureMatching ::
        (MonadIO m, M.MonadError M.Error m) =>
        [HLIR.Type] ->
        [(Text, HLIR.Scheme (Map Text HLIR.Type))] ->
        Text ->
        HLIR.Position ->
        m (Maybe (Map Text HLIR.Type))
    findStructureMatching _ [] _ _ = pure Nothing
    findStructureMatching args ((structName, scheme) : xs) name pos
        | structName == name = do
            -- We instantiate the structure scheme to get a concrete type
            structTy <- M.instantiateMapAndSub scheme args >>= mapM M.performAliasRemoval
            pure (Just structTy)
        | otherwise = findStructureMatching args xs name pos

findImplementationMatching ::
    (MonadIO m, M.MonadError M.Error m) =>
    [((Text, HLIR.Type), HLIR.Scheme HLIR.Type)] ->
    [HLIR.Type] ->
    Text ->
    HLIR.Type ->
    HLIR.Position ->
    m (Maybe HLIR.Type)
findImplementationMatching [] _ _ _ _ = pure Nothing
findImplementationMatching (((implName, _), scheme) : xs) tys name ty pos
    | implName == name = do
        -- We instantiate the implementation scheme to get a concrete type
        -- and remove aliases from it.
        implTy <- M.instantiateWithSub scheme tys >>= M.performAliasRemoval

        -- We check if the implementation type is a subtype of the required type
        -- If it is, we return it as a matching implementation.
        result <- runExceptT $ M.applySubtypeRelation False ty implTy

        case result of
            Right _ -> pure (Just implTy)
            Left _ -> findImplementationMatching xs tys name ty pos
    | otherwise = findImplementationMatching xs tys name ty pos

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
getTypeArgs :: MonadIO m => HLIR.Type -> m [HLIR.Type]
getTypeArgs (HLIR.MkTyApp _ args) = pure args
getTypeArgs (HLIR.MkTyVar ref) = do
    ty <- readIORef ref

    case ty of
        HLIR.Link t -> getTypeArgs t
        HLIR.Unbound _ _ -> pure []
getTypeArgs _ = pure []

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
        Nothing -> M.newType >>= \ty -> pure (HLIR.Forall [] ty)

-- | Remove the third element from a triple.
-- | This is used to convert constraints from (name, type, position)
-- | to (name, type).
removeThird :: (a, b, c) -> (a, b)
removeThird (x, y, _) = (x, y)

-- | Check if the following type is a function type.
-- | If it is, return True, otherwise return False.
isFunctionType :: (MonadIO m) => HLIR.Type -> m Bool
isFunctionType (HLIR.MkTyFun _ _) = pure True
isFunctionType (HLIR.MkTyVar ref) = do
    ty <- readIORef ref

    case ty of
        HLIR.Link t -> isFunctionType t
        HLIR.Unbound _ _ -> pure False
isFunctionType _ = pure False
