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

removeThird :: (a, b, c) -> (a, b)
removeThird (x, y, _) = (x, y)

checkToplevelSingular ::
    (MonadIO m, M.MonadError M.Error m) =>
    HLIR.HLIR "toplevel" ->
    m (HLIR.TLIR "toplevel")
checkToplevelSingular (HLIR.MkTopConstantDeclaration ann expr) = do
    expectedType <- M.performAliasRemoval ann.typeValue

    (typedExpr, cs) <- checkE expectedType expr

    unless (null cs)
        $ M.throw (M.UnsolvedConstraints (map removeThird cs))

    modifyIORef' M.defaultCheckerState $ \s ->
        s
            { M.environment = Map.insert ann.name (HLIR.Forall [] expectedType) s.environment
            }

    pure (HLIR.MkTopConstantDeclaration ann typedExpr)
checkToplevelSingular (HLIR.MkTopFunctionDeclaration ann params ret body) = do
    paramTypes <- mapM (M.performAliasRemoval . (.typeValue)) params
    retType <- M.performAliasRemoval ret

    let newParams = zipWith (\p ty -> p{HLIR.typeValue = ty}) params paramTypes

    let funcType = paramTypes HLIR.:->: retType

    modifyIORef' M.defaultCheckerState $ \s ->
        s
            { M.environment =
                Map.insert ann.name (HLIR.Forall ann.typeValue funcType) s.environment
            }

    oldEnv <- readIORef M.defaultCheckerState <&> M.environment

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

    (typedBody, cs) <- checkE retType body

    solveConstraints cs

    modifyIORef' M.defaultCheckerState $ \s ->
        s
            { M.environment = oldEnv
            }

    pure (HLIR.MkTopFunctionDeclaration ann newParams retType typedBody)
checkToplevelSingular (HLIR.MkTopTypeAlias ann aliased) = do
    realiasedType <- M.performAliasRemoval aliased
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
    fieldTypes <- traverse M.performAliasRemoval fields

    modifyIORef' M.defaultCheckerState $ \s ->
        s
            { M.structures =
                Map.insert ann.name (HLIR.Forall ann.typeValue fieldTypes) s.structures
            }

    pure (HLIR.MkTopStructureDeclaration ann fields)
checkToplevelSingular (HLIR.MkTopExternalFunction ann params ret) = do
    paramTypes <- mapM (M.performAliasRemoval . (.typeValue)) params
    retType <- M.performAliasRemoval ret

    let funcType = paramTypes HLIR.:->: retType
        newParams = zipWith (\p ty -> p{HLIR.typeValue = ty}) params paramTypes

    modifyIORef' M.defaultCheckerState $ \s ->
        s
            { M.environment =
                Map.insert ann.name (HLIR.Forall ann.typeValue funcType) s.environment
            }

    pure (HLIR.MkTopExternalFunction ann newParams retType)
checkToplevelSingular (HLIR.MkTopImport _) = M.throw (M.CompilerError "Imports are not supported in the typechecker.")
checkToplevelSingular (HLIR.MkTopProperty header params returnType) = do
    paramTypes <- mapM (M.performAliasRemoval . (.typeValue)) params
    retType <- M.performAliasRemoval returnType

    let funcType = paramTypes HLIR.:->: retType
        newParams = zipWith (\p ty -> p{HLIR.typeValue = ty}) params paramTypes

    modifyIORef' M.defaultCheckerState $ \s ->
        s
            { M.properties =
                Map.insert header.name (HLIR.Forall header.typeValue funcType) s.properties
            }

    pure (HLIR.MkTopProperty header newParams retType)
checkToplevelSingular (HLIR.MkTopImplementation forType header params returnType body) = do
    scheme <- findPropertyByName header.name

    aliasedReturnType <- M.performAliasRemoval returnType
    aliasedParamTypes <- mapM (M.performAliasRemoval . (.typeValue)) params
    aliasedForType <- M.performAliasRemoval forType.typeValue

    let funcType = (aliasedForType : aliasedParamTypes) HLIR.:->: aliasedReturnType
    let implScheme = HLIR.Forall header.typeValue funcType

    -- Checking if the implementation matches the property

    expectedPropType <- M.instantiate scheme >>= M.performAliasRemoval
    expectedImplType <- M.instantiate implScheme >>= M.performAliasRemoval

    void $ expectedPropType `M.isSubtypeOf` expectedImplType

    -- End of checking

    modifyIORef' M.defaultCheckerState $ \s ->
        s
            { M.implementations =
                Map.insert (header.name, aliasedForType) implScheme s.implementations
            }

    paramTypes <- mapM (M.performAliasRemoval . (.typeValue)) params
    let newParams = zipWith (\p ty -> p{HLIR.typeValue = ty}) params paramTypes

    let env =
            Map.fromList (map ((HLIR.Forall [] <$>) . HLIR.unannotate) newParams)
                <> Map.singleton forType.name (HLIR.Forall [] aliasedForType)

    (newBody, cs) <- M.withEnvironment env $ checkE aliasedReturnType body

    solveConstraints cs

    pure
        ( HLIR.MkTopImplementation
            forType{HLIR.typeValue = aliasedForType}
            header
            newParams
            aliasedReturnType
            newBody
        )

findPropertyByName ::
    (MonadIO m, M.MonadError M.Error m) => Text -> m (HLIR.Scheme HLIR.Type)
findPropertyByName name = do
    properties <- readIORef M.defaultCheckerState <&> M.properties

    case Map.lookup name properties of
        Just scheme -> pure scheme
        Nothing -> M.throw (M.PropertyNotFound name)

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
    HLIR.MkLitInt n -> pure (HLIR.MkTyInt, HLIR.MkExprLiteral (HLIR.MkLitInt n), mempty)
    HLIR.MkLitFloat f -> pure (HLIR.MkTyFloat, HLIR.MkExprLiteral (HLIR.MkLitFloat f), mempty)
    HLIR.MkLitBool b -> pure (HLIR.MkTyBool, HLIR.MkExprLiteral (HLIR.MkLitBool b), mempty)
    HLIR.MkLitString s -> pure (HLIR.MkTyString, HLIR.MkExprLiteral (HLIR.MkLitString s), mempty)
    HLIR.MkLitChar c -> pure (HLIR.MkTyChar, HLIR.MkExprLiteral (HLIR.MkLitChar c), mempty)
synthesizeE (HLIR.MkExprVariable ann types) = do
    env <- readIORef M.defaultCheckerState
    let variables = env.environment
        properties = env.properties

    case Map.lookup ann.name variables of
        Just scheme -> do
            ty <- M.instantiateWithSub scheme types >>= M.performAliasRemoval
            pure (ty, HLIR.MkExprVariable ann{HLIR.typeValue = Identity ty} types, mempty)
        Nothing -> case Map.lookup ann.name properties of
            Just scheme -> do
                ty <- M.instantiateWithSub scheme types >>= M.performAliasRemoval
                pos <- HLIR.peekPosition'
                pure
                    ( ty
                    , HLIR.MkExprVariable ann{HLIR.typeValue = Identity ty} types
                    , [(ann.name, ty, pos)]
                    )
            Nothing -> M.throw (M.VariableNotFound ann.name)
synthesizeE (HLIR.MkExprCondition cond thenB elseB) = do
    (condExpr, cs1) <- checkE HLIR.MkTyBool cond

    (thenTy, thenExpr, cs2) <- synthesizeE thenB
    (elseExpr, cs3) <- checkE thenTy elseB

    let cs = cs1 <> cs2 <> cs3

    pure (thenTy, HLIR.MkExprCondition condExpr thenExpr elseExpr, cs)
synthesizeE (HLIR.MkExprLetIn binding value inExpr) = do
    oldEnv <- readIORef M.defaultCheckerState <&> M.environment

    expectedType <- maybe M.newType M.performAliasRemoval binding.typeValue

    modifyIORef' M.defaultCheckerState $ \s ->
        s
            { M.environment =
                Map.insert binding.name (HLIR.Forall [] expectedType) s.environment
            }

    (valueExpr, cs1) <- checkE expectedType value

    forM_ binding.typeValue $ \t ->
        void $ t `M.isSubtypeOf` expectedType

    (inTy, typedInExpr, cs2) <- synthesizeE inExpr

    modifyIORef' M.defaultCheckerState $ \s ->
        s{M.environment = oldEnv}

    let cs = cs1 <> cs2

    pure
        ( inTy
        , HLIR.MkExprLetIn
            binding{HLIR.typeValue = Identity expectedType}
            valueExpr
            typedInExpr
        , cs
        )
synthesizeE (HLIR.MkExprLambda params ret body) = do
    paramTypes <- mapM (maybe M.newType M.performAliasRemoval . (.typeValue)) params

    let paramAnnotations = zipWith (\p ty -> p{HLIR.typeValue = Identity ty}) params paramTypes

    oldEnv <- readIORef M.defaultCheckerState <&> M.environment

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

    retType <- maybe M.newType M.performAliasRemoval ret

    (bodyExpr, cs) <- checkE retType body

    modifyIORef' M.defaultCheckerState $ \s ->
        s
            { M.environment = oldEnv
            }

    let funcType = paramTypes HLIR.:->: retType

    pure
        (funcType, HLIR.MkExprLambda paramAnnotations (Identity retType) bodyExpr, cs)
synthesizeE (HLIR.MkExprApplication callee args) = do
    (calleeTy, calleeExpr, cs1) <- synthesizeE callee

    case calleeTy of
        HLIR.MkTyFun paramTypes retType -> do
            if length paramTypes /= length args
                then M.throw (M.InvalidArgumentQuantity (length paramTypes) (length args))
                else do
                    (checkedArgs, cs2) <- unzip <$> zipWithM checkE paramTypes args

                    let cs = cs1 <> mconcat cs2

                    pure (retType, HLIR.MkExprApplication calleeExpr checkedArgs, cs)
        _ -> M.throw (M.ExpectedFunction calleeTy)
synthesizeE (HLIR.MkExprStructureCreation ty fields) = do
    let annHeader = getHeader ty
        annTypes = getTypeArgs ty
    HLIR.Forall qvars structType <- findStructureMaybeById annHeader

    let subst = zip qvars annTypes
        rest = drop (length annTypes) qvars
    newVars <- forM rest $ const M.newType
    let substMap = Map.fromList (subst ++ zip rest newVars)

    structTy <-
        Map.traverseWithKey (\_ t -> M.applySubstitution substMap t) structType

    checkedFields <- forM (Map.toList fields) $ \(name, expr) -> do
        case Map.lookup name structTy of
            Just fieldTy -> do
                (checkedExpr, cs) <- checkE fieldTy expr
                pure ((name, checkedExpr), cs)
            Nothing -> M.throw (M.FieldNotFound name)

    let (unzippedFields, cs) = unzip checkedFields

    pure
        ( ty
        , HLIR.MkExprStructureCreation ty (Map.fromList unzippedFields)
        , concat cs
        )
synthesizeE (HLIR.MkExprStructureAccess struct field) = do
    (structTy, structExpr, cs) <- synthesizeE struct

    annHeader <- getHeader <$> M.removeAliases structTy
    HLIR.Forall qvars ty <- findStructureMaybeById annHeader

    let substMap = Map.fromList (zip qvars (getTypeArgs structTy))
    structMap <- Map.traverseWithKey (\_ t -> M.applySubstitution substMap t) ty

    case Map.lookup field structMap of
        Just fieldTy -> pure (fieldTy, HLIR.MkExprStructureAccess structExpr field, cs)
        Nothing -> M.throw (M.FieldNotFound field)
synthesizeE (HLIR.MkExprDereference e) = do
    newType <- M.newType
    (eTy, eExpr, cs) <- synthesizeE e

    let expectedType = HLIR.MkTyPointer newType

    void $ eTy `M.isSubtypeOf` expectedType

    pure (newType, HLIR.MkExprDereference eExpr, cs)
synthesizeE (HLIR.MkExprReference e) = do
    (eTy, eExpr, cs) <- synthesizeE e
    let refType = HLIR.MkTyPointer eTy
    pure (refType, HLIR.MkExprReference eExpr, cs)
synthesizeE (HLIR.MkExprUpdate update value) = do
    (updateTy, updateExpr, cs1) <- synthesizeE update
    (valueExpr, cs2) <- checkE updateTy value

    let cs = cs1 <> cs2

    pure (updateTy, HLIR.MkExprUpdate updateExpr valueExpr, cs)
synthesizeE (HLIR.MkExprSizeOf t) = do
    aliasedType <- M.performAliasRemoval t
    pure (HLIR.MkTyId "u64", HLIR.MkExprSizeOf aliasedType, mempty)

getTypeArgs :: HLIR.Type -> [HLIR.Type]
getTypeArgs (HLIR.MkTyApp _ args) = args
getTypeArgs _ = []

checkE ::
    (MonadIO m, M.MonadError M.Error m) =>
    HLIR.Type ->
    HLIR.HLIR "expression" ->
    m (HLIR.TLIR "expression", M.Constraints)
checkE expected expr = do
    (inferredTy, typedExpr, cs) <- synthesizeE expr

    void $ inferredTy `M.isSubtypeOf` expected

    pure (typedExpr, cs)

getHeader :: HLIR.Type -> Maybe Text
getHeader (HLIR.MkTyApp (HLIR.MkTyId name) _) = Just name
getHeader (HLIR.MkTyId name) = Just name
getHeader _ = Nothing

findStructureMaybeById ::
    (MonadIO m, M.MonadError M.Error m) =>
    Maybe Text -> m (HLIR.Scheme (Map Text HLIR.Type))
findStructureMaybeById name = do
    structTypes <- readIORef M.defaultCheckerState <&> M.structures

    case name of
        Just n -> case Map.lookup n structTypes of
            Just scheme -> pure scheme
            Nothing -> M.throw (M.StructureNotFound (HLIR.MkTyId n))
        Nothing -> M.throw M.InvalidHeader

solveConstraints :: (MonadIO m, M.MonadError M.Error m) => M.Constraints -> m ()
solveConstraints constraints = do
    implementations <-
        Map.toList <$> (readIORef M.defaultCheckerState <&> M.implementations)
    forM_ constraints $ \(name, ty, pos) -> do
        implType <- findImplementationMatching implementations name ty pos
        case implType of
            Just implTyValue -> void $ implTyValue `M.isSubtypeOf` ty
            Nothing -> pure ()
  where
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
            implTy <- M.instantiate scheme >>= M.performAliasRemoval
            result <- runExceptT $ M.applySubtypeRelation False implTy ty

            case result of
                Right _ -> pure (Just implTy)
                Left _ -> findImplementationMatching xs name ty pos
        | otherwise = findImplementationMatching xs name ty pos
