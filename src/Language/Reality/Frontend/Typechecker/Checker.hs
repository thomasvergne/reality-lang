module Language.Reality.Frontend.Typechecker.Checker where

import Language.Reality.Frontend.Typechecker.Monad qualified as M
import Control.Monad.Result qualified as M
import Language.Reality.Syntax.HLIR qualified as HLIR
import Control.Monad.Except qualified as M
import Language.Reality.Frontend.Typechecker.Unification qualified as M
import Data.Map qualified as Map

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

checkToplevelSingular ::
    (MonadIO m, M.MonadError M.Error m) =>
    HLIR.HLIR "toplevel" ->
    m (HLIR.TLIR "toplevel")
checkToplevelSingular (HLIR.MkTopConstantDeclaration ann expr) = do
    expectedType <- M.performAliasRemoval ann.typeValue

    typedExpr <- checkE expectedType expr

    modifyIORef' M.defaultCheckerState $ \s ->
        s { M.environment = Map.insert ann.name (HLIR.Forall [] expectedType) s.environment }

    pure (HLIR.MkTopConstantDeclaration ann typedExpr)
checkToplevelSingular (HLIR.MkTopFunctionDeclaration ann params ret body) = do
    paramTypes <- mapM (M.performAliasRemoval . (.typeValue)) params
    retType <- M.performAliasRemoval ret

    let newParams = zipWith (\p ty -> p { HLIR.typeValue = ty }) params paramTypes

    let funcType = paramTypes HLIR.:->: retType

    modifyIORef' M.defaultCheckerState $ \s ->
        s { M.environment = Map.insert ann.name (HLIR.Forall ann.typeValue funcType) s.environment }

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

    typedBody <- checkE retType body

    modifyIORef' M.defaultCheckerState $ \s ->
        s
            { M.environment = oldEnv
            }

    pure (HLIR.MkTopFunctionDeclaration ann newParams retType typedBody)
checkToplevelSingular (HLIR.MkTopTypeAlias ann aliased) = do
    realiasedType <- M.performAliasRemoval aliased
    modifyIORef' M.defaultCheckerState $ \s ->
        s { M.typeAliases = Map.insert ann.name (HLIR.Forall ann.typeValue realiasedType) s.typeAliases }
    pure (HLIR.MkTopTypeAlias ann aliased)
checkToplevelSingular (HLIR.MkTopLocated p n) = do
    HLIR.pushPosition p
    typedNode <- checkToplevelSingular n
    void HLIR.popPosition
    pure (HLIR.MkTopLocated p typedNode)
checkToplevelSingular (HLIR.MkTopPublic node) = do
    typedNode <- checkToplevelSingular node
    pure (HLIR.MkTopPublic typedNode)
checkToplevelSingular (HLIR.MkTopModuleDeclaration {}) = M.throw (M.CompilerError "Modules are not supported in the typechecker.")
checkToplevelSingular (HLIR.MkTopStructureDeclaration ann fields) = do
    fieldTypes <- traverse M.performAliasRemoval fields

    modifyIORef' M.defaultCheckerState $ \s ->
        s { M.structures = Map.insert ann.name (HLIR.Forall ann.typeValue fieldTypes) s.structures }

    pure (HLIR.MkTopStructureDeclaration ann fields)
checkToplevelSingular (HLIR.MkTopExternalFunction ann params ret) = do
    paramTypes <- mapM (M.performAliasRemoval . (.typeValue)) params
    retType <- M.performAliasRemoval ret

    let funcType = paramTypes HLIR.:->: retType
        newParams = zipWith (\p ty -> p { HLIR.typeValue = ty }) params paramTypes

    modifyIORef' M.defaultCheckerState $ \s ->
        s { M.environment = Map.insert ann.name (HLIR.Forall ann.typeValue funcType) s.environment }

    pure (HLIR.MkTopExternalFunction ann newParams retType)
checkToplevelSingular (HLIR.MkTopImport _) = M.throw (M.CompilerError "Imports are not supported in the typechecker.")

synthesizeE ::
    (MonadIO m, M.MonadError M.Error m) =>
    HLIR.HLIR "expression" ->
    m (HLIR.Type, HLIR.TLIR "expression")
synthesizeE (HLIR.MkExprLocated p e) = do
    HLIR.pushPosition p

    (ty, expr) <- synthesizeE e

    void HLIR.popPosition

    pure (ty, HLIR.MkExprLocated p expr)
synthesizeE (HLIR.MkExprLiteral lit) = case lit of
    HLIR.MkLitInt n -> pure (HLIR.MkTyInt, HLIR.MkExprLiteral (HLIR.MkLitInt n))
    HLIR.MkLitFloat f -> pure (HLIR.MkTyFloat, HLIR.MkExprLiteral (HLIR.MkLitFloat f))
    HLIR.MkLitBool b -> pure (HLIR.MkTyBool, HLIR.MkExprLiteral (HLIR.MkLitBool b))
    HLIR.MkLitString s -> pure (HLIR.MkTyString, HLIR.MkExprLiteral (HLIR.MkLitString s))
    HLIR.MkLitChar c -> pure (HLIR.MkTyChar, HLIR.MkExprLiteral (HLIR.MkLitChar c))
synthesizeE (HLIR.MkExprVariable ann types) = do
    variables <- readIORef M.defaultCheckerState <&> M.environment

    case Map.lookup ann.name variables of
        Just scheme -> do
            ty <- M.instantiateWithSub scheme types >>= M.performAliasRemoval
            pure (ty, HLIR.MkExprVariable ann { HLIR.typeValue = Identity ty } types)
        Nothing -> M.throw (M.VariableNotFound ann.name)
synthesizeE (HLIR.MkExprCondition cond thenB elseB) = do
    condExpr <- checkE HLIR.MkTyBool cond

    (thenTy, thenExpr) <- synthesizeE thenB
    elseExpr <- checkE thenTy elseB

    pure (thenTy, HLIR.MkExprCondition condExpr thenExpr elseExpr)
synthesizeE (HLIR.MkExprLetIn binding value inExpr) = do
    expectedType <- maybe M.newType M.performAliasRemoval binding.typeValue

    oldEnv <- readIORef M.defaultCheckerState <&> M.environment

    modifyIORef' M.defaultCheckerState $ \s ->
        s { M.environment = Map.insert binding.name (HLIR.Forall [] expectedType) s.environment }

    valueExpr <- checkE expectedType value

    (inTy, typedInExpr) <- synthesizeE inExpr

    modifyIORef' M.defaultCheckerState $ \s ->
        s { M.environment = oldEnv }

    pure (inTy, HLIR.MkExprLetIn binding { HLIR.typeValue = Identity expectedType } valueExpr typedInExpr)
synthesizeE (HLIR.MkExprLambda params ret body) = do
    paramTypes <- mapM (maybe M.newType M.performAliasRemoval . (.typeValue)) params

    let paramAnnotations = zipWith (\p ty -> p { HLIR.typeValue = Identity ty }) params paramTypes

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

    bodyExpr <- checkE retType body

    modifyIORef' M.defaultCheckerState $ \s ->
        s
            { M.environment = oldEnv
            }

    let funcType = paramTypes HLIR.:->: retType

    pure (funcType, HLIR.MkExprLambda paramAnnotations (Identity retType) bodyExpr)
synthesizeE (HLIR.MkExprApplication callee args) = do
    (calleeTy, calleeExpr) <- synthesizeE callee

    case calleeTy of
        HLIR.MkTyFun paramTypes retType -> do
            if length paramTypes /= length args
                then M.throw (M.InvalidArgumentQuantity (length paramTypes) (length args))
                else do
                    checkedArgs <- zipWithM checkE paramTypes args
                    pure (retType, HLIR.MkExprApplication calleeExpr checkedArgs)
        _ -> M.throw (M.ExpectedFunction calleeTy)
synthesizeE (HLIR.MkExprStructureCreation ty fields) = do
    let annHeader = getHeader ty
    scheme <- findStructureMaybeById annHeader

    structTy <- M.instantiateMap scheme

    checkedFields <- forM (Map.toList fields) $ \(name, expr) -> do
        case Map.lookup name structTy of
            Just fieldTy -> do
                checkedExpr <- checkE fieldTy expr
                pure (name, checkedExpr)
            Nothing -> M.throw (M.FieldNotFound name)

    let (fieldNames, fieldExprs) = unzip checkedFields

    pure ( ty, HLIR.MkExprStructureCreation ty (Map.fromList (zip fieldNames fieldExprs)) )
synthesizeE (HLIR.MkExprStructureAccess struct field) = do
    (structTy, structExpr) <- synthesizeE struct

    let annHeader = getHeader structTy
    scheme <- findStructureMaybeById annHeader

    (structMap, _) <- M.instantiateMapAndSub scheme

    case Map.lookup field structMap of
        Just fieldTy -> pure (fieldTy, HLIR.MkExprStructureAccess structExpr field)
        Nothing -> M.throw (M.FieldNotFound field)

checkE ::
    (MonadIO m, M.MonadError M.Error m) =>
    HLIR.Type ->
    HLIR.HLIR "expression" ->
    m (HLIR.TLIR "expression")
checkE expected expr = do
    (inferredTy, typedExpr) <- synthesizeE expr

    void $ inferredTy `M.isSubtypeOf` expected

    pure typedExpr

getHeader :: HLIR.Type -> Maybe Text
getHeader (HLIR.MkTyApp (HLIR.MkTyId name) _) = Just name
getHeader (HLIR.MkTyId name) = Just name
getHeader _ = Nothing

findStructureMaybeById :: (MonadIO m, M.MonadError M.Error m) => Maybe Text -> m (HLIR.Scheme (Map Text HLIR.Type))
findStructureMaybeById name = do
    structTypes <- readIORef M.defaultCheckerState <&> M.structures

    case name of
        Just n -> case Map.lookup n structTypes of
            Just scheme -> pure scheme
            Nothing -> M.throw (M.StructureNotFound (HLIR.MkTyId n))
        Nothing -> M.throw M.InvalidHeader
