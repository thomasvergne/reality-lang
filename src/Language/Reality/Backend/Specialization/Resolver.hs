{-# LANGUAGE LambdaCase, RecordWildCards #-}

module Language.Reality.Backend.Specialization.Resolver where

import Language.Reality.Syntax.HLIR qualified as HLIR
import Control.Monad.Result qualified as M
import Control.Monad.Except qualified as M
import Language.Reality.Frontend.Typechecker.Monad qualified as M
import Language.Reality.Frontend.Typechecker.Unification qualified as M
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import GHC.IO qualified as IO
import Data.Traversable qualified as Trav

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
    | null ann.typeValue = do
        (specParams, newDefs) <- mapAndUnzipM (\case
                HLIR.MkAnnotation name ty -> do
                    (specTy, newDefs) <- resolveSpecializationInType ty
                    pure (HLIR.MkAnnotation name specTy, newDefs)
            ) params
        (specRet, newDefs2) <- resolveSpecializationInType ret
        (typedBody, newDefs3) <- resolveSpecializationInExpr body

        let allNewDefs = concat newDefs ++ newDefs2 ++ newDefs3

        pure (Just $ HLIR.MkTopFunctionDeclaration ann specParams specRet typedBody, allNewDefs)
    | otherwise = do
        let paramTypes = map (.typeValue) params
            funcType = paramTypes HLIR.:->: ret

        let scheme = HLIR.Forall ann.typeValue funcType

        modifyIORef' defaultSpecializer $ \s ->
            s { variables = Map.insert ann.name (scheme, node ) s.variables }

        pure (Nothing, [])
resolveSpecializationSingular (HLIR.MkTopPublic n) = do
    (resolved, newDefs) <- resolveSpecializationSingular n
    pure (HLIR.MkTopPublic <$> resolved, newDefs)
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
    (specParams, newDefs) <- mapAndUnzipM (\case
            HLIR.MkAnnotation name ty -> do
                (specTy, newDefs) <- resolveSpecializationInType ty.runIdentity
                pure (HLIR.MkAnnotation name (Identity specTy), newDefs)
        ) params
    (specRet, newDefs2) <- first Identity <$> resolveSpecializationInType ret.runIdentity

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

    pure (HLIR.MkExprCondition typedCond typedThen typedElse, newDefs1 ++ newDefs2 ++ newDefs3)
resolveSpecializationInExpr (HLIR.MkExprApplication callee args) = do
    (typedCallee, newDefs1) <- resolveSpecializationInExpr callee
    (typedArgs, newDefs2) <- mapAndUnzipM resolveSpecializationInExpr args

    pure (HLIR.MkExprApplication typedCallee typedArgs, newDefs1 ++ concat newDefs2)
resolveSpecializationInExpr (HLIR.MkExprStructureAccess struct field) = do
    (typedStruct, newDefs) <- resolveSpecializationInExpr struct
    pure (HLIR.MkExprStructureAccess typedStruct field, newDefs)
resolveSpecializationInExpr (HLIR.MkExprStructureCreation ann fields) = do
    (specAnn, newDefs) <- resolveSpecializationInType ann
    (typedFields, newDefs2) <- mapAndUnzipM (\(name, expr) -> do
        (typedExpr, defs) <- resolveSpecializationInExpr expr
        pure ((name, typedExpr), defs)
        ) (Map.toList fields)

    let fieldMap = Map.fromList typedFields
        allNewDefs2 = concat newDefs2
        allNewDefs = newDefs ++ allNewDefs2

    pure (HLIR.MkExprStructureCreation specAnn fieldMap, allNewDefs)

applySubstInExpr :: MonadIO m => Map Text HLIR.Type -> HLIR.TLIR "expression" -> m (HLIR.TLIR "expression")
applySubstInExpr subst (HLIR.MkExprLocated p e) =
    HLIR.MkExprLocated p <$> applySubstInExpr subst e
applySubstInExpr subst (HLIR.MkExprVariable ann@(HLIR.MkAnnotation _ ty) types) = do
    newType <- M.applySubstitution subst ty.runIdentity
    pure (HLIR.MkExprVariable (ann { HLIR.typeValue = Identity newType }) types)
applySubstInExpr _ (HLIR.MkExprLiteral lit) = pure (HLIR.MkExprLiteral lit)
applySubstInExpr subst (HLIR.MkExprLambda params ret body) = do
    newParams <- forM params $ \(HLIR.MkAnnotation name ty) -> do
        newTy <- M.applySubstitution subst ty.runIdentity
        pure (HLIR.MkAnnotation name (Identity newTy))
    newRet <- M.applySubstitution subst ret.runIdentity
    newBody <- applySubstInExpr subst body
    pure (HLIR.MkExprLambda newParams (Identity newRet) newBody)
applySubstInExpr subst (HLIR.MkExprLetIn binding value inExpr)
    = do
        newBinding <- case binding of
            HLIR.MkAnnotation name ty -> do
                newTy <- M.applySubstitution subst ty.runIdentity
                pure (HLIR.MkAnnotation name (Identity newTy))
        newValue <- applySubstInExpr subst value
        newInExpr <- applySubstInExpr subst inExpr
        pure (HLIR.MkExprLetIn newBinding newValue newInExpr)
applySubstInExpr subst (HLIR.MkExprCondition cond thenB elseB)
    = do
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

    case Map.lookup name (variables specState) of
        Just (scheme@(HLIR.Forall qvars _), toplevel) -> do
            (schemeType, subst) <- M.instantiateAndSub scheme
            void $ ty `M.isSubtypeOf` schemeType

            case toplevel of
                HLIR.MkTopFunctionDeclaration {
                  parameters
                , returnType
                , body
                , name = _
                } -> do
                    let orderedVars = flip map qvars $ \var -> Map.findWithDefault (HLIR.MkTyQuantified var) var subst
                    let newName = name <> "_" <> Text.intercalate "_" (map toText orderedVars)

                    if Set.member newName specState.rememberedVariables
                        then do
                            let newAnn = HLIR.MkAnnotation newName (Identity ty)
                            pure (newAnn, [])
                        else do
                            specParameters <- Trav.for parameters $ \(HLIR.MkAnnotation paramName paramType) -> do
                                newParamType <- M.applySubstitution subst paramType
                                pure (HLIR.MkAnnotation paramName newParamType)

                            specReturnType <- M.applySubstitution subst returnType

                            newBody <- applySubstInExpr subst body
                            (specBody, newDefs) <- resolveSpecializationInExpr newBody

                            let newFunction = HLIR.MkTopFunctionDeclaration
                                    { HLIR.name = HLIR.MkAnnotation newName []
                                    , HLIR.parameters = specParameters
                                    , HLIR.returnType = specReturnType
                                    , HLIR.body = specBody
                                    }

                            let allNewDefs = newDefs ++ [newFunction]

                            let funcType = map (.typeValue) specParameters HLIR.:->: specReturnType

                            pure (HLIR.MkAnnotation name (Identity funcType), allNewDefs)
                _ -> M.throw (M.VariableNotFound name)
        Nothing -> pure (HLIR.MkAnnotation name (Identity ty), [])

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
        HLIR.Unbound {} -> pure (HLIR.MkTyVar ref, [])
resolveSpecializationInType (HLIR.MkTyQuantified name) = do
    pure (HLIR.MkTyQuantified name, [])
resolveSpecializationInType (HLIR.MkTyAnonymousStructure fields) = do
    (typedFields, newDefs) <- mapAndUnzipM (\(name, ty) -> do
        (typedTy, defs) <- resolveSpecializationInType ty
        pure ((name, typedTy), defs)
        ) (Map.toList fields)
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

            when (Map.size instMap /= length args) $
                M.throw (M.InvalidArgumentQuantity (Map.size instMap) (length args))

            let s = Map.fromList (zip (Map.keys instMap) args) <> sub

            specializedFields <- Map.traverseWithKey (\_ ty -> M.applySubstitution s ty) instMap

            let specName = name <> "<" <> Text.intercalate "," (map toText args) <> ">"

            if Set.member specName specState.rememberedStructures
                then pure (HLIR.MkTyId specName, [])
                else do
                    let newStruct = HLIR.MkTopStructureDeclaration
                            { HLIR.header = HLIR.MkAnnotation specName []
                            , HLIR.fields = specializedFields
                            }

                    let newSpecializer = specState
                            { structures = Map.insert specName (HLIR.Forall [] specializedFields) specState.structures
                            , rememberedStructures = Set.insert specName specState.rememberedStructures
                            }

                    liftIO $ writeIORef defaultSpecializer newSpecializer

                    pure (HLIR.MkTyId specName, [newStruct])
        Nothing -> pure (header, [])

-- | Utility types

data Specializer = Specializer
    { variables :: Map Text (HLIR.Scheme HLIR.Type, HLIR.TLIR "toplevel")
    , structures :: Map Text (HLIR.Scheme (Map Text HLIR.Type))

    , rememberedVariables :: Set Text
    , rememberedStructures :: Set Text
    }
    deriving (Ord, Eq)

emptySpecializer :: Specializer
emptySpecializer = Specializer Map.empty Map.empty Set.empty Set.empty

defaultSpecializer :: IORef Specializer
defaultSpecializer = IO.unsafePerformIO $ newIORef emptySpecializer
