module Language.Reality.Frontend.Typechecker.Unification where

import Control.Monad.Except qualified as M
import Control.Monad.Result qualified as M
import Data.Map qualified as Map
import Language.Reality.Frontend.Typechecker.Monad qualified as M
import Language.Reality.Syntax.HLIR qualified as HLIR

-- | SUBTYPING RELATION
-- | Check if a type is a subtype of another type.
-- | This is used to check if a type can be assigned to another type.
-- | For example, an Int can be assigned to a Float, but not vice versa.
-- | This function takes two types, and returns a boolean indicating if the first type
-- | is a subtype of the second type.
-- | It also returns a list of type substitutions that were made during the check.
-- | If the types are not compatible, it throws a TypeMismatch error.
isSubtypeOf ::
    (MonadIO m, M.MonadError M.Error m) =>
    HLIR.Type ->
    HLIR.Type ->
    m M.Substitution
isSubtypeOf t1 t2 = do
    aliasedT1 <- performAliasRemoval t1
    aliasedT2 <- performAliasRemoval t2

    if aliasedT1 == aliasedT2
        then pure Map.empty
        else applySubtypeRelation True aliasedT1 aliasedT2

-- | Simplify and remove type aliases from a type.
-- | This is used to prepare a type for unification or subtype checking.
-- | This function takes a type, and returns a simplified type with no aliases.
-- | If the type contains aliases that cannot be resolved, it throws an error.
performAliasRemoval ::
    (MonadIO m, M.MonadError M.Error m) => HLIR.Type -> m HLIR.Type
performAliasRemoval ty = do
    simplTy <- HLIR.simplify ty
    removeAliases simplTy

removeAliases :: (MonadIO m, M.MonadError M.Error m) => HLIR.Type -> m HLIR.Type
removeAliases (HLIR.MkTyApp (HLIR.MkTyId base) args) = do
    typeAliases <- M.typeAliases <$> liftIO (readIORef M.defaultCheckerState)

    case Map.lookup base typeAliases of
        Just (HLIR.Forall qvars aliasedType) -> do
            when (length qvars /= length args)
                $ M.throw (M.InvalidArgumentQuantity (length qvars) (length args))

            let s = Map.fromList (zip qvars args)

            M.applySubstitution s aliasedType >>= removeAliases
        Nothing -> do
            newBase <- removeAliases (HLIR.MkTyId base)
            newArgs <- mapM removeAliases args
            pure (HLIR.MkTyApp newBase newArgs)
removeAliases (HLIR.MkTyId name) = do
    typeAliases <- M.typeAliases <$> liftIO (readIORef M.defaultCheckerState)

    case Map.lookup name typeAliases of
        Just (HLIR.Forall [] aliasedType) -> removeAliases aliasedType
        _ -> pure (HLIR.MkTyId name)
removeAliases (HLIR.MkTyVar ref) = do
    ty <- liftIO $ readIORef ref
    case ty of
        HLIR.Link ty' -> removeAliases ty'
        HLIR.Unbound{} -> pure (HLIR.MkTyVar ref)
removeAliases (HLIR.MkTyQuantified name) = pure (HLIR.MkTyQuantified name)
removeAliases (HLIR.MkTyAnonymousStructure n fields) = do
    n' <- removeAliases n
    newFields <- traverse removeAliases fields
    pure (HLIR.MkTyAnonymousStructure n' newFields)
removeAliases (HLIR.MkTyApp base args) =
    HLIR.MkTyApp <$> removeAliases base <*> mapM removeAliases args

-- | Should the unification mutate the type variables
-- | - If True, the type variables will be mutated to point to the unified type.
-- | - If False, the type variables will not be mutated, and only a substitution
-- |   will be returned.
type ShouldMutate = Bool

-- | APPLY SUBTYPE RELATION
-- | Apply the subtype relation between two types.
-- | This function takes two types, and returns a substitution that makes the first
-- | type a subtype of the second type.
-- | If the types are not compatible, it throws a TypeMismatch error.
-- | The ShouldMutate parameter indicates whether the type variables should be mutated
-- | to point to the unified type.
-- |
-- | The rules for subtyping are as follows:
-- | - Function types are contravariant in their arguments and covariant in their return type :
-- |   A1 -> R1 <: A2 -> R2 if A2 <: A1 and R1 <: R2
-- |
-- | - Type variables can be unified with any type, as long as they do not create
-- |   a cyclic dependency.
-- |
-- | - Type applications are covariant in their arguments:
-- |   F[A1, A2, ..., An] <: F[B1, B2, ..., Bn] if Ai <: Bi for all i
-- |
-- | - Named types are equal if they have the same name, or if they are both integer
-- |   types with the same or compatible sizes (e.g., i32 <: i64).
-- |   Similarly for unsigned integer types (u8, u16, u32, u64, u128)
-- |   and floating-point types (f16, f32, f64, f128).
-- |   An integer type can be a subtype of an unsigned integer type if its size
-- |   is strictly less (e.g., i32 <: u64).
-- |
-- |   It should normally check by bound checking but for simplicity we just check sizes here.
-- |
-- | - All other types must be exactly equal to be considered subtypes.
applySubtypeRelation ::
    (MonadIO m, M.MonadError M.Error m) =>
    ShouldMutate ->
    HLIR.Type ->
    HLIR.Type ->
    m M.Substitution
applySubtypeRelation shouldMutate (argsF1 HLIR.:->: retF1) (argsF2 HLIR.:->: retF2)
    | length argsF1 == length argsF2 = do
        subsArgs <-
            mconcat <$> zipWithM (flip (applySubtypeRelation shouldMutate)) argsF2 argsF1
        subRet <- applySubtypeRelation shouldMutate retF1 retF2

        pure (subsArgs <> subRet)
    | otherwise =
        M.throw (M.InvalidArgumentQuantity (length argsF1) (length argsF2))
applySubtypeRelation shouldMutate t1@(HLIR.MkTyVar ref1) t2 | t1 /= t2 = do
    ty1 <- readIORef ref1

    case ty1 of
        HLIR.Unbound name1 _ -> do
            occursCheck name1 t2

            when shouldMutate $ writeIORef ref1 (HLIR.Link t2)

            pure (Map.singleton name1 t2)
        HLIR.Link ty1' -> applySubtypeRelation shouldMutate ty1' t2
applySubtypeRelation shouldMutate t1 t2@(HLIR.MkTyVar ref2) | t1 /= t2 = do
    ty2 <- readIORef ref2

    case ty2 of
        HLIR.Unbound name2 _ -> do
            occursCheck name2 t1

            when shouldMutate $ writeIORef ref2 (HLIR.Link t1)

            pure (Map.singleton name2 t1)
        HLIR.Link ty2' -> applySubtypeRelation shouldMutate t1 ty2'
applySubtypeRelation shouldMutate (HLIR.MkTyApp base1 args1) (HLIR.MkTyApp base2 args2)
    | length args1 == length args2 = do
        sub <- applySubtypeRelation shouldMutate base1 base2
        subs <- mconcat <$> zipWithM (applySubtypeRelation shouldMutate) args1 args2
        pure (sub <> subs)
    | otherwise = M.throw (M.InvalidArgumentQuantity (length args1) (length args2))
applySubtypeRelation _ (HLIR.MkTyId name1) (HLIR.MkTyId name2)
    | name1 == name2 = pure Map.empty
    | Just subsize <- getIntegerPart (HLIR.MkTyId name1)
    , Just supSize <- getIntegerPart (HLIR.MkTyId name2)
    , subsize <= supSize =
        pure Map.empty
    | Just subsize <- getUnsignedIntegerPart (HLIR.MkTyId name1)
    , Just supSize <- getUnsignedIntegerPart (HLIR.MkTyId name2)
    , subsize <= supSize =
        pure Map.empty
    | Just subsize <- getFloatPart (HLIR.MkTyId name1)
    , Just supSize <- getFloatPart (HLIR.MkTyId name2)
    , subsize <= supSize =
        pure Map.empty
    | Just subsize <- getIntegerPart (HLIR.MkTyId name1)
    , Just supSize <- getUnsignedIntegerPart (HLIR.MkTyId name2)
    , subsize < supSize =
        pure Map.empty
    | Just subsize <- getUnsignedIntegerPart (HLIR.MkTyId name1)
    , Just supSize <- getIntegerPart (HLIR.MkTyId name2)
    , subsize < supSize =
        pure Map.empty
applySubtypeRelation _ t1 t2 | t1 /= t2 = M.throw (M.UnificationFail t1 t2)
applySubtypeRelation _ _ _ = pure Map.empty

-- | OCCURS CHECK
-- | Check if a type variable occurs in a type.
-- | This is used to prevent cyclic dependencies when unifying types.
-- | This function takes a type variable name and a type, and throws a CyclicTypeVariable
-- | error if the type variable occurs in the type.
-- | Otherwise, it does nothing.
occursCheck :: (MonadIO m, M.MonadError M.Error m) => Text -> HLIR.Type -> m ()
occursCheck name t@(HLIR.MkTyVar ref) = do
    ty <- readIORef ref
    case ty of
        HLIR.Link ty' -> occursCheck name ty'
        HLIR.Unbound name' _ ->
            when (name == name')
                $ M.throw (M.CyclicTypeVariable name t)
occursCheck name (HLIR.MkTyApp base args) = do
    occursCheck name base
    mapM_ (occursCheck name) args
occursCheck _ _ = pure ()

-- | GET INTEGER PART
-- | Get the size of an integer type.
-- | This is used to check if an integer type can be a subtype of another integer type.
-- | This function takes a type, and returns the size of the integer type if it is
-- | an integer type, or Nothing otherwise.
-- | It supports the following integer types: i8, i16, i32, i64, i128.
getIntegerPart :: HLIR.Type -> Maybe Int
getIntegerPart (HLIR.MkTyId "i8") = Just 8
getIntegerPart (HLIR.MkTyId "i16") = Just 16
getIntegerPart (HLIR.MkTyId "i32") = Just 32
getIntegerPart (HLIR.MkTyId "i64") = Just 64
getIntegerPart (HLIR.MkTyId "i128") = Just 128
getIntegerPart _ = Nothing

-- | GET UNSIGNED INTEGER PART
-- | Get the size of an unsigned integer type.
-- | This is used to check if an unsigned integer type can be a subtype of another
-- | unsigned integer type.
-- | This function takes a type, and returns the size of the unsigned integer type
-- | if it is an unsigned integer type, or Nothing otherwise.
-- | It supports the following unsigned integer types: u8, u16, u32, u64, u128.
getUnsignedIntegerPart :: HLIR.Type -> Maybe Int
getUnsignedIntegerPart (HLIR.MkTyId "u8") = Just 8
getUnsignedIntegerPart (HLIR.MkTyId "u16") = Just 16
getUnsignedIntegerPart (HLIR.MkTyId "u32") = Just 32
getUnsignedIntegerPart (HLIR.MkTyId "u64") = Just 64
getUnsignedIntegerPart (HLIR.MkTyId "u128") = Just 128
getUnsignedIntegerPart _ = Nothing

-- | GET FLOAT PART
-- | Get the size of a floating-point type.
-- | This is used to check if a floating-point type can be a subtype of another
-- | floating-point type.
-- | This function takes a type, and returns the size of the floating-point type
-- | if it is a floating-point type, or Nothing otherwise.
-- | It supports the following floating-point types: f16, f32, f64, f128.
getFloatPart :: HLIR.Type -> Maybe Int
getFloatPart (HLIR.MkTyId "f16") = Just 16
getFloatPart (HLIR.MkTyId "f32") = Just 32
getFloatPart (HLIR.MkTyId "f64") = Just 64
getFloatPart (HLIR.MkTyId "f128") = Just 128
getFloatPart _ = Nothing
