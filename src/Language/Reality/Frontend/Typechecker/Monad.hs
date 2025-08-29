module Language.Reality.Frontend.Typechecker.Monad where

import Data.Map qualified as Map
import Data.Text qualified as Text
import GHC.IO qualified as IO
import Language.Reality.Syntax.HLIR qualified as HLIR

data CheckerState = CheckerState
    { counter :: IORef Int
    , environment :: Map Text (HLIR.Scheme HLIR.Type)
    , typeAliases :: Map Text (HLIR.Scheme HLIR.Type)
    , structures :: Map Text (HLIR.Scheme (Map Text HLIR.Type))
    , implementations :: Map (Text, HLIR.Type) (HLIR.Scheme HLIR.Type)
    , properties :: Map Text (HLIR.Scheme HLIR.Type)
    }
    deriving (Eq, Ord, Generic)

type Substitution = Map Text HLIR.Type
type Constraints = [(Text, HLIR.Type, HLIR.Position)]

withEnvironment :: (MonadIO m) => Map Text (HLIR.Scheme HLIR.Type) -> m a -> m a
withEnvironment env action = do
    ref <- liftIO $ readIORef defaultCheckerState
    let oldEnv = ref.environment
    liftIO $ writeIORef defaultCheckerState ref{environment = Map.union env oldEnv}
    result <- action
    ref' <- liftIO $ readIORef defaultCheckerState
    liftIO $ writeIORef defaultCheckerState ref'{environment = oldEnv}
    pure result

defaultCheckerState :: IORef CheckerState
defaultCheckerState = IO.unsafePerformIO $ do
    ref <- newIORef 0
    newIORef
        CheckerState
            { counter = ref
            , environment = Map.empty
            , typeAliases = Map.empty
            , structures = Map.empty
            , implementations = Map.empty
            , properties = Map.empty
            }

newSymbol :: (MonadIO m) => Text -> m Text
newSymbol prefix = do
    ref <- liftIO $ readIORef defaultCheckerState
    let c = ref.counter

    liftIO $ modifyIORef c (+ 1)
    i <- liftIO $ readIORef c

    pure $ prefix <> Text.pack (show i)

newType :: (MonadIO m) => m HLIR.Type
newType = do
    sym <- newSymbol "t"
    ioref <- newIORef (HLIR.Unbound sym 0)
    pure $ HLIR.MkTyVar ioref

resetState :: (MonadIO m) => m ()
resetState = do
    liftIO
        $ writeIORef
            defaultCheckerState
            CheckerState
                { counter = IO.unsafePerformIO (newIORef 0)
                , environment = Map.empty
                , typeAliases = Map.empty
                , structures = Map.empty
                , implementations = Map.empty
                , properties = Map.empty
                }

instantiateAndSub ::
    (MonadIO m) => HLIR.Scheme HLIR.Type -> m (HLIR.Type, Map Text HLIR.Type)
instantiateAndSub (HLIR.Forall qvars schemeTy) = do
    newVars <- forM qvars $ const newType
    let s = Map.fromList (zip qvars newVars)

    ty <- applySubstitution s schemeTy

    pure (ty, s)

instantiateMapAndSub ::
    (MonadIO m) =>
    HLIR.Scheme (Map Text HLIR.Type) -> m (Map Text HLIR.Type, Map Text HLIR.Type)
instantiateMapAndSub (HLIR.Forall qvars schemeTy) = do
    newVars <- forM qvars $ const newType
    let s = Map.fromList (zip qvars newVars)
    ty <- Map.traverseWithKey (\_ ty -> applySubstitution s ty) schemeTy

    pure (ty, s)

instantiateWithSub ::
    (MonadIO m) => HLIR.Scheme HLIR.Type -> [HLIR.Type] -> m HLIR.Type
instantiateWithSub (HLIR.Forall qvars schemeTy) types = do
    let subst = zip qvars types
        rest = drop (length types) qvars
    newVars <- forM rest $ const newType
    let s' = Map.fromList (subst ++ zip rest newVars)
    applySubstitution s' schemeTy

instantiate :: (MonadIO m) => HLIR.Scheme HLIR.Type -> m HLIR.Type
instantiate (HLIR.Forall qvars schemeTy) = fst <$> instantiateAndSub (HLIR.Forall qvars schemeTy)

instantiateMap ::
    (MonadIO m) => HLIR.Scheme (Map Text HLIR.Type) -> m (Map Text HLIR.Type)
instantiateMap (HLIR.Forall qvars schemeTy) = fst <$> instantiateMapAndSub (HLIR.Forall qvars schemeTy)

applySubstitution ::
    (MonadIO m) => Map Text HLIR.Type -> HLIR.Type -> m HLIR.Type
applySubstitution s (HLIR.MkTyVar tvr) = do
    tv <- liftIO $ readIORef tvr
    case tv of
        HLIR.Link ty -> applySubstitution s ty
        HLIR.Unbound{} -> pure $ HLIR.MkTyVar tvr
applySubstitution s (HLIR.MkTyApp t ts) = do
    t' <- applySubstitution s t
    ts' <- mapM (applySubstitution s) ts
    pure $ HLIR.MkTyApp t' ts'
applySubstitution s (HLIR.MkTyId name) =
    case Map.lookup name s of
        Just ty -> pure ty
        Nothing -> pure $ HLIR.MkTyId name
applySubstitution s (HLIR.MkTyQuantified name) =
    case Map.lookup name s of
        Just ty -> pure ty
        Nothing -> pure $ HLIR.MkTyQuantified name
applySubstitution s (HLIR.MkTyAnonymousStructure fields) = do
    fields' <-
        Map.traverseWithKey
            (\_ ty -> applySubstitution s ty)
            fields
    pure $ HLIR.MkTyAnonymousStructure fields'
