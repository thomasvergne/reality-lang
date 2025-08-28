module Language.Reality.Frontend.Import.Resolver where

import Control.Monad.Except qualified as M
import Control.Monad.Result qualified as M
import Data.Map qualified as Map
import Data.Text qualified as Text
import GHC.IO qualified as IO
import Language.Reality.Frontend.Parser qualified as P
import Language.Reality.Frontend.Parser.Toplevel qualified as P
import Language.Reality.Syntax.HLIR qualified as HLIR
import System.Directory qualified as IO
import System.FilePath qualified as IO

-- | Run the import resolver on a HLIR program.
-- | This function takes a list of toplevel nodes, and returns a list of toplevel
-- | nodes with imports resolved.
runImportResolver ::
    (MonadIO m, M.MonadError M.Error m) =>
    AbsolutePath ->
    [HLIR.HLIR "toplevel"] ->
    m [HLIR.HLIR "toplevel"]
runImportResolver cwd toplevels = do
    liftIO $ modifyIORef defaultModuleState $ \s -> s{cwd = cwd}
    resolveImports toplevels

-- | Resolve imports in a HLIR program.
-- | This function takes a list of toplevel nodes, and returns a list of toplevel
-- | nodes with imports resolved.
resolveImports ::
    (MonadIO m, M.MonadError M.Error m) =>
    [HLIR.HLIR "toplevel"] ->
    m [HLIR.HLIR "toplevel"]
resolveImports (n : ns) = do
    resolved <- resolveSingularNode n
    rest <- resolveImports ns
    pure (resolved ++ rest)
resolveImports [] = pure []

-- | Resolve a singular HLIR toplevel node.
-- | This function takes a toplevel node, and returns a list of toplevel nodes.
-- | This is used to resolve imports, as an import may resolve to multiple toplevel
-- | nodes.
resolveSingularNode ::
    (MonadIO m, M.MonadError M.Error m) =>
    HLIR.HLIR "toplevel" ->
    m [HLIR.HLIR "toplevel"]
resolveSingularNode (HLIR.MkTopImport paths) = do
    let shouldFlatten = viaNonEmpty last paths == Just "*"
        importPaths = if shouldFlatten then fromMaybe [] (viaNonEmpty init paths) else paths

    -- Here we would resolve the import paths to actual file paths.
    -- For simplicity, let's assume we have a function `resolveImportPath`
    -- that takes a list of path segments and returns an absolute file path.
    moduleState <- readIORef defaultModuleState
    let basePath = moduleState.cwd
    let absolutePath = basePath IO.</> toString (Text.intercalate "/" importPaths) IO.<.> "rl"
    let pkgName = Text.intercalate "::" importPaths

    visitModule absolutePath pkgName shouldFlatten
resolveSingularNode (HLIR.MkTopLocated p n) = do
    HLIR.pushPosition p

    nodes <- resolveSingularNode n

    void HLIR.popPosition

    pure $ map (HLIR.MkTopLocated p) nodes
resolveSingularNode n = pure [n]

-- | Visit a module to resolve its imports.
-- | This function takes a module, and returns a module with its imports resolved.
-- | This is used to resolve imports, as an import may resolve to multiple toplevel
-- | nodes.
visitModule ::
    (MonadIO m, M.MonadError M.Error m) =>
    AbsolutePath ->
    PackageName ->
    FlattenModule ->
    m [HLIR.HLIR "toplevel"]
visitModule absPath pkgName flatten = do
    let pkgAsFilePath = toString pkgName
    unlessM (liftIO $ IO.doesFileExist absPath) $ do
        M.throw $ M.ModuleNotFound pkgAsFilePath

    fileContent :: Text <- decodeUtf8 <$> readFileBS absPath

    result <- P.parseRealityFile absPath fileContent P.parseProgram

    case result of
        Left err -> M.throw $ M.ParseError err
        Right toplevels -> do
            moduleState <- readIORef defaultModuleState

            case Map.lookup absPath moduleState.modules of
                Just modState -> do
                    case modState.modState of
                        Unvisited -> processAST toplevels
                        Visiting -> M.throw $ M.CyclicModuleDependency absPath []
                        Visited ->
                            if flatten
                                then flattenOneLevel modState.modToplevels
                                else pure modState.modToplevels
                Nothing -> processAST toplevels
  where
    processAST ::
        (MonadIO m, M.MonadError M.Error m) =>
        [HLIR.HLIR "toplevel"] -> m [HLIR.HLIR "toplevel"]
    processAST nodes = do
        let newModule =
                Module
                    { modPath = absPath
                    , modPackage = pkgName
                    , modState = Unvisited
                    , modToplevels = []
                    }

        addNewModule newModule

        resolvedToplevels <- resolveImports nodes

        updateModuleState absPath Visited
        updateModuleToplevels absPath resolvedToplevels

        if flatten
            then flattenOneLevel resolvedToplevels
            else pure resolvedToplevels

-- | Flatten one leve of modules
-- | This function takes a list of toplevel nodes, and returns a list of toplevel
-- | nodes with all modules flattened one level.
flattenOneLevel ::
    (MonadIO m, M.MonadError M.Error m) =>
    [HLIR.HLIR "toplevel"] ->
    m [HLIR.HLIR "toplevel"]
flattenOneLevel (n@(HLIR.MkTopModuleDeclaration _ nodes) : ns) = do
    let newNodes = n : nodes
    rest <- flattenOneLevel ns
    pure (newNodes ++ rest)
flattenOneLevel (HLIR.MkTopLocated p n : ns) = do
    nodes <- flattenOneLevel [n]
    rest <- flattenOneLevel ns
    pure (map (HLIR.MkTopLocated p) nodes ++ rest)
flattenOneLevel (n : ns) = do
    rest <- flattenOneLevel ns
    pure (n : rest)
flattenOneLevel [] = pure []

-- | UTILITY TYPES
-- | These types are used to keep track of the state of the module resolution
-- | process.
-- | They are not exposed outside of this module.
type AbsolutePath = FilePath

type PackageName = Text
type FlattenModule = Bool

data ModuleVisitState
    = Unvisited
    | Visiting
    | Visited
    deriving (Eq, Show)

data Module = Module
    { modPath :: AbsolutePath
    , modPackage :: PackageName
    , modState :: ModuleVisitState
    , modToplevels :: [HLIR.HLIR "toplevel"]
    }
    deriving (Eq)

data ModuleState = ModuleState
    { modules :: Map AbsolutePath Module
    , cwd :: AbsolutePath
    }
    deriving (Eq)

defaultModuleState :: IORef ModuleState
defaultModuleState = IO.unsafePerformIO . newIORef $ ModuleState mempty "."

addNewModule :: (MonadIO m) => Module -> m ()
addNewModule newModule = do
    modifyIORef defaultModuleState $ \s ->
        s{modules = Map.insert newModule.modPath newModule s.modules}

updateModuleState :: (MonadIO m) => AbsolutePath -> ModuleVisitState -> m ()
updateModuleState path newState = do
    modifyIORef defaultModuleState $ \s ->
        s{modules = Map.adjust (\m -> m{modState = newState}) path s.modules}

updateModuleToplevels ::
    (MonadIO m) => AbsolutePath -> [HLIR.HLIR "toplevel"] -> m ()
updateModuleToplevels path newToplevels = do
    modifyIORef defaultModuleState $ \s ->
        s
            { modules = Map.adjust (\m -> m{modToplevels = newToplevels}) path s.modules
            }
