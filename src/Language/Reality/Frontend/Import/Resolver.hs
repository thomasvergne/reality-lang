module Language.Reality.Frontend.Import.Resolver (
    runImportResolver,
) where

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
import qualified Data.List as List

-- | Run the import resolver on a HLIR program.
-- | This function takes a list of toplevel nodes, and returns a list of toplevel
-- | nodes with imports resolved.
runImportResolver ::
    (MonadIO m, M.MonadError M.Error m) =>
    AbsolutePath ->
    Map Text AbsolutePath ->
    [HLIR.HLIR "toplevel"] ->
    m [HLIR.HLIR "toplevel"]
runImportResolver cwd aliases toplevels = do
    liftIO $ modifyIORef defaultModuleState $ \s -> s{cwd = cwd, pathAliases = aliases}
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

findLongestAlias :: 
    Map Text AbsolutePath ->
    [Text] ->
    Maybe (Text, AbsolutePath, Int)
findLongestAlias aliases paths =
    let matches = Map.toList $ Map.filterWithKey (\k _ -> k `elem` prefixes) aliases
        prefixes = [Text.intercalate "." (take i paths) | i <- [1 .. length paths]]
     in if null matches
            then Nothing
            else Just $ List.maximumBy (\(k1, _, _) (k2, _, _) -> compare (Text.length k1) (Text.length k2)) [(k, v, Text.count "." k + 1) | (k, v) <- matches]

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
        aliases = moduleState.pathAliases

    let (resolvedBasePath, remainingPaths) = case findLongestAlias aliases importPaths of
            Just (_, aliasPath, aliasLength) ->
                (aliasPath, drop aliasLength importPaths)
            Nothing -> (basePath, importPaths)

    let absolutePath 
            | null remainingPaths = resolvedBasePath
            | otherwise = resolvedBasePath IO.</> toString (Text.intercalate "/" remainingPaths) IO.-<.> "rl"
    let pkgName = Text.intercalate "." remainingPaths

    visitModule absolutePath pkgName shouldFlatten
resolveSingularNode (HLIR.MkTopLocated p n) = do
    HLIR.pushPosition p

    nodes <- resolveSingularNode n

    void HLIR.popPosition

    pure $ map (HLIR.MkTopLocated p) nodes
resolveSingularNode (HLIR.MkTopModuleDeclaration name body) = do
    resolvedBody <- resolveImports body
    pure [HLIR.MkTopModuleDeclaration name resolvedBody]
resolveSingularNode (HLIR.MkTopPublic n) = do
    resolved <- resolveSingularNode n
    pure (map HLIR.MkTopPublic resolved)
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
    -- Checking if the file exists
    let pkgAsFilePath = toString pkgName
    unlessM (liftIO $ IO.doesFileExist absPath) $ do
        M.throw $ M.ModuleNotFound pkgAsFilePath

    -- Reading the file content and parsing it
    fileContent :: Text <- decodeUtf8 <$> readFileBS absPath

    result <- P.parseRealityFile absPath fileContent P.parseProgram

    case result of
        Left err -> M.throw $ M.ParseError err
        Right toplevels -> do
            moduleState <- readIORef defaultModuleState

            -- Checking the module state to avoid cyclic dependencies
            -- and also avoiding re-processing ASTs, if the module has already
            -- been visited.
            case Map.lookup absPath moduleState.modules of
                Just modState -> do
                    case modState.modState of
                        Unvisited -> processAST toplevels
                        Visiting -> M.throw $ M.CyclicModuleDependency absPath []
                        Visited ->
                            -- We check if we should flatten the modules or not
                            -- based on the current import statement.
                            --
                            -- For instance, if we have:
                            --
                            -- import A::B::C::*
                            --
                            -- Then, we may want to flatten the module C one level.
                            if flatten
                                then flattenOneLevel modState.modToplevels
                                else pure modState.modToplevels
                -- If we found nothing, that means that the module has never been
                -- visited before, so we can process it.
                Nothing -> processAST toplevels
  where
    processAST ::
        (MonadIO m, M.MonadError M.Error m) =>
        [HLIR.HLIR "toplevel"] -> m [HLIR.HLIR "toplevel"]
    processAST nodes = do
        -- Creating a new module artifact and marking it as visiting
        -- to detect cyclic dependencies.
        --
        -- Then adding it to the module state.
        let newModule =
                Module
                    { modPath = absPath
                    , modPackage = pkgName
                    , modState = Visiting
                    , modToplevels = []
                    }

        oldCwd <- cwd <$> readIORef defaultModuleState
        modifyIORef' defaultModuleState $ \s ->
            s{ cwd = IO.takeDirectory absPath }

        addNewModule newModule

        -- Resolving imports in the module's toplevel nodes
        -- This is done recursively, as each import may resolve to
        -- multiple toplevel nodes.
        resolvedToplevels <- resolveImports nodes

        -- Marking the module as visited and updating its toplevel nodes
        -- in the module state.
        updateModuleState absPath Visited
        updateModuleToplevels absPath resolvedToplevels
        modifyIORef' defaultModuleState $ \s ->
            s{ cwd = oldCwd }

        -- We check if we should flatten the modules or not
        -- based on the current import statement.
        if flatten
            then flattenOneLevel resolvedToplevels
            else pure resolvedToplevels

-- | Flatten one leve of modules
-- | This function takes a list of toplevel nodes, and returns a list of toplevel
-- | nodes with all modules flattened one level.
-- | For instance, the following structure:
-- |
-- | mod A {
-- |    mod B {
-- |        const x = 1;
-- |    }
-- |    const y = 2;
-- | }
-- |
-- | Maybe be converted to:
-- |
-- | mod B {
-- |    const x = 1;
-- | }
-- | const y = 2;
flattenOneLevel ::
    (MonadIO m, M.MonadError M.Error m) =>
    [HLIR.HLIR "toplevel"] ->
    m [HLIR.HLIR "toplevel"]
flattenOneLevel ((HLIR.MkTopModuleDeclaration name nodes) : ns) = do
    nodes' <- removeExternals nodes
    let newNodes = nodes ++ [HLIR.MkTopModuleDeclaration name nodes']
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

removeExternals ::
    (MonadIO m, M.MonadError M.Error m) =>
    [HLIR.HLIR "toplevel"] ->
    m [HLIR.HLIR "toplevel"]
removeExternals (HLIR.MkTopExternalFunction{} : ns) =
    removeExternals ns
removeExternals (HLIR.MkTopLocated p n : ns) = do
    nodes <- removeExternals [n]
    rest <- removeExternals ns
    pure (map (HLIR.MkTopLocated p) nodes ++ rest)
removeExternals (n : ns) = do
    rest <- removeExternals ns
    pure (n : rest)
removeExternals [] = pure []

-- | UTILITY TYPES
-- | These types are used to keep track of the state of the module resolution
-- | process.
-- | They are not exposed outside of this module.

-- | An absolute path is a file path that is absolute, i.e. it starts from the root
-- | of the file system. We use this to uniquely identify modules.
type AbsolutePath = FilePath

-- | A package name is a string that uniquely identifies a package. We use this to
-- | keep track of which package a module belongs to.
-- | For instance, the package name for the module `Data.List` is `Data::List`.
type PackageName = Text

-- | A flatten module is a boolean that indicates whether to flatten the module or not.
-- | If true, the module will be flattened one level, i.e. all modules within the module
-- | will be lifted to the top level.
type FlattenModule = Bool

-- | MODULE STATE
-- | The module state is used to keep track of the state of each module during the
-- | resolution process. This is used to detect cyclic dependencies and to avoid
-- | reprocessing modules that have already been processed.
-- | The module state is stored in an IORef, which allows us to modify it during the
-- | resolution process.
data ModuleVisitState
    = Unvisited
    | Visiting
    | Visited
    deriving (Eq, Show)

-- | A module is a file that contains a set of toplevel nodes. We keep track of the
-- | module's path, package name, visit state, and toplevel nodes.
-- | - The module's path is used to uniquely identify the module, and the package name
-- |   is used to keep track of which package the module belongs to.
-- |
-- | - The visit state is used to detect cyclic dependencies, and the toplevel nodes are
-- |   the actual content of the module.
-- |
-- | - The module state is stored in an IORef, which allows us to modify it during the
-- |   resolution process.
-- |
-- | - The toplevel nodes are stored as a list of HLIR toplevel nodes, which allows us
-- |   to easily manipulate them during the resolution process.
data Module = Module
    { modPath :: AbsolutePath
    , modPackage :: PackageName
    , modState :: ModuleVisitState
    , modToplevels :: [HLIR.HLIR "toplevel"]
    }
    deriving (Eq)

-- | The module state is a map from module paths to modules, along with the current
-- | working directory. This allows us to easily look up modules by their path, and
-- | to keep track of the current working directory during the resolution process.
-- | The module state is stored in an IORef, which allows us to modify it during the
-- | resolution process.
data ModuleState = ModuleState
    { modules :: Map AbsolutePath Module
    , cwd :: AbsolutePath
    , pathAliases :: Map Text AbsolutePath
    }
    deriving (Eq)

defaultModuleState :: IORef ModuleState
defaultModuleState = IO.unsafePerformIO . newIORef $ ModuleState mempty "." mempty

-- | UTILITY FUNCTIONS
-- | These functions are used to manipulate the module state.
-- | They are not exposed outside of this module.
-- | They are used internally by the import resolver.
-- | They are used to add new modules, update module states, and update module
-- | toplevels.
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
