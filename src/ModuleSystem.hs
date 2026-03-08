{-# LANGUAGE OverloadedStrings #-}

-- | Module system for tulam: path resolution, dependency graph, cycle detection,
-- and shared module loading functions.
module ModuleSystem
    ( resolveModulePath
    , buildDependencyGraph
    , topologicalSort
    , detectCycles
    , extractDependencies
    , extractModuleKey
    , extractImports
    , filterVisibleNames
    , extractPrivateNames
    , extractModulePath
    , extractExports
    , ModuleInfo(..)
    , ModuleGraph
    , modPathToKey
    , defaultLibPaths
    -- Shared loading functions (M2: single source of truth)
    , loadFileQuiet
    , loadFileWithCache
    , loadModuleTree
    , resolveAllDeps
    , runParseOnly
    , runModulePasses
    , CompiledModule(..)
    , baseModulePath
    , preludeModulePath
    ) where

import Surface (Name, ModulePath, Expr(..), ImportSpec(..), Lambda(..))
import State
import Pipeline (afterparserPass, actionDesugarPass, stringLiteralDesugarPass, buildEnvPass, recordDesugarPass, lamToCLMPass, timedPass)
import CaseOptimization (caseOptimizationPass, checkSealedExhaustiveness, positivityCheckPass, terminationCheckPass, coverageCheckPass)
import CLMOptimize (runCLMOptPasses)
import Parser (parseWholeFile)
import TypeCheck (typeCheckPass)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (get, put, modify, evalStateT, execStateT)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import System.FilePath ((</>), joinPath)
import System.Directory (doesFileExist)
import Data.HashMap.Strict as Map
import qualified Data.Set as Set
import Data.List (intercalate, nub)

import Util.IOLogger (initLogState)
import Interface (ModuleCache(..), writeModuleCache, loadCacheIfFresh, hashSource, toCachePath, ensureCacheDir)

-- | Module tree entry points
baseModulePath :: FilePath
baseModulePath = "lib/Base.tl"

preludeModulePath :: FilePath
preludeModulePath = "lib/Prelude.tl"

-- | Information about a loaded module
data ModuleInfo = ModuleInfo
    { modPath       :: ModulePath           -- ^ Qualified module path
    , modFilePath   :: FilePath             -- ^ Absolute file path on disk
    , modImports    :: [(ModulePath, ImportSpec, Maybe Name)]  -- ^ Imports (path, spec, target)
    , modExports    :: [(ModulePath, Maybe [Name])]            -- ^ Re-exports
    , modPublicNames  :: [Name]             -- ^ Public names (default visibility)
    , modPrivateNames :: [Name]             -- ^ Private names
    , modIsLoaded   :: Bool                 -- ^ Has been fully loaded
    } deriving (Show)

-- | Dependency graph: module path -> list of imported module paths
type ModuleGraph = HashMap String [String]

-- | Default library search paths
defaultLibPaths :: [FilePath]
defaultLibPaths = ["lib/"]

-- | Resolve a module path to a file path, trying each search directory.
-- Returns the first existing file path found, or Nothing.
resolveModulePath :: [FilePath] -> ModulePath -> IO (Maybe FilePath)
resolveModulePath searchPaths modName = go searchPaths
  where
    relPath = joinPath modName ++ ".tl"
    go [] = return Nothing
    go (dir:dirs) = do
        let fullPath = dir </> relPath
        exists <- doesFileExist fullPath
        if exists then return (Just fullPath)
        else go dirs

-- | Extract the module key from parsed expressions (from ModuleDecl).
-- Returns the module name as a dot-separated string, or Nothing if no ModuleDecl found.
extractModuleKey :: [Expr] -> Maybe String
extractModuleKey [] = Nothing
extractModuleKey (ModuleDecl path : _) = Just (modPathToKey path)
extractModuleKey (_ : rest) = extractModuleKey rest

-- | Extract all tulam module dependencies from parsed expressions.
-- Includes imports, opens, and exports (re-exports are dependencies too).
-- Skips target-specific imports (those with Just target).
extractDependencies :: [Expr] -> [ModulePath]
extractDependencies = nub . go
  where
    go [] = []
    go (Import path _ Nothing : rest) = path : go rest     -- tulam import (no target)
    go (Import _ _ (Just _) : rest) = go rest               -- target import, skip
    go (Open path : rest) = path : go rest                   -- open = import + re-export
    go (Export path _ : rest) = path : go rest               -- re-exports are deps
    go (_ : rest) = go rest

-- | Extract the module path from parsed expressions (from ModuleDecl).
extractModulePath :: [Expr] -> ModulePath
extractModulePath [] = []
extractModulePath (ModuleDecl path : _) = path
extractModulePath (_ : rest) = extractModulePath rest

-- | Extract export declarations from parsed expressions.
extractExports :: [Expr] -> [(ModulePath, Maybe [Name])]
extractExports [] = []
extractExports (Export path names : rest) = (path, names) : extractExports rest
extractExports (_ : rest) = extractExports rest

-- | Extract names declared as private. Looks at PrivateDecl wrappers
-- and extracts the inner declaration's name.
extractPrivateNames :: [Expr] -> Set.Set Name
extractPrivateNames = Set.fromList . concatMap go
  where
    go (PrivateDecl inner) = declNames inner
    go _ = []
    declNames (Function lam) = [lamName lam]
    declNames (SumType lam) = lamName lam : consNames (body lam)
    declNames (Structure lam _) = [lamName lam]
    declNames (Primitive lam) = [lamName lam]
    declNames (Action lam) = [lamName lam]
    declNames _ = []
    consNames (Constructors cs) = Prelude.map lamName cs
    consNames _ = []

-- | Extract module imports from a list of parsed top-level expressions.
extractImports :: [Expr] -> [(ModulePath, ImportSpec, Maybe Name)]
extractImports [] = []
extractImports (Import path spec tgt : rest) = (path, spec, tgt) : extractImports rest
extractImports (Open path : rest) = (path, ImportAll, Nothing) : extractImports rest
extractImports (_ : rest) = extractImports rest

-- | Build a dependency graph from a map of module paths to their expressions.
buildDependencyGraph :: HashMap String [Expr] -> ModuleGraph
buildDependencyGraph modules = Map.mapWithKey (\_ exprs ->
    nub [modPathToKey path | (path, _, Nothing) <- extractImports exprs]
    ) modules

-- | Convert a module path to a graph key
modPathToKey :: ModulePath -> String
modPathToKey = intercalate "."

-- | Detect cycles in the dependency graph. Returns Nothing if acyclic,
-- or Just the cycle (list of module names) if a cycle exists.
detectCycles :: ModuleGraph -> Maybe [String]
detectCycles graph = go (Map.keys graph) [] []
  where
    go [] _ _ = Nothing
    go (node:rest) visited stack
        | node `elem` stack = Just (dropWhile (/= node) stack ++ [node])
        | node `elem` visited = go rest visited stack
        | otherwise = case dfs node (node : stack) of
            Just cycle -> Just cycle
            Nothing -> go rest (node : visited) stack
    dfs node stack =
        let deps = maybe [] id (Map.lookup node graph)
        in goNeighbors deps stack
    goNeighbors [] _ = Nothing
    goNeighbors (dep:deps) stack
        | dep `elem` stack = Just (dropWhile (/= dep) stack ++ [dep])
        | otherwise = case dfs dep (dep : stack) of
            Just cycle -> Just cycle
            Nothing -> goNeighbors deps stack

-- | Topological sort of the dependency graph (Kahn's algorithm).
-- Returns Left if cycles detected, Right with sorted order otherwise.
topologicalSort :: ModuleGraph -> Either [String] [String]
topologicalSort graph =
    case detectCycles graph of
        Just cycle -> Left cycle
        Nothing -> Right (reverse $ go initQueue Map.empty [])
  where
    -- In-degree count for each node
    allNodes = nub $ Map.keys graph ++ concat (Map.elems graph)
    inDegrees = Prelude.foldl (\m node ->
        let deps = maybe [] id (Map.lookup node graph)
        in Prelude.foldl (\m' dep -> Map.insertWith (+) dep (1::Int) m') m deps
        ) (Map.fromList [(n, 0::Int) | n <- allNodes]) allNodes
    -- Start with nodes that have in-degree 0
    initQueue = [n | (n, d) <- Map.toList inDegrees, d == 0]
    -- Process queue
    go [] _ result = result
    go (node:queue) processed result =
        if Map.member node processed then go queue processed result
        else
            let deps = maybe [] id (Map.lookup node graph)
                processed' = Map.insert node () processed
                -- Find newly unblocked nodes
                newQueue = [dep | dep <- deps, allDepsProcessed dep processed']
            in go (queue ++ newQueue) processed' (node : result)
    allDepsProcessed node processed =
        let incomingFrom = [src | (src, dsts) <- Map.toList graph, node `elem` dsts]
        in Prelude.all (\src -> Map.member src processed) incomingFrom

--------------------------------------------------------------------------------
-- Module visibility enforcement (M3)
--------------------------------------------------------------------------------

-- | Filter which names from a module are visible based on an ImportSpec.
-- Given a module's public names and an import spec, returns the visible names.
filterVisibleNames :: Set.Set Name -> ImportSpec -> Set.Set Name
filterVisibleNames publicNames ImportAll = publicNames
filterVisibleNames publicNames (ImportOnly names) =
    Set.intersection publicNames (Set.fromList names)
filterVisibleNames publicNames (ImportHiding names) =
    Set.difference publicNames (Set.fromList names)
filterVisibleNames publicNames (ImportAs _) = publicNames  -- qualified access handled separately

-- | Get the visible names from a loaded module, respecting import spec.
-- Private names are never visible to other modules.
getModuleVisibleNames :: String -> ImportSpec -> HashMap String ModuleEnv -> Set.Set Name
getModuleVisibleNames modKey spec loadedMods =
    case Map.lookup modKey loadedMods of
        Nothing   -> Set.empty
        Just menv -> filterVisibleNames (publicNames menv) spec

--------------------------------------------------------------------------------
-- Shared module loading functions (M2: single source of truth)
--------------------------------------------------------------------------------

-- | Result of compiling a single module.
data CompiledModule = CompiledModule
    { cmModuleKey   :: String           -- ^ e.g. "Algebra.Eq" or file path
    , cmModulePath  :: ModulePath       -- ^ e.g. ["Algebra", "Eq"]
    , cmPublicNames :: Set.Set Name     -- ^ names visible to downstream modules
    , cmPrivateNames :: Set.Set Name    -- ^ names hidden from downstream modules
    , cmImports     :: [(ModulePath, ImportSpec, Maybe Name)]
    , cmExports     :: [(ModulePath, Maybe [Name])]
    } deriving (Show)

-- | Run all compilation passes (0 through 4.5) on the current parsedModule.
-- Assumes parsedModule is already populated with this module's expressions.
-- Modifies currentEnvironment in place (adds types, constructors, lambdas, CLM, etc.).
runModulePasses :: IntState ()
runModulePasses = do
    timedPass "Pass 0 (desugar)" afterparserPass
    clearAllLogs
    timedPass "Pass 0.25 (string desugar)" stringLiteralDesugarPass
    clearAllLogs
    timedPass "Pass 0.5 (action desugar)" actionDesugarPass
    clearAllLogs
    timedPass "Pass 1 (env build)" buildEnvPass
    clearAllLogs
    timedPass "Pass 1.5 (record desugar)" recordDesugarPass
    clearAllLogs
    timedPass "Pass 2 (case opt)" caseOptimizationPass
    checkSealedExhaustiveness
    positivityCheckPass
    coverageCheckPass
    clearAllLogs
    timedPass "Pass 3 (typecheck)" typeCheckPass
    terminationCheckPass
    clearAllLogs
    timedPass "Pass 4 (CLM)" lamToCLMPass
    clearAllLogs
    timedPass "Pass 4.5 (CLM opt)" runCLMOptPasses
    clearAllLogs

-- | Finalize a compiled module: compute public/private names, build ModuleEnv,
-- enforce visibility, and write cache if applicable.
finalizeModule :: String -> T.Text -> Set.Set Name -> HashMap String ModuleEnv
               -> [Expr] -> IntState CompiledModule
finalizeModule nm fileText namesBefore prevLoadedMods exprs = do
    st' <- get
    let namesAfter = allEnvNames (currentEnvironment st')
    let newNames = Set.difference namesAfter namesBefore
    let privDeclNames = extractPrivateNames exprs
    let pubNames = Set.difference newNames privDeclNames
    let privNames = Set.intersection newNames privDeclNames
    let modPath = extractModulePath exprs
    let imports = extractImports exprs
    let exports = extractExports exprs
    let modKey = case modPath of
            [] -> nm
            path -> modPathToKey path
    let menv = ModuleEnv
            { moduleName = modPath
            , publicNames = pubNames
            , privateNames = privNames
            , moduleImports = imports
            , moduleExports = exports
            , loadedModules = Map.insert modKey
                (ModuleEnv modPath pubNames privNames imports exports Map.empty)
                prevLoadedMods
            }
    let env' = if Set.null privNames
            then currentEnvironment st'
            else removeNames privNames (currentEnvironment st')
    put $ st' { currentEnvironment = env'
              , currentModuleEnv = menv
              }
    -- Record source hash for dependency tracking
    let srcHash = hashSource fileText
    modify (\s -> s { moduleSourceHashes = Map.insert modKey srcHash (moduleSourceHashes s) })
    -- Write module cache for modules with proper module declarations only
    case modPath of
        [] -> return ()
        _  -> do
            st'' <- get
            let depModKeys = [modPathToKey dp | (dp, _, Nothing) <- imports]
            let depsHashes = [(dk, h) | dk <- depModKeys
                                      , Just h <- [Map.lookup dk (moduleSourceHashes st'')]]
            let envSlice = sliceEnvironment pubNames (currentEnvironment st'')
            liftIO $ writeModuleCache ModuleCache
                { mcVersion     = 1
                , mcModuleKey   = modKey
                , mcSourceHash  = srcHash
                , mcDepsHashes  = depsHashes
                , mcEnvironment = envSlice
                }
    return $ CompiledModule modKey modPath pubNames privNames imports exports

-- | Quiet version of file loading — runs all passes without printing.
-- Tracks per-module defined names and stores in loadedModules.
-- Phase 2A: Scopes parsedModule per module — saves/restores so each module's
-- passes only see that module's parsed expressions.
loadFileQuiet :: String -> IntState ()
loadFileQuiet nm = do
    st <- get
    -- Phase 2A: Save the accumulated parsedModule from previous modules
    let prevParsedModule = parsedModule st
    -- Clear parsedModule so parsing only produces this module's expressions
    put $ st { parsedModule = [] }
    fileText <- liftIO (TIO.readFile nm)
    res <- parseWholeFile fileText nm
    st2 <- get
    -- Snapshot environment names before this module's compilation
    let namesBefore = allEnvNames (currentEnvironment st2)
    -- Reset module env for this module, preserving loadedModules
    let prevLoadedMods = loadedModules (currentModuleEnv st2)
    -- Store source text for error display
    let srcs = Map.insert nm fileText (loadedSources st2)
    put $ st2 { currentSource = fileText
              , currentModuleEnv = emptyModuleEnv { loadedModules = prevLoadedMods }
              , loadedSources = srcs
              }
    case res of
        Left _err -> do
            liftIO $ putStrLn $ "Error loading " ++ nm ++ ": parse error"
            -- Phase 2A: Restore parsedModule even on parse failure
            modify (\s -> s { parsedModule = prevParsedModule })
        Right exprs -> do
            runModulePasses
            _cm <- finalizeModule nm fileText namesBefore prevLoadedMods exprs
            -- Phase 2A: Restore accumulated parsedModule (this module's + all previous)
            st' <- get
            let allParsed = parsedModule st' ++ prevParsedModule
            modify (\s -> s { parsedModule = allParsed })

-- | Load a module from cache if fresh, otherwise compile from source and cache.
loadFileWithCache :: String -> String -> IntState ()
loadFileWithCache modKey filePath = do
    fileText <- liftIO (TIO.readFile filePath)
    let srcHash = hashSource fileText
    -- Pass all known module source hashes for dependency freshness checking
    st0 <- get
    mCache <- liftIO $ loadCacheIfFresh modKey fileText (moduleSourceHashes st0)
    case mCache of
        Just mc -> do
            -- Cache hit: merge cached environment slice directly
            modify (\s -> s {
                currentEnvironment = mergeEnvironment (currentEnvironment s) (mcEnvironment mc),
                -- Record source hash so downstream modules can track this as a dep
                moduleSourceHashes = Map.insert modKey srcHash (moduleSourceHashes s)
            })
            -- Also register in loadedModules so downstream visibility queries work
            st <- get
            let prevLoadedMods = loadedModules (currentModuleEnv st)
            let pubNames = allEnvNames (mcEnvironment mc)
            let menv = (currentModuleEnv st) {
                    loadedModules = Map.insert modKey
                        (ModuleEnv [] pubNames Set.empty [] [] Map.empty)
                        prevLoadedMods
                }
            put $ st { currentModuleEnv = menv }
        Nothing -> do
            -- Cache miss: full compile (loadFileQuiet writes cache + records hash)
            loadFileQuiet filePath

-- | Load a module tree starting from an entry file.
-- Resolves all dependencies recursively, topologically sorts them,
-- and loads each module in order. Always loads Prelude first.
-- Uses binary cache when available for fast loading.
loadModuleTree :: FilePath -> IntState ()
loadModuleTree entryFile = do
    st <- get
    let searchPaths = libSearchPaths st
    -- Ensure cache directory exists
    liftIO ensureCacheDir
    -- Load prelude first (primitives, no dependencies)
    liftIO $ putStrLn "  Loading Prelude..."
    loadFileWithCache "Prelude" preludeModulePath
    -- Parse entry file to get its dependencies
    liftIO $ putStrLn "  Resolving module dependencies..."
    allModules <- liftIO $ resolveAllDeps searchPaths Set.empty [entryFile]
    -- Load each module in order (prelude already loaded, skip it)
    liftIO $ putStrLn $ "  Loading " ++ show (length allModules) ++ " modules..."
    mapM_ (\(modKey, filePath) -> do
        liftIO $ putStrLn $ "    " ++ modKey
        loadFileWithCache modKey filePath
        ) allModules
    liftIO $ putStrLn "  All modules loaded."

-- | Recursively resolve all dependencies starting from a list of files.
-- Returns modules in dependency order (dependencies first).
resolveAllDeps :: [FilePath] -> Set.Set String -> [FilePath] -> IO [(String, FilePath)]
resolveAllDeps _ _ [] = return []
resolveAllDeps searchPaths loaded (filePath:rest) = do
    fileText <- TIO.readFile filePath
    parseResult <- runParseOnly fileText filePath
    case parseResult of
        Left _ -> resolveAllDeps searchPaths loaded rest  -- skip unparseable files
        Right exprs -> do
            let modKey = case extractModuleKey exprs of
                    Just k  -> k
                    Nothing -> filePath  -- fallback to file path
            if Set.member modKey loaded || modKey == "Prelude"
                then resolveAllDeps searchPaths loaded rest
                else do
                    -- Resolve dependencies first
                    let deps = extractDependencies exprs
                    depFiles <- mapM (resolveModulePath searchPaths) deps
                    let validDeps = [fp | Just fp <- depFiles]
                    -- Recurse into dependencies
                    depModules <- resolveAllDeps searchPaths loaded validDeps
                    let loaded' = Set.union loaded (Set.fromList [k | (k, _) <- depModules])
                    -- Now add this module if not yet loaded
                    if Set.member modKey loaded'
                        then resolveAllDeps searchPaths loaded' rest
                        else do
                            restModules <- resolveAllDeps searchPaths (Set.insert modKey loaded') rest
                            return $ depModules ++ [(modKey, filePath)] ++ restModules

-- | Parse a file just to extract expressions (no state effects).
-- Runs the full parser in a fresh monad stack and discards the state.
runParseOnly :: T.Text -> FilePath -> IO (Either String [Expr])
runParseOnly text filePath = do
    result <- evalStateT (evalStateT (parseWholeFile text filePath) emptyIntState) initLogState
    case result of
        Left err  -> return $ Left (show err)
        Right exs -> return $ Right exs
