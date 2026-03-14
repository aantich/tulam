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
    , runPhase1Passes
    , runPhase2Passes
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
    runPhase1Passes
    runPhase2Passes

-- | Phase 1: Parse through case optimization (Passes 0-2).
-- Builds the environment but does NOT type-check or generate CLM.
-- Used in two-phase compilation: all modules run Phase 1 first.
runPhase1Passes :: IntState ()
runPhase1Passes = do
    timedPass "Pass 0 (desugar)" afterparserPass
    flushLogs
    timedPass "Pass 0.25 (string desugar)" stringLiteralDesugarPass
    flushLogs
    timedPass "Pass 0.5 (action desugar)" actionDesugarPass
    flushLogs
    timedPass "Pass 1 (env build)" buildEnvPass
    flushLogs
    timedPass "Pass 1.5 (record desugar)" recordDesugarPass
    flushLogs
    timedPass "Pass 2 (case opt)" caseOptimizationPass
    checkSealedExhaustiveness
    positivityCheckPass
    coverageCheckPass
    flushLogs

-- | Phase 2: Type checking through CLM (Passes 3-4.5).
-- Assumes Phase 1 has run and currentEnvironment is fully populated.
runPhase2Passes :: IntState ()
runPhase2Passes = do
    -- Reset TC error count before type checking this module
    modify (\s -> s { tcErrorCount = 0 })
    timedPass "Pass 3 (typecheck)" typeCheckPass
    terminationCheckPass
    flushLogs
    -- In strict mode, halt pipeline if type errors were found in this module
    st <- get
    let moduleErrors = tcErrorCount st
    let shouldHalt = strictTypes (currentFlags st) && moduleErrors > 0
    if shouldHalt
      then
        compilerMsg Errors $ "[TC] " ++ show moduleErrors
             ++ " type error(s) found. Halting compilation (strict mode)."
      else do
        timedPass "Pass 4 (CLM)" lamToCLMPass
        flushLogs
        timedPass "Pass 4.5 (CLM opt)" runCLMOptPasses
        flushLogs

-- | Finalize a compiled module: compute public/private names, build ModuleEnv,
-- enforce visibility, and write cache if applicable.
finalizeModule :: String -> T.Text -> Set.Set Name -> HashMap String ModuleEnv
               -> [Expr] -> IntState CompiledModule
finalizeModule nm fileText namesBefore _prevLoadedMods exprs = do
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
    -- Use CURRENT loadedModules (not stale Phase 1 snapshot) so all previously
    -- finalized modules are visible
    let currentLoaded = loadedModules (currentModuleEnv st')
    let menv = ModuleEnv
            { moduleName = modPath
            , publicNames = pubNames
            , privateNames = privNames
            , moduleImports = imports
            , moduleExports = exports
            , loadedModules = Map.insert modKey
                (ModuleEnv modPath pubNames privNames imports exports Map.empty)
                currentLoaded
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

-- | Two-phase file loading for a single file.
-- Phase 1 (env build + case opt) then immediately Phase 2 (TC + CLM).
-- Used for REPL :load and test program loading (single files on top of existing env).
loadFileQuiet :: String -> IntState ()
loadFileQuiet nm = do
    loadFileQuietPhase1 nm
    runModulePhase2 nm

-- | Phase 1 only version of loadFileQuiet.
-- Runs Passes 0-2 (desugar, env build, case opt) but NOT type checking or CLM.
-- Saves the parsedModule to moduleParsedExprs for Phase 2 retrieval.
-- Does NOT call finalizeModule (deferred to Phase 2).
loadFileQuietPhase1 :: String -> IntState ()
loadFileQuietPhase1 nm = do
    st <- get
    let prevParsedModule = parsedModule st
    put $ st { parsedModule = [] }
    fileText <- liftIO (TIO.readFile nm)
    res <- parseWholeFile fileText nm
    st2 <- get
    -- Snapshot environment names BEFORE this module's Phase 1
    let namesBefore = allEnvNames (currentEnvironment st2)
    let prevLoadedMods = loadedModules (currentModuleEnv st2)
    let srcs = Map.insert nm fileText (loadedSources st2)
    put $ st2 { currentSource = fileText
              , currentModuleEnv = emptyModuleEnv { loadedModules = prevLoadedMods }
              , loadedSources = srcs
              }
    case res of
        Left _err -> do
            liftIO $ putStrLn $ "Error loading " ++ nm ++ ": parse error"
            modify (\s -> s { parsedModule = prevParsedModule })
        Right _exprs -> do
            runPhase1Passes
            -- Save this module's parsed expressions + snapshot for Phase 2
            st' <- get
            let thisModuleParsed = parsedModule st'
            modify (\s -> s { moduleParsedExprs =
                Map.insert nm (thisModuleParsed, namesBefore, prevLoadedMods) (moduleParsedExprs s) })
            -- Restore accumulated parsedModule
            let allParsed = thisModuleParsed ++ prevParsedModule
            modify (\s -> s { parsedModule = allParsed })

-- | Run Phase 2 (TC + CLM) for a module that already went through Phase 1.
-- Restores parsedModule from moduleParsedExprs, runs type checking and CLM,
-- then finalizes the module.
runModulePhase2 :: String -> IntState ()
runModulePhase2 nm = do
    st <- get
    case Map.lookup nm (moduleParsedExprs st) of
        Nothing -> return ()  -- Module not found (maybe cached or parse error)
        Just (savedParsed, namesBefore, savedPrevLoadedMods) -> do
            -- Save current parsedModule and restore this module's
            let prevParsedModule = parsedModule st
            put $ st { parsedModule = savedParsed }
            -- Load file text for finalize
            let fileText = case Map.lookup nm (loadedSources st) of
                    Just t  -> t
                    Nothing -> ""
            let exprs = Prelude.map fst savedParsed
            -- Check if all imported modules are loaded; defer TC if not
            let deps = extractDependencies exprs
                importedKeys = [intercalate "." d | d <- deps]
                loadedKeys = loadedModules (currentModuleEnv st)
                missingKeys = [k | k <- importedKeys, not (Map.member k loadedKeys)]
            if Prelude.null missingKeys
              then do
                -- All deps loaded: run Phase 2 passes (TC + CLM)
                runPhase2Passes
                -- Finalize module (visibility, cache, etc.)
                _cm <- finalizeModule nm fileText namesBefore savedPrevLoadedMods exprs
                -- Restore accumulated parsedModule
                st' <- get
                let allParsed = parsedModule st' ++ prevParsedModule
                modify (\s -> s { parsedModule = allParsed })
              else do
                -- Deps not loaded: defer TC, but still register Phase 1 completion
                -- (definitions ARE available, TC is just deferred validation)
                _cm <- finalizeModule nm fileText namesBefore savedPrevLoadedMods exprs
                modify (\s -> s { deferredTCModules =
                    (nm, savedParsed, missingKeys) : deferredTCModules s })
                -- Restore accumulated parsedModule
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
-- Uses two-phase compilation: Phase 1 (env build) for ALL modules first,
-- then Phase 2 (TC + CLM) for ALL modules. This ensures the type checker
-- sees the complete global environment including definitions from later modules.
loadModuleTree :: FilePath -> IntState ()
loadModuleTree entryFile = do
    st <- get
    let searchPaths = libSearchPaths st
    -- Ensure cache directory exists
    liftIO ensureCacheDir
    -- Load prelude first (primitives, no dependencies) — full pipeline (no cross-deps)
    compilerMsg Normal "  Loading Prelude..."
    loadFileWithCache "Prelude" preludeModulePath
    -- Parse entry file to get its dependencies
    compilerMsg Normal "  Resolving module dependencies..."
    allModules <- liftIO $ resolveAllDeps searchPaths Set.empty [entryFile]
    compilerMsg Normal $ "  Loading " ++ show (length allModules) ++ " modules..."
    -- Phase 1: All modules through env build + case optimization
    -- After this, currentEnvironment has ALL types, constructors, lambdas, instances
    compilerMsg Normal "  Phase 1: Building environments..."
    phase1Modules <- loadAllModulesPhase1 allModules
    -- Phase 2: All modules through type checking + CLM generation
    -- TC now sees the complete global environment
    compilerMsg Normal "  Phase 2: Type checking & CLM..."
    mapM_ (\(_, filePath) -> do
        runModulePhase2 filePath
        ) phase1Modules
    compilerMsg Normal "  All modules loaded."

-- | Load all modules through Phase 1 only (env build + case opt).
-- Returns the list of modules that need Phase 2 (excludes cache hits).
-- Cache hits go through full pipeline immediately since they don't need TC.
loadAllModulesPhase1 :: [(String, FilePath)] -> IntState [(String, FilePath)]
loadAllModulesPhase1 modules = do
    -- For each module: try cache first, otherwise run Phase 1 only
    phase2Needed <- mapM (\(modKey, filePath) -> do
        compilerMsg Verbose $ "    " ++ modKey
        needsPhase2 <- loadFileWithCacheOrPhase1 modKey filePath
        return (if needsPhase2 then Just (modKey, filePath) else Nothing)
        ) modules
    return [m | Just m <- phase2Needed]

-- | Try to load from cache; if cache miss, run Phase 1 only.
-- Returns True if Phase 2 is still needed (cache miss), False if fully loaded from cache.
loadFileWithCacheOrPhase1 :: String -> String -> IntState Bool
loadFileWithCacheOrPhase1 modKey filePath = do
    fileText <- liftIO (TIO.readFile filePath)
    let srcHash = hashSource fileText
    st0 <- get
    mCache <- liftIO $ loadCacheIfFresh modKey fileText (moduleSourceHashes st0)
    case mCache of
        Just mc -> do
            -- Cache hit: merge cached environment slice directly (full load)
            modify (\s -> s {
                currentEnvironment = mergeEnvironment (currentEnvironment s) (mcEnvironment mc),
                moduleSourceHashes = Map.insert modKey srcHash (moduleSourceHashes s)
            })
            st <- get
            let prevLoadedMods = loadedModules (currentModuleEnv st)
            let pubNames = allEnvNames (mcEnvironment mc)
            let menv = (currentModuleEnv st) {
                    loadedModules = Map.insert modKey
                        (ModuleEnv [] pubNames Set.empty [] [] Map.empty)
                        prevLoadedMods
                }
            put $ st { currentModuleEnv = menv }
            return False  -- No Phase 2 needed
        Nothing -> do
            -- Cache miss: run Phase 1 only
            loadFileQuietPhase1 filePath
            return True  -- Phase 2 still needed

-- | Recursively resolve all dependencies starting from a list of files.
-- Returns modules in dependency order (dependencies first).
-- Tracks both fully loaded modules AND modules currently being resolved
-- to prevent infinite recursion on circular dependencies.
resolveAllDeps :: [FilePath] -> Set.Set String -> [FilePath] -> IO [(String, FilePath)]
resolveAllDeps searchPaths loaded files = resolveAllDeps' searchPaths loaded Set.empty files

resolveAllDeps' :: [FilePath] -> Set.Set String -> Set.Set String -> [FilePath] -> IO [(String, FilePath)]
resolveAllDeps' _ _ _ [] = return []
resolveAllDeps' searchPaths loaded resolving (filePath:rest) = do
    fileText <- TIO.readFile filePath
    parseResult <- runParseOnly fileText filePath
    case parseResult of
        Left _ -> resolveAllDeps' searchPaths loaded resolving rest  -- skip unparseable files
        Right exprs -> do
            let modKey = case extractModuleKey exprs of
                    Just k  -> k
                    Nothing -> filePath  -- fallback to file path
            if Set.member modKey loaded || modKey == "Prelude" || Set.member modKey resolving
                then resolveAllDeps' searchPaths loaded resolving rest
                else do
                    -- Resolve dependencies first (mark this module as resolving to break cycles)
                    let deps = extractDependencies exprs
                    depFiles <- mapM (resolveModulePath searchPaths) deps
                    let validDeps = [fp | Just fp <- depFiles]
                    let resolving' = Set.insert modKey resolving
                    depModules <- resolveAllDeps' searchPaths loaded resolving' validDeps
                    let loaded' = Set.union loaded (Set.fromList [k | (k, _) <- depModules])
                    -- Now add this module if not yet loaded
                    if Set.member modKey loaded'
                        then resolveAllDeps' searchPaths loaded' resolving rest
                        else do
                            restModules <- resolveAllDeps' searchPaths (Set.insert modKey loaded') resolving rest
                            return $ depModules ++ [(modKey, filePath)] ++ restModules

-- | Parse a file just to extract expressions (no state effects).
-- Runs the full parser in a fresh monad stack and discards the state.
runParseOnly :: T.Text -> FilePath -> IO (Either String [Expr])
runParseOnly text filePath = do
    result <- evalStateT (evalStateT (parseWholeFile text filePath) emptyIntState) initLogState
    case result of
        Left err  -> return $ Left (show err)
        Right exs -> return $ Right exs
