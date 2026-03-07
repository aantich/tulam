# Separate Compilation Design: Interface Files & Per-Module Compilation

## 1. Motivation

tulam loads all 69+ stdlib modules from source on every startup. Each module is parsed and run through 7 pipeline passes, accumulating into a single global `Environment`. Problems:

1. **Startup cost** — re-compiling the stdlib from source every time
2. **No incremental builds** — changing one module forces everything to recompile
3. **No parallel compilation** — sequential loading due to shared mutable state
4. **Codegen blocker** — JS/.NET targets need per-module output units

### Design Principles

1. **A compiled module = a serialized Environment slice.** No new types — reuses `Environment`, `Lambda`, `CLMLam`, `CLMExpr`, `ClassMeta` directly via `Data.Binary`.

2. **Layer 3 ready.** Every design decision must be compatible with adding content-addressed definitions (DependencyDesign.md) later. Specifically: names can be replaced with hashes at any lookup boundary; `Environment` maps are keyed by `Name` today but could be keyed by `Hash` tomorrow; the `CompileEnv` abstraction provides a clean seam for swapping name-based resolution to hash-based resolution.

3. **Storage architecture.** Global content store (`~/.tulam/store/`) for immutable compiled artifacts, per-project name mappings. Same structure that DependencyDesign.md proposes. Phase 2 starts with per-project `.tulam-cache/`, Phase 3 migrates to global store.

---

## 2. Current State (Phase 2A+2B+2C+2F COMPLETE)

### 2.1 What's Built

- All core types have `Generic` + `Binary` instances (Logs.hs, Surface.hs, CLM.hs, State.hs)
- `src/Interface.hs` — `ModuleCache` type, `writeModuleCache`, `readModuleCache`, `loadCacheIfFresh`
- `loadFileQuiet` writes `.tli` after successful compilation
- `loadModuleTree` uses `loadFileWithCache` (try cache -> fallback to compile)
- `sliceEnvironment` in State.hs (inverse of `removeNames`)
- `.tulam-cache/v1/` with 73 cached modules, `.gitignore`'d
- **Phase 2A**: `parsedModule` scoped per module — each module's passes only see its own expressions
- **Phase 2C**: Dependency hash tracking — `moduleSourceHashes` in state, cache files store dep hashes, stale deps force recompile
- **Phase 2B**: `CompiledModule` type + `runModulePasses` + `finalizeModule` extracted from `loadFileQuiet`
- **Phase 2F**: REPL commands `:cache status/clear/rebuild`
- All 713 tests pass with and without cache

### 2.2 Remaining Limitation

Modules still compile against accumulated global `Environment` (not just imports' public interfaces). Cache hits skip recompilation but the compilation itself isn't fully isolated — module B's compilation can see all internals of module A that were already in the env. This blocks true parallel compilation but doesn't affect correctness.

---

## 3. Pipeline Audit: What Each Pass Needs

### 3.1 Pass Dependency Map

```
parsedModule (accumulated) ──────────────────────────┐
                                                      v
Pass 0/0.25/0.5 (desugar)  LOCAL only: transforms parsedModule AST in-place
                            Reads: parsedModule, currentFlags
                            Writes: parsedModule
                            Cross-module reads: NONE
                                                      │
                                                      v
Pass 1 (env build)          THE CRITICAL PASS
                            Reads: parsedModule (iterates ALL accumulated exprs)
                            Reads from env: types, constructors, topLambdas,
                                           instanceLambdas, structInheritance,
                                           classDecls, reprMap, effectDecls
                            Writes to env: ALL of the above + classTagCounter
                            Cross-module reads: YES — structure inheritance,
                                               class parent lookup, instance
                                               resolution, morphism composition
                                                      │
                                                      v
Pass 1.5 (record desugar)  Reads: constructors (field name -> index resolution)
                            Writes: parsedModule (transforms RecordConstruct etc.)
                            Cross-module reads: YES — looks up constructors by name
                                                      │
                                                      v
Pass 2 (case opt)           Reads: topLambdas, instanceLambdas, constructors
                            Writes: topLambdas, instanceLambdas (expanded patterns)
                            Cross-module reads: YES — constructors for pattern expansion
                                                      │
                                                      v
Pass 3 (type check)         Reads: types, constructors, topLambdas, instanceLambdas,
                                   structInheritance, classDecls
                            Writes: (warnings/errors only, no env mutation)
                            Cross-module reads: YES — type signatures of imported names
                                                      │
                                                      v
Pass 4 (CLM conversion)     Reads: types, constructors, topLambdas, instanceLambdas,
                                   reprMap, classDecls
                            Writes: clmLambdas, clmInstances
                            Cross-module reads: YES — constructor tags, instance keys
                                                      │
                                                      v
Pass 4.5 (CLM optimize)     Reads: clmLambdas (for inline-small cross-ref check)
                            Writes: clmLambdas, clmInstances (optimized)
                            Cross-module reads: MINIMAL — only inline-small size check
```

### 3.2 Interpreter Runtime Needs

The interpreter reads ONLY these `Environment` fields at runtime:

| Field | Purpose | Cross-module? |
|-------|---------|--------------|
| `clmLambdas` | Function lookup by name | Yes |
| `clmInstances` | Instance dispatch by `funcName\0type` key | Yes |
| `constructors` | Field name resolution, type inference | Yes |
| `types` | Reflection intrinsics only | Yes |
| `classDecls` | Method dispatch, class hierarchy | Yes |

**NOT used at runtime**: `topLambdas`, `instanceLambdas` (Surface AST — only CLM versions matter), `reprMap` (compile-time only), `effectDecls`/`effectHandlers` (dispatch via instances), `structInheritance`, `topBindings`.

### 3.3 The Core Problem: `parsedModule` Accumulation — RESOLVED (Phase 2A)

`buildEnvPass` (Pass 1) iterates over ALL of `parsedModule`, which used to accumulate across file loads. **Fixed:** `loadFileQuiet` now saves/clears/restores `parsedModule` around each module's compilation, so passes only see the current module's expressions.

### 3.4 `classTagCounter` — Mutable Shared State

`classTagCounter` is incremented each time a class is declared. It must be consistent across modules (two modules can't generate the same tag). Solutions:
- Pass it through as part of the `CompileEnv`
- Or: assign tag ranges per module (e.g., module hash-based offset)
- For Layer 3 compatibility: tags become part of the content hash

---

## 4. Phase 2: True Separate Compilation — Detailed Plan

### 4.1 Goal

Each module compiles using only:
1. Its own source file (`.tl`)
2. The `CompileEnv` built from cached `.tli` files of its imports
3. Shared state: `classTagCounter` (monotonic counter passed through)

After compilation, it produces:
1. A `CompiledModule` (Environment slice — exactly what we already cache)
2. Updated `classTagCounter`

### 4.2 New Type: `CompileEnv`

```haskell
-- | Read-only environment for compiling a module.
-- Built from merged .tli files of imported modules.
-- Structurally identical to Environment but semantically read-only.
type CompileEnv = Environment
```

We do NOT create a new type. `CompileEnv` is just a type alias for `Environment`. This is critical for Layer 3 compatibility — when we add content addressing, we swap how `CompileEnv` gets populated (from hash lookups instead of file loads) without changing the type.

### 4.3 New Type: `CompiledModule`

```haskell
-- | Output of compiling a single module.
data CompiledModule = CompiledModule
    { cmModuleKey     :: String
    , cmPublicEnv     :: Environment     -- public names only (what .tli stores)
    , cmFullEnv       :: Environment     -- all names including private (for linker)
    , cmClassTagNext  :: Int             -- classTagCounter after this module
    } deriving (Show, Generic)

instance Binary CompiledModule
```

Why both `cmPublicEnv` and `cmFullEnv`? The `.tli` file stores `cmPublicEnv` (what downstream modules see). The interpreter/linker needs `cmFullEnv` (private functions still execute at runtime). Currently `loadFileQuiet` handles this by removing private names from the global env but keeping them in CLM maps — we preserve that behavior.

### 4.4 Milestone A: Scope `parsedModule` Per Module

**The simplest, highest-impact change.** Currently `parsedModule` accumulates across file loads because the parser appends to it. `buildEnvPass` then iterates ALL of `parsedModule`.

**Change:** Before running passes for a module, snapshot and isolate `parsedModule`:

```haskell
loadFileQuiet nm = do
    fileText <- liftIO (TIO.readFile nm)
    res <- parseWholeFile fileText nm
    st <- get
    let savedParsedModule = parsedModule st  -- snapshot
    -- ... (existing setup) ...
    case res of
        Right exprs -> do
            -- parsedModule now contains ONLY this module's exprs
            -- (parseWholeFile already set it)
            runPasses
            -- After passes: restore accumulated parsedModule
            st' <- get
            put $ st' { parsedModule = parsedModule st' }
            -- parsedModule already has both old + new because parser appends
```

Wait — actually the parser appends to `parsedModule`. So after `parseWholeFile`, `parsedModule` has all previous modules' exprs PLUS the new file's exprs. The fix:

```haskell
loadFileQuiet nm = do
    fileText <- liftIO (TIO.readFile nm)
    st <- get
    let prevModule = parsedModule st
    -- Clear parsedModule so parser only adds THIS module's exprs
    put $ st { parsedModule = [] }
    res <- parseWholeFile fileText nm
    st2 <- get
    let thisModuleExprs = parsedModule st2  -- only this module
    -- ... run passes on thisModuleExprs ...
    -- After: restore full parsedModule for REPL/downstream
    st3 <- get
    put $ st3 { parsedModule = parsedModule st3 ++ prevModule }
```

This is the KEY change that scopes compilation to a single module. All passes that iterate `parsedModule` will now see only the current module's expressions.

**Risk:** Some passes may depend on seeing previously-loaded expressions. Audit says Pass 0/0.25/0.5 are local transforms (safe). `buildEnvPass` iterates `parsedModule` — but it also reads from `Environment` for cross-module references. If we scope `parsedModule` to one module but keep the accumulated `Environment` available for lookups, this should work. The `Environment` already contains all processed results from previous modules.

### 4.5 Milestone B: Separate `CompileEnv` from Output Env

**Change:** Each pass receives a read-only `CompileEnv` (imported names) and writes to a fresh local env (this module's names). After all passes, the local env becomes the `CompiledModule`.

```haskell
compileModule :: CompileEnv -> [(Expr, SourceInfo)] -> Int -> IntState CompiledModule
compileModule importedEnv moduleExprs tagCounter = do
    st <- get
    -- Set up: imported env as baseline, this module's exprs only
    put $ st { currentEnvironment = importedEnv
             , parsedModule = moduleExprs
             , currentFlags = (currentFlags st) { classTagCounter' = tagCounter }
             }
    -- Run all passes (they read from currentEnvironment which has imports,
    -- and WRITE new names into it)
    runAllPasses
    -- Extract what this module added
    st' <- get
    let fullEnv = currentEnvironment st'
    let newNames = allEnvNames fullEnv `Set.difference` allEnvNames importedEnv
    let publicEnv = sliceEnvironment publicNames fullEnv
    return CompiledModule { ... }
```

This is architecturally clean: the passes don't change at all. They still read/write `currentEnvironment`. The difference is what's IN `currentEnvironment` at the start — just the imported env, not the accumulated global state.

**Why this works:** Every pass already reads `currentEnvironment` for imported names (types, constructors, etc.) and writes new definitions into it. By starting from `importedEnv` instead of the accumulated global env, we get the same behavior but scoped.

### 4.6 Milestone C: Build `CompileEnv` from `.tli` Files

```haskell
loadModuleTree entryFile = do
    -- Resolve dependency order (existing)
    allModules <- resolveAllDeps ...

    -- For each module in topological order:
    compiledModules <- foldM (\compiled (modKey, filePath) -> do
        -- Build CompileEnv from this module's imports' compiled outputs
        let importedEnv = buildCompileEnv compiled (importsOf modKey)

        -- Try cache first
        cached <- tryCache modKey filePath
        case cached of
            Just cm -> return (Map.insert modKey cm compiled)
            Nothing -> do
                -- Compile from source
                cm <- compileModule importedEnv (parseFile filePath) tagCounter
                writeCache modKey cm
                return (Map.insert modKey cm compiled)
    ) Map.empty allModules

    -- Link: merge all compiled modules into global env for interpreter
    let finalEnv = linkModules compiledModules
    modify (\st -> st { currentEnvironment = finalEnv })

buildCompileEnv :: HashMap String CompiledModule -> [String] -> Environment
buildCompileEnv compiled importKeys =
    foldl' (\env key -> case Map.lookup key compiled of
        Just cm -> mergeEnvironment env (cmPublicEnv cm)
        Nothing -> env
    ) initialEnvironment importKeys

linkModules :: HashMap String CompiledModule -> Environment
linkModules = foldl' (\env cm -> mergeEnvironment env (cmFullEnv cm)) initialEnvironment
```

### 4.7 Milestone D: Dependency Hash Tracking

Currently `mcDepsHashes` is `[]`. Fix:

```haskell
-- After compiling a module, compute its cache hash including dep hashes
let depHashes = [(depKey, mcSourceHash depCache) | depKey <- importKeys
                                                  , Just depCache <- [Map.lookup depKey cacheMap]]
let mc = ModuleCache { ..., mcDepsHashes = depHashes }
```

Cache invalidation: if any dependency's `.tli` changed (different hash), this module must recompile too.

### 4.8 Milestone E: Split `.tli` (Interface) from `.tlc` (Compiled)

Currently `.tli` stores `cmPublicEnv` (Environment slice with public names). For the interpreter we also need private CLM code. Split into:

```
.tulam-cache/v1/
    Algebra.Eq.tli    -- public interface (types, signatures, public CLM)
    Algebra.Eq.tlc    -- full compiled output (all CLM including private)
```

- `.tli` = what downstream modules compile against (public `Environment` slice)
- `.tlc` = what the interpreter/linker loads (full `Environment` slice including private CLM)

This maps directly to Layer 3: `.tli` becomes the content store's name-to-hash mapping, `.tlc` becomes the compiled artifacts keyed by hash.

### 4.9 Milestone F: Parallel Compilation

Once modules compile against `.tli` files only, independent modules can compile in parallel:

```
Prelude (sequential — must be first)
    |
    +-- Algebra.Eq ──┐
    +-- Algebra.Ord ──┤  (parallel group 1)
    +-- Algebra.Semigroup ─┘
         |
         +-- Core.Bool ──┐
         +-- Core.Maybe ──┤  (parallel group 2)
         +-- Core.Ordering ─┘
```

Implementation: `Control.Concurrent.Async`. Each module gets its own `InterpreterState`. Only `classTagCounter` needs synchronization (use `IORef` or pre-assign ranges).

---

## 5. Storage Architecture

### 5.1 Phase 2: Per-Project Cache (Current)

```
project/
  .tulam-cache/
    v1/
      Algebra.Eq.tli
      Algebra.Eq.tlc
      ...
```

Simple, isolated, no cross-project concerns.

### 5.2 Phase 3 (Layer 3): Global Content Store

```
~/.tulam/
  store/
    objects/          -- content-addressed: hash -> serialized definition
      ab/cd1234...
      ef/gh5678...
    index.db          -- hash -> metadata (name hints, type sig, backends)

project/
  tulam.json          -- project manifest: name -> hash bindings
  tulam.lock          -- lock file: complete flattened hash map
  .tulam-cache/       -- per-project compiled artifacts (derived from store)
    v1/
      Algebra.Eq.tli  -- still exists but now references hashes in store
      Algebra.Eq.tlc
```

The migration path from Phase 2 to Phase 3:
1. `CompileEnv` is still just `Environment` — but built from hash-resolved definitions instead of name-resolved `.tli` files
2. `writeModuleCache` gains a step: also store individual definitions in `~/.tulam/store/`
3. `readModuleCache` gains a step: verify hashes against store before loading
4. Lock file is generated alongside `.tli` files

### 5.3 Why This Avoids Dependency Hell

**Haskell's problem (cabal hell):** Global mutable package database. Installing package B can break package A.

**Node's problem (node_modules):** Per-project copies of everything. Disk bloat, inconsistency between projects.

**tulam's approach:**
- Global store is **append-only, content-addressed** — installing B cannot break A (different content = different hash)
- Per-project `.tulam-cache/` is a **derived artifact** — can be deleted and rebuilt from store + lock file
- No version numbers, no SAT solver — lock file is just a flat list of hashes
- Multiple versions coexist naturally (different hashes)

---

## 6. Implementation Order

### Phase 2A: Scope `parsedModule` per module (Milestone A) — DONE
- `loadFileQuiet` saves/restores `parsedModule` so each module's passes only see that module's expressions
- Parser clears `parsedModule` before parsing; after all passes, appends to accumulated list
- Error path also restores `parsedModule`
- **Result:** 713/713 tests pass with and without cache

### Phase 2B: `compileModule` abstraction (Milestones B + C)
- Extract `compileModule :: CompileEnv -> [(Expr, SourceInfo)] -> IntState CompiledModule`
- Modify `loadFileWithCache` to use `compileModule`
- Build `CompileEnv` from cached `.tli` files of imports
- **Effort:** Medium (new function, refactor loadFileQuiet)
- **Risk:** Medium — must verify every pass works when `currentEnvironment` starts from import-only env

### Phase 2C: Dependency hash tracking (Milestone D) — DONE
- `moduleSourceHashes :: HashMap String Int` in `InterpreterState` tracks source hashes per module
- `loadFileQuiet` records source hash after compilation; `loadFileWithCache` records on cache hit too
- Cache writes include dep hashes computed from `extractImports` + `moduleSourceHashes`
- `loadCacheIfFresh` takes `HashMap String Int` and checks each cached dep hash against current
- Missing dep = stale (forces recompile)
- **Result:** 713/713 tests pass; 73 cached `.tli` files with proper dep hashes

### Phase 2D: Split `.tli`/`.tlc` (Milestone E)
- Separate public interface from full compiled output
- Update `loadFileWithCache` to write both, read appropriate one
- **Effort:** Small-Medium
- **Risk:** Low

### Phase 2E: Parallel compilation (Milestone F)
- Identify parallel groups from dependency graph
- `Control.Concurrent.Async` for independent modules
- Synchronize `classTagCounter` via IORef or pre-assignment
- **Effort:** Medium-High (thread safety concerns)
- **Risk:** Medium — need isolated `InterpreterState` per thread

### Phase 2F: REPL integration — DONE
- `:cache status` — shows cache dir, module count, source hash count
- `:cache clear` — deletes `.tulam-cache/`
- `:cache rebuild` — clears cache, resets source hashes, reloads all modules
- `:cache` (no args) — shows available sub-commands
- `:reload` — incremental (only changed + dependents) — FUTURE

---

## 7. Layer 3 Compatibility Checklist

Every Phase 2 decision must pass this test: "Can I swap name-based lookup for hash-based lookup without structural changes?"

| Decision | Layer 3 compatible? | Notes |
|----------|-------------------|-------|
| `CompileEnv = Environment` (type alias) | Yes | Swap how it's populated, not the type |
| `.tli` = serialized public `Environment` slice | Yes | Add hash metadata alongside |
| Per-project `.tulam-cache/` | Yes | Becomes derived from global store |
| `mergeEnvironment` for linking | Yes | Merge by hash instead of by name |
| `classTagCounter` passed through | Yes | Tags become part of content hash |
| Instance keys `"funcName\0type"` | Yes | Keys gain hash component |
| `sliceEnvironment` by name set | Yes | Slice by hash set instead |

---

## 8. Open Issues

1. **`parsedModule` scoping for REPL.** Interactive definitions don't have module boundaries. Keep current behavior: REPL accumulates into global `parsedModule` and compiles against full env. Only file-loaded modules get scoped.

2. **Structure inheritance across modules.** `resolveExtends` in Pass 1 looks up parent structure members from `Environment`. This works if the imported env contains the parent's `topLambdas`. Verify with test: algebra in module A, `extends` in module B.

3. **Morphism composition across modules.** `composeMorphismInstances` in Pass 1 walks all known instances to find transitive paths. Must see imported instances. Works if `CompileEnv` includes imported `instanceLambdas`.

4. **Record desugaring.** Pass 1.5 looks up constructors by field names. Must see imported constructors. Works if `CompileEnv` includes imported `constructors`.

5. **Intrinsic dispatch.** Intrinsics are registered in `Intrinsics.hs` and checked in the interpreter. They don't go through `Environment` — they're a separate registry. No change needed.
