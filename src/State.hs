{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances, OverloadedStrings, DeriveGeneric #-}

-- State / IO monad where we are doing all our transformations etc
-- So, our parser needs to work in this monad as well

module State where

import qualified Data.HashTable.IO as H
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class (lift)
import Data.IORef

import Data.Text as L

import Data.Sequence as S

import Surface
import CLM
import Data.HashMap.Strict as Map
import qualified Data.Set as Set
import qualified Data.List
-- import Core.Environment

import Util.IOLogger as Log
import Util.PrettyPrinting
import Logs as Logs
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.Binary (Binary)
import qualified Data.Binary as Bin
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS

type NameMap = HashMap Name
type LTProgram = [(Expr, SourceInfo)]

-- Module environment: tracks module-level information
data ModuleEnv = ModuleEnv {
    moduleName     :: ModulePath,                         -- current module's path
    publicNames    :: Set.Set Name,                       -- public (default) names
    privateNames   :: Set.Set Name,                       -- private names
    moduleImports  :: [(ModulePath, ImportSpec, Maybe Name)], -- imports
    moduleExports  :: [(ModulePath, Maybe [Name])],       -- re-exports
    loadedModules  :: HashMap String ModuleEnv            -- loaded sub-modules
} deriving Show

emptyModuleEnv :: ModuleEnv
emptyModuleEnv = ModuleEnv {
    moduleName    = [],
    publicNames   = Set.empty,
    privateNames  = Set.empty,
    moduleImports = [],
    moduleExports = [],
    loadedModules = Map.empty
}

-- | Get all names currently defined in the environment
allEnvNames :: Environment -> Set.Set Name
allEnvNames env = Set.unions
    [ Set.fromList (Map.keys (types env))
    , Set.fromList (Map.keys (constructors env))
    , Set.fromList (Map.keys (topLambdas env))
    , Set.fromList (Map.keys (instanceLambdas env))
    ]

-- | Remove names from the environment (for visibility enforcement)
removeNames :: Set.Set Name -> Environment -> Environment
removeNames names env = env
    { types = Map.filterWithKey (\k _ -> not (Set.member k names)) (types env)
    , constructors = Map.filterWithKey (\k _ -> not (Set.member k names)) (constructors env)
    , topLambdas = Map.filterWithKey (\k _ -> not (Set.member k names)) (topLambdas env)
    , topBindings = Map.filterWithKey (\k _ -> not (Set.member k names)) (topBindings env)
    , clmLambdas = Map.filterWithKey (\k _ -> not (Set.member k names)) (clmLambdas env)
    , clmBindings = Map.filterWithKey (\k _ -> not (Set.member k names)) (clmBindings env)
    }

-- | Slice environment to keep only the given names (inverse of removeNames).
-- Used for extracting a module's public environment for caching.
sliceEnvironment :: Set.Set Name -> Environment -> Environment
sliceEnvironment keepNames env = env
    { types = Map.filterWithKey (\k _ -> Set.member k keepNames) (types env)
    , constructors = Map.filterWithKey (\k _ -> Set.member k keepNames) (constructors env)
    , topLambdas = Map.filterWithKey (\k _ -> Set.member k keepNames) (topLambdas env)
    , topBindings = Map.filterWithKey (\k _ -> Set.member k keepNames) (topBindings env)
    , clmLambdas = Map.filterWithKey (\k _ -> Set.member k keepNames) (clmLambdas env)
    , clmBindings = Map.filterWithKey (\k _ -> Set.member k keepNames) (clmBindings env)
    -- Instance keys are "funcName\0type1\0..." — keep if function name part matches
    , instanceLambdas = Map.filterWithKey (\k _ -> instanceKeyMatches k keepNames) (instanceLambdas env)
    , clmInstances = Map.filterWithKey (\k _ -> instanceKeyMatches k keepNames) (clmInstances env)
    -- These are keyed by type/class/effect name
    , structInheritance = Map.filterWithKey (\k _ -> Set.member k keepNames) (structInheritance env)
    , reprMap = Map.filterWithKey (\k _ -> Set.member k keepNames) (reprMap env)
    , effectDecls = Map.filterWithKey (\k _ -> Set.member k keepNames) (effectDecls env)
    , effectHandlers = Map.filterWithKey (\k _ -> Set.member k keepNames) (effectHandlers env)
    , classDecls = Map.filterWithKey (\k _ -> Set.member k keepNames) (classDecls env)
    -- classTagCounter, targetImports, outProgram, raw* kept as-is
    }

-- | Check if an instance key (e.g. "funcName\0TypeName") matches the keep set.
-- An instance is kept if the function name or any type in the key is in the set.
instanceKeyMatches :: Name -> Set.Set Name -> Bool
instanceKeyMatches key keepNames =
    let parts = splitOn0 key
    in Prelude.any (`Set.member` keepNames) parts
  where
    splitOn0 [] = [""]
    splitOn0 s  = let (w, rest) = Prelude.break (== '\0') s
                  in w : case rest of
                           [] -> []
                           (_:rs) -> splitOn0 rs

-- | Merge one environment into another (restore hidden names)
mergeEnvironment :: Environment -> Environment -> Environment
mergeEnvironment base overlay = Environment
    { types = Map.union (types overlay) (types base)
    , constructors = Map.union (constructors overlay) (constructors base)
    , topLambdas = Map.union (topLambdas overlay) (topLambdas base)
    , topBindings = Map.union (topBindings overlay) (topBindings base)
    , clmLambdas = Map.union (clmLambdas overlay) (clmLambdas base)
    , clmBindings = Map.union (clmBindings overlay) (clmBindings base)
    , instanceLambdas = Map.union (instanceLambdas overlay) (instanceLambdas base)
    , clmInstances = Map.union (clmInstances overlay) (clmInstances base)
    , structInheritance = Map.union (structInheritance overlay) (structInheritance base)
    , reprMap = Map.union (reprMap overlay) (reprMap base)
    , effectDecls = Map.union (effectDecls overlay) (effectDecls base)
    , effectHandlers = Map.union (effectHandlers overlay) (effectHandlers base)
    , classDecls = Map.union (classDecls overlay) (classDecls base)
    , classTagCounter = max (classTagCounter overlay) (classTagCounter base)
    , targetImports = targetImports overlay ++ targetImports base
    , outProgram = Map.union (outProgram overlay) (outProgram base)
    , rawCLMLambdas = Map.union (rawCLMLambdas overlay) (rawCLMLambdas base)
    , rawCLMInstances = Map.union (rawCLMInstances overlay) (rawCLMInstances base)
    , fixityTable = Map.union (fixityTable overlay) (fixityTable base)
    }

-- Class metadata for OOP class declarations
data ClassMeta = ClassMeta {
    cmParent        :: Maybe Name,             -- parent class name
    cmAllFields     :: [Var],                  -- ALL fields: inherited then own
    cmOwnFields     :: [Var],                  -- this class's new fields only
    cmMethods       :: NameMap CLMLam,          -- ALL methods (inherited + overridden + new)
    cmStaticMethods :: NameMap CLMLam,          -- static methods
    cmFieldIndices  :: NameMap Int,             -- field name -> positional index
    cmModifier      :: ClassModifier,           -- Normal | Abstract | Sealed
    cmChildren      :: [Name],                  -- direct subclass names (for sealed checking)
    cmImplements    :: [Name],                  -- algebra names
    cmSuperArgs     :: [Expr],                  -- super constructor argument expressions
    cmExtern        :: Maybe Name,              -- target name if extern, Nothing if tulam-native
    cmTag           :: Int,                      -- unique class tag for ConsTag
    cmSourceFile    :: String                    -- file where class was declared
} deriving (Show, Eq, Generic)

-- structure keeping our current environment
data Environment = Environment {
    -- Map that keeps all our TypeReps in the current environment
    types        :: NameMap Expr,
    constructors :: NameMap (Lambda, Int),
    -- constructors are stored with their number inside the sum type
    topLambdas   :: NameMap Lambda,
    topBindings  :: NameMap Var,
    clmLambdas   :: NameMap CLMLam,
    clmBindings  :: NameMap CLMVar,
    -- instance-specialized functions: key is "funcName\0typeName"
    instanceLambdas :: NameMap Lambda,
    clmInstances    :: NameMap CLMLam,
    -- structure inheritance: maps child structure name to list of parent names
    structInheritance :: NameMap [Name],
    -- repr map: user type -> list of (reprTypeName, isDefault, toReprLam, fromReprLam, maybeInvariant)
    reprMap :: NameMap [(Name, Bool, Lambda, Lambda, Maybe Expr)],
    -- effect declarations: effect name -> (params, operations)
    effectDecls :: NameMap ([Var], [Lambda]),
    -- effect handlers: handler name -> (effect name, params, implementations)
    effectHandlers :: NameMap (Name, [Var], [Expr]),
    -- Class hierarchy
    classDecls :: NameMap ClassMeta,
    classTagCounter :: !Int,
    -- Target imports for extern class metadata resolution (codegen-time)
    targetImports :: [(ModulePath, Name)],
    outProgram   :: NameMap String,
    -- Pre-optimization CLM snapshots (for :clm-raw comparison)
    rawCLMLambdas  :: NameMap CLMLam,
    rawCLMInstances :: NameMap CLMLam,
    -- Operator fixity table: op name -> (assoc, precedence)
    fixityTable :: NameMap OperatorFixity
} deriving (Show, Generic)

-- Binary helpers for HashMap (not provided by binary package)
putHashMap :: (Binary k, Binary v) => HashMap k v -> Bin.Put
putHashMap m = Bin.put (Map.toList m)

getHashMap :: (Binary k, Binary v, Eq k, Hashable k) => Bin.Get (HashMap k v)
getHashMap = Map.fromList <$> Bin.get

-- We need Hashable for HashMap keys (String/Name) — already available via unordered-containers

instance Binary ClassMeta where
    put cm = do
        Bin.put (cmParent cm)
        Bin.put (cmAllFields cm)
        Bin.put (cmOwnFields cm)
        putHashMap (cmMethods cm)
        putHashMap (cmStaticMethods cm)
        putHashMap (cmFieldIndices cm)
        Bin.put (cmModifier cm)
        Bin.put (cmChildren cm)
        Bin.put (cmImplements cm)
        Bin.put (cmSuperArgs cm)
        Bin.put (cmExtern cm)
        Bin.put (cmTag cm)
        Bin.put (cmSourceFile cm)
    get = ClassMeta <$> Bin.get <*> Bin.get <*> Bin.get
                    <*> getHashMap <*> getHashMap <*> getHashMap
                    <*> Bin.get <*> Bin.get <*> Bin.get
                    <*> Bin.get <*> Bin.get <*> Bin.get <*> Bin.get

instance Binary Environment where
    put env = do
        putHashMap (types env)
        putHashMap (constructors env)
        putHashMap (topLambdas env)
        putHashMap (topBindings env)
        putHashMap (clmLambdas env)
        putHashMap (clmBindings env)
        putHashMap (instanceLambdas env)
        putHashMap (clmInstances env)
        putHashMap (structInheritance env)
        putHashMap (reprMap env)
        putHashMap (effectDecls env)
        putHashMap (effectHandlers env)
        putHashMap (classDecls env)
        Bin.put (classTagCounter env)
        Bin.put (targetImports env)
        putHashMap (outProgram env)
        putHashMap (rawCLMLambdas env)
        putHashMap (rawCLMInstances env)
        putHashMap (fixityTable env)
    get = Environment <$> getHashMap <*> getHashMap <*> getHashMap <*> getHashMap
                      <*> getHashMap <*> getHashMap <*> getHashMap <*> getHashMap
                      <*> getHashMap <*> getHashMap <*> getHashMap <*> getHashMap
                      <*> getHashMap <*> Bin.get <*> Bin.get <*> getHashMap
                      <*> getHashMap <*> getHashMap <*> getHashMap

initialEnvironment = Environment {
    types       = Map.empty,
    constructors = Map.empty,
    topLambdas  = Map.empty,
    clmLambdas = Map.empty,
    topBindings = Map.empty,
    clmBindings = Map.empty,
    instanceLambdas = Map.empty,
    clmInstances = Map.empty,
    structInheritance = Map.empty,
    reprMap = Map.empty,
    effectDecls = Map.empty,
    effectHandlers = Map.empty,
    classDecls = Map.empty,
    classTagCounter = 1000,
    targetImports = [],
    outProgram   = Map.empty,
    rawCLMLambdas = Map.empty,
    rawCLMInstances = Map.empty,
    fixityTable = defaultFixities
}


-- Default operator fixities (matching standard math/logic conventions)
defaultFixities :: HashMap Name OperatorFixity
defaultFixities = Map.fromList
    [ ("*",  OperatorFixity AssocLeft  7), ("/",  OperatorFixity AssocLeft  7)
    , ("*#", OperatorFixity AssocLeft  7), ("/#", OperatorFixity AssocLeft  7)
    , ("+",  OperatorFixity AssocLeft  6), ("-",  OperatorFixity AssocLeft  6)
    , ("+#", OperatorFixity AssocLeft  6), ("-#", OperatorFixity AssocLeft  6)
    , ("++", OperatorFixity AssocRight 5)
    , ("==", OperatorFixity AssocNone  4), ("!=", OperatorFixity AssocNone  4)
    , ("<",  OperatorFixity AssocNone  4), (">",  OperatorFixity AssocNone  4)
    , ("<=", OperatorFixity AssocNone  4), (">=", OperatorFixity AssocNone  4)
    , (".&.", OperatorFixity AssocRight 3), ("/\\", OperatorFixity AssocRight 3)
    , (".|.", OperatorFixity AssocRight 2), ("\\/", OperatorFixity AssocRight 2)
    ]

-- Look up fixity for an operator. Unknown operators default to infixl 9.
lookupFixity :: Name -> Environment -> OperatorFixity
lookupFixity op env = case Map.lookup op (fixityTable env) of
    Just fix -> fix
    Nothing  -> OperatorFixity AssocLeft 9

-- Add/update fixity for an operator
addFixity :: Name -> OperatorFixity -> Environment -> Environment
addFixity op fix env = env { fixityTable = Map.insert op fix (fixityTable env) }

-- scary, building a stack - stacking IO inside logger monad
-- type IntState = StateT InterpreterState IO
type IntState = StateT InterpreterState LambdaLoggerMonad

-- function to run IntState monad on top of Logger state monad
runIntState act s = evalStateT (evalStateT act s) initLogState
--type HashTable k v = H.BasicHashTable k v
--type ExpressionTable = HashTable Name Expr

data InterpreterState = InterpreterState {
    currentFlags :: CurrentFlags,
    -- this is being filled by the parser as we go, so last line in the file will be first here!
    parsedModule :: LTProgram,
    currentSource :: Text,
    currentEnvironment :: Environment,
    currentModuleEnv :: ModuleEnv,
    libSearchPaths :: [FilePath],
    -- | Source text per loaded file, for error display
    loadedSources :: HashMap FilePath Text,
    -- | Source hashes per module, for dependency hash tracking in cache
    moduleSourceHashes :: HashMap String Int,
    -- Managed mutability: IORef-backed mutable references
    refTable       :: HashMap Int (IORef CLMExpr),
    nextRefId      :: !Int,
    -- Managed mutability: mutable arrays (IORef-wrapped list for simplicity)
    mutArrayTable  :: HashMap Int (IORef [CLMExpr]),
    nextMutArrayId :: !Int,
    -- Effect handler stack: [(effectName, opName → implementation)]
    -- Head of list = innermost handler (checked first)
    handlerStack   :: [(Name, HashMap Name CLMExpr)]
}

instance Show InterpreterState where
    show st = "InterpreterState { currentFlags = " ++ show (currentFlags st)
        ++ ", parsedModule = " ++ show (Prelude.length (parsedModule st)) ++ " items"
        ++ ", refs = " ++ show (nextRefId st) ++ " allocated"
        ++ ", mutArrays = " ++ show (nextMutArrayId st) ++ " allocated"
        ++ ", handlers = " ++ show (Prelude.length (handlerStack st)) ++ " active"
        ++ " }"

emptyIntState = InterpreterState {
    currentFlags = CurrentFlags False True False False False False defaultOptFlags True True True,
    parsedModule = [],
    currentSource = "",
    currentEnvironment = initialEnvironment,
    currentModuleEnv = emptyModuleEnv,
    libSearchPaths = ["lib/"],
    loadedSources = Map.empty,
    moduleSourceHashes = Map.empty,
    refTable = Map.empty,
    nextRefId = 0,
    mutArrayTable = Map.empty,
    nextMutArrayId = 0,
    handlerStack = []
}

-- | Per-pass optimization flags
data OptFlags = OptFlags {
    optimizeEnabled   :: Bool  -- master toggle
  , optEtaReduce      :: Bool
  , optConstantFold   :: Bool
  , optKnownConstructor :: Bool
  , optDeadCodeElim   :: Bool
  , optInlineSmall    :: Bool  -- Phase 2
} deriving Show

defaultOptFlags :: OptFlags
defaultOptFlags = OptFlags True True True True True True

data CurrentFlags = CurrentFlags {
    strict    :: Bool -- true if strict, false if lazy
  , pretty    :: Bool -- pretty print or raw output
  , tracing   :: Bool -- whether to trace execution steps
  , strictTypes :: Bool -- true if type errors are fatal, false for warnings
  , verbose   :: Bool -- verbose pass logging and timing
  , newStrings :: Bool -- true if string literals desugar to fromStringLiteral
  , optSettings :: OptFlags -- optimization pass settings
  , checkPositivity   :: Bool -- positivity checking for inductive types
  , checkTermination  :: Bool -- termination checking for recursive functions
  , checkCoverage     :: Bool -- pattern match coverage checking
} deriving Show

-- Safety limits for evaluation
maxEvalIterations :: Int
maxEvalIterations = 10000

maxEvalDepth :: Int
maxEvalDepth = 1000

initializeInterpreter :: IO InterpreterState
initializeInterpreter = return $ InterpreterState {
    currentFlags = CurrentFlags False True False False False False defaultOptFlags True True True,
    parsedModule = [],
    currentSource = "",
    currentEnvironment = initialEnvironment,
    currentModuleEnv = emptyModuleEnv,
    libSearchPaths = ["lib/"],
    loadedSources = Map.empty,
    moduleSourceHashes = Map.empty,
    refTable = Map.empty,
    nextRefId = 0,
    mutArrayTable = Map.empty,
    nextMutArrayId = 0,
    handlerStack = []
}

-- | Look up a handler op by name in the handler stack (innermost first)
lookupHandlerOp :: Name -> [(Name, HashMap Name CLMExpr)] -> Maybe CLMExpr
lookupHandlerOp _ [] = Nothing
lookupHandlerOp opName ((_, ops):rest) =
    case Map.lookup opName ops of
        Just impl -> Just impl
        Nothing   -> lookupHandlerOp opName rest

-- | Apply a pure lambda transformation to both topLambdas and instanceLambdas.
-- Common pattern used by multiple passes (record desugaring, CLM conversion, etc.)
transformLambdaMaps :: (Lambda -> Lambda) -> Environment -> Environment
transformLambdaMaps f env = env
    { topLambdas = Map.map f (topLambdas env)
    , instanceLambdas = Map.map f (instanceLambdas env)
    }

------------------ Monadic traversal of the Expr tree ---------------------
-- needed for optimizations, error checking etc

{-
data Expr =
  | Binding Var -- Var contains both the name and the expression to be bound to, used inside Actions etc
  | Function Lambda -- defining a function by abstracting a bunch of variables in a tuple
  | Action Lambda -- Action is simply a list of expressions in order
  | Constructors [Lambda] -- only for constructor list inside sum types
  | CaseOf Record Expr SourceInfo
  | SumType Lambda -- sum type definition, which is also basically a lambda with 
  -- body expression being a tuple of Lambdas which are constructors
  | UnaryOp Name Expr
  | BinaryOp Name Expr Expr
  | U Int -- Universe hierarchy
  | Instance Name [Expr] [Expr] -- instance declarations
-}

traverseExprM :: (Expr -> IntState Expr) -> Expr -> IntState Expr
traverseExprM f UNDEFINED = pure UNDEFINED
traverseExprM f (Typed e1 e2) = Typed <$> (f e1) <*> (f e2)
traverseExprM f (App e exs) = App <$> (f e) <*> (mapM f exs)
traverseExprM f (PatternMatches exs) = PatternMatches <$> (mapM f exs)
traverseExprM f (NTuple fields) = NTuple <$> mapM (\(mn,e) -> (,) mn <$> f e) fields
traverseExprM f (DeclBlock exs) = DeclBlock <$> (mapM f exs)
traverseExprM f (Statements exs) = Statements <$> (mapM f exs)
traverseExprM f (UnaryOp nm e) = UnaryOp <$> (pure nm) <*> (f e)
traverseExprM _ (Primitive lam) = pure (Primitive lam)
traverseExprM _ Intrinsic = pure Intrinsic
traverseExprM _ Derive = pure Derive
traverseExprM f (Repr nm tp def fns inv) = Repr nm <$> f tp <*> pure def <*> mapM f fns <*> traverse f inv
traverseExprM f (ReprCast e tp) = ReprCast <$> f e <*> f tp
traverseExprM f (PatternGuard pc ex) = PatternGuard pc <$> f ex
-- Module system nodes
traverseExprM _ e@(ModuleDecl _) = pure e
traverseExprM _ e@(Import _ _ _) = pure e
traverseExprM _ e@(Open _) = pure e
traverseExprM _ e@(Export _ _) = pure e
traverseExprM f (PrivateDecl e) = PrivateDecl <$> f e
traverseExprM _ e@(OpaqueTy _ _) = pure e
traverseExprM _ e@(TargetBlock _ _) = pure e
traverseExprM _ e@(TargetSwitch _) = pure e
traverseExprM f (ArrayLit exs) = ArrayLit <$> mapM f exs
-- Record system nodes
traverseExprM f (RecFieldAccess ac e) = RecFieldAccess ac <$> f e
traverseExprM f (RecordType fields isOpen) = (\fs -> RecordType fs isOpen) <$> mapM (\(n,e) -> (,) n <$> f e) fields
traverseExprM f (RecordConstruct nm fields) = RecordConstruct nm <$> mapM (\(n,e) -> (,) n <$> f e) fields
traverseExprM f (RecordUpdate e fields) = RecordUpdate <$> f e <*> mapM (\(n,ex) -> (,) n <$> f ex) fields
traverseExprM f (RecordPattern nm fields) = RecordPattern nm <$> mapM (\(n,e) -> (,) n <$> f e) fields
-- Fixity declarations (leaf node)
traverseExprM _ e@(FixityDecl _ _ _) = pure e
-- Effect system nodes
traverseExprM _ e@(EffectDecl _ _ _) = pure e
traverseExprM _ e@(HandlerDecl _ _ _ _) = pure e
traverseExprM f (HandleWith e h) = HandleWith <$> f e <*> f h
traverseExprM f (ActionBlock stmts) = ActionBlock <$> mapM (traverseActionStmtM f) stmts
traverseExprM f (EffType row res) = EffType <$> f row <*> f res
-- Remaining expressions
traverseExprM f (BinaryOp nm e1 e2) = BinaryOp nm <$> f e1 <*> f e2
traverseExprM f (ConTuple ct exs) = ConTuple ct <$> mapM f exs
traverseExprM f (ExpandedCase exs e si) = ExpandedCase <$> mapM f exs <*> f e <*> pure si
traverseExprM f (CaseOf recs e si) = CaseOf recs <$> f e <*> pure si
traverseExprM f (Function lam) = do
    b <- f (body lam)
    pure $ Function (lam { body = b })
traverseExprM f (Action lam) = do
    b <- f (body lam)
    pure $ Action (lam { body = b })
traverseExprM f (SumType lam) = do
    b <- f (body lam)
    pure $ SumType (lam { body = b })
traverseExprM f (Structure lam si) = do
    b <- f (body lam)
    pure $ Structure (lam { body = b }) si
traverseExprM f (Binding (Var nm tp val)) = Binding <$> (Var nm <$> f tp <*> f val)
traverseExprM f (Instance nm targs impls reqs) = Instance nm <$> mapM f targs <*> mapM f impls <*> mapM f reqs
traverseExprM f (IfThenElse c t e) = IfThenElse <$> f c <*> f t <*> f e
traverseExprM f (LetIn binds bdy) = LetIn <$> mapM (\(v,ex) -> (,) v <$> f ex) binds <*> f bdy
traverseExprM f (Pi mn e1 e2) = Pi mn <$> f e1 <*> f e2
traverseExprM f (Implicit e) = Implicit <$> f e
traverseExprM f (Value v ex) = Value v <$> f ex
traverseExprM f (Constructors lams) = Constructors <$> mapM (\l -> do { b <- f (body l); pure (l { body = b }) }) lams
traverseExprM f e = f e

traverseActionStmtM :: (Expr -> IntState Expr) -> ActionStmt -> IntState ActionStmt
traverseActionStmtM f (ActionBind nm e) = ActionBind nm <$> f e
traverseActionStmtM f (ActionLet nm e) = ActionLet nm <$> f e
traverseActionStmtM f (ActionExpr e) = ActionExpr <$> f e

---------------------------- BASIC FUNCTIONS -----------------------------
lookupLambda :: Name -> Environment -> Maybe Lambda
lookupLambda n env = Map.lookup n (topLambdas env)

lookupConstructor :: Name -> Environment -> Maybe (Lambda, Int)
lookupConstructor n env = Map.lookup n (constructors env)

lookupType :: Name -> Environment -> Maybe Expr
lookupType n env = Map.lookup n (types env)

addLambda :: Name -> Lambda -> Environment -> Environment
addLambda n l env = env { topLambdas = Map.insert n l (topLambdas env) }

addCLMLambda :: Name -> CLMLam -> Environment -> Environment
addCLMLambda n l env = env { clmLambdas = Map.insert n l (clmLambdas env) }

addNamedSumType :: Expr -> Environment -> Environment
addNamedSumType tp@(SumType lam) env = env { types = Map.insert (lamName lam) tp (types env) } 
addNamedSumType e env = env

addNamedStructure :: Expr -> Environment -> Environment
addNamedStructure st@(Structure lam _si) env = env { types = Map.insert (lamName lam) st (types env) }
addNamedStructure e env = env

-- Register structure inheritance: child extends parents
registerInheritance :: Name -> [Name] -> Environment -> Environment
registerInheritance child parents env =
    env { structInheritance = Map.insert child parents (structInheritance env) }

-- Get all transitive parents of a structure
getAllParents :: Name -> Environment -> [Name]
getAllParents name env = go [name] []
  where
    go [] visited = visited
    go (n:ns) visited
      | n `Prelude.elem` visited = go ns visited
      | otherwise = case Map.lookup n (structInheritance env) of
          Nothing      -> go ns (visited ++ [n])
          Just parents -> go (parents ++ ns) (visited ++ [n])

-- Class hierarchy helpers
lookupClass :: Name -> Environment -> Maybe ClassMeta
lookupClass nm env = Map.lookup nm (classDecls env)

isSubclassOf :: Name -> Name -> Environment -> Bool
isSubclassOf className targetName env
    | className == targetName = True
    | otherwise = case lookupClass className env of
        Just cm -> case cmParent cm of
            Just parent -> isSubclassOf parent targetName env
            Nothing -> False
        Nothing -> False

lookupParentMethod :: Name -> Name -> Environment -> Maybe CLMLam
lookupParentMethod className methodName env =
    case lookupClass className env of
        Just cm -> case cmParent cm of
            Just parentName -> case lookupClass parentName env of
                Just parentMeta -> Map.lookup methodName (cmMethods parentMeta)
                Nothing -> Nothing
            Nothing -> Nothing
        Nothing -> Nothing

isSealedClass :: Name -> Environment -> Bool
isSealedClass nm env = case lookupClass nm env of
    Just cm -> cmModifier cm == ClassSealed
    Nothing -> False

-- Get all transitive subclasses of a class
getAllSubclasses :: Name -> Environment -> [Name]
getAllSubclasses nm env = go [nm] []
  where
    go [] visited = visited
    go (n:ns) visited
        | n `Prelude.elem` visited = go ns visited
        | otherwise = case lookupClass n env of
            Just cm -> go (cmChildren cm ++ ns) (visited ++ [n])
            Nothing -> go ns (visited ++ [n])

addNamedLambda :: Lambda -> Environment -> Environment
addNamedLambda l env = env { topLambdas = Map.insert (lamName l) l (topLambdas env) }

addManyNamedLambdas :: [Lambda] -> Environment -> Environment
addManyNamedLambdas ls env = env { topLambdas = Prelude.foldl (\acc l1 -> Map.insert (lamName l1) l1 acc) (topLambdas env) ls }

addNamedConstructor :: Int -> Lambda -> Environment -> Environment
addNamedConstructor i l env = env { constructors = Map.insert (lamName l) (l,i) (constructors env) }

addManyNamedConstructors :: Int -> [Lambda] -> Environment -> Environment
addManyNamedConstructors i []     env = env
addManyNamedConstructors i (c:cs) env = addManyNamedConstructors (i+1) cs (addNamedConstructor i c env)

-- addManyNamedConstructors :: [Lambda] -> Environment -> Environment
-- addManyNamedConstructors ls env = env { constructors = Prelude.foldl (\acc l1 -> Map.insert (lamName l1) l1 acc) (topLambdas env) ls }

-- Instance functions: keyed by "funcName\0type1\0type2\0..."
-- For single-param: mkInstanceKey "==" ["Nat"] → "==\0Nat" (backward compatible)
mkInstanceKey :: Name -> [Name] -> Name
mkInstanceKey funcName typeNames = funcName ++ "\0" ++ Data.List.intercalate "\0" typeNames

addInstanceLambda :: Name -> [Name] -> Lambda -> Environment -> Environment
addInstanceLambda funcNm typeNms lam env =
    env { instanceLambdas = Map.insert (mkInstanceKey funcNm typeNms) lam (instanceLambdas env) }

lookupInstanceLambda :: Name -> [Name] -> Environment -> Maybe Lambda
lookupInstanceLambda funcNm typeNms env = Map.lookup (mkInstanceKey funcNm typeNms) (instanceLambdas env)

addCLMInstance :: Name -> [Name] -> CLMLam -> Environment -> Environment
addCLMInstance funcNm typeNms clm env =
    env { clmInstances = Map.insert (mkInstanceKey funcNm typeNms) clm (clmInstances env) }

lookupCLMInstance :: Name -> [Name] -> Environment -> Maybe CLMLam
lookupCLMInstance funcNm typeNms env = Map.lookup (mkInstanceKey funcNm typeNms) (clmInstances env)

-- Reverse lookup: given a constructor name, find which type it belongs to
lookupTypeOfConstructor :: Name -> Environment -> Maybe Name
lookupTypeOfConstructor consName env =
    case Map.lookup consName (constructors env) of
        Just (lam, _) -> case lamType lam of
            Id nm -> Just nm
            App (Id nm) _ -> Just nm  -- parameterized types like List(a)
            _     -> Nothing
        Nothing -> Nothing

-- Prefix-based CLM instance lookup: find instances where key starts with funcName\0type1\0type2...
-- Used for morphism dispatch where we know arg types but not return type.
-- When multiple matches exist, prefer non-composed instances (those whose body
-- does NOT re-dispatch via CLMIAP) to avoid infinite recursion from composed
-- instances that call the same function.
lookupCLMInstancePrefix :: Name -> [Name] -> Environment -> Maybe CLMLam
lookupCLMInstancePrefix funcNm typeNms env =
    let prefix = mkInstanceKey funcNm typeNms ++ "\0"
        -- try exact match first, then prefix
        exact = Map.lookup (mkInstanceKey funcNm typeNms) (clmInstances env)
    in case exact of
        Just _ -> exact
        Nothing ->
            let matches = Map.toList $ Map.filterWithKey
                    (\k _ -> Prelude.take (Prelude.length prefix) k == prefix) (clmInstances env)
                -- Partition: direct instances (no CLMIAP in body) vs composed
                (direct, composed) = Data.List.partition (isDirect . snd) matches
            in case direct of
                ((_, v):_) -> Just v
                [] -> case composed of
                    ((_, v):_) -> Just v
                    [] -> Nothing
  where
    isDirect (CLMLam _ (CLMIAP _ _)) = False
    isDirect (CLMLam _ (CLMAPP _ _)) = False
    isDirect (CLMLamCases _ bodies) = not (Prelude.any isRedispatch bodies)
    isDirect _ = True
    isRedispatch (CLMIAP _ _) = True
    isRedispatch (CLMAPP (CLMIAP _ _) _) = True
    isRedispatch _ = False

-- Find any CLM instance for a given function name (for nullary dispatch)
findAnyInstance :: Name -> Environment -> Maybe CLMLam
findAnyInstance funcNm env =
    let prefix = funcNm ++ "\0"
        matches = Map.filterWithKey (\k _ -> Prelude.take (Prelude.length prefix) k == prefix) (clmInstances env)
    in case Map.elems matches of
        (x:_) -> Just x
        []    -> Nothing

-- Find any CLM instance and return its type name (for nullary intrinsic dispatch)
findAnyInstanceWithType :: Name -> Environment -> Maybe (Name, CLMLam)
findAnyInstanceWithType funcNm env =
    let prefix = funcNm ++ "\0"
        matches = Map.filterWithKey (\k _ -> Prelude.take (Prelude.length prefix) k == prefix) (clmInstances env)
    in case Map.toList matches of
        ((k, v):_) -> Just (Prelude.drop (Prelude.length prefix) k, v)
        []         -> Nothing

-- Find all instances of a morphism's function: given function name prefix "funcName\0",
-- return all (type1, type2) pairs for 2-param morphisms
findMorphismInstances :: Name -> Environment -> [(Name, Name)]
findMorphismInstances funcNm env =
    let prefix = funcNm ++ "\0"
        keys = Map.keys (instanceLambdas env)
        matching = Prelude.filter (\k -> Prelude.take (Prelude.length prefix) k == prefix) keys
    in Prelude.concatMap parseKey matching
  where
    parseKey k = case Prelude.break (== '\0') (Prelude.drop (Prelude.length funcNm + 1) k) of
        (t1, '\0':t2) | not (Prelude.null t1) && not (Prelude.null t2) && '\0' `Prelude.notElem` t2
            -> [(t1, t2)]
        _   -> []

-- Repr map helpers

-- | Build a repr map key from a user type expression.
-- Simple types: Id "Nat" → "Nat"
-- Parameterized types: App (Id "Array") [Id "Byte"] → "Array\0Byte"
mkReprKey :: Expr -> Name
mkReprKey (Id n) = n
mkReprKey (App (Id n) args) = Data.List.intercalate "\0" (n : Prelude.map exprToKeyPart args)
  where
    exprToKeyPart (Id nm) = nm
    exprToKeyPart (App (Id nm) as) = nm ++ "(" ++ Data.List.intercalate "," (Prelude.map exprToKeyPart as) ++ ")"
    exprToKeyPart _ = "_"
mkReprKey _ = "_unknown_"

addRepr :: Name -> Name -> Bool -> Lambda -> Lambda -> Maybe Expr -> Environment -> Environment
addRepr userType reprType isDefault toR fromR inv env =
    let existing = maybe [] id (Map.lookup userType (reprMap env))
        updated = (reprType, isDefault, toR, fromR, inv) : existing
    in env { reprMap = Map.insert userType updated (reprMap env) }

lookupRepr :: Name -> Environment -> [(Name, Bool, Lambda, Lambda, Maybe Expr)]
lookupRepr nm env = maybe [] id (Map.lookup nm (reprMap env))

getDefaultRepr :: Name -> Environment -> Maybe (Name, Lambda, Lambda)
getDefaultRepr nm env = case lookupRepr nm env of
    [] -> Nothing
    xs -> case Prelude.filter (\(_, def, _, _, _) -> def) xs of
        ((rn, _, toR, fromR, _):_) -> Just (rn, toR, fromR)
        [] -> case xs of  -- if no default, use first
            ((rn, _, toR, fromR, _):_) -> Just (rn, toR, fromR)
            [] -> Nothing

lookupReprPair :: Name -> Name -> Environment -> Maybe (Lambda, Lambda)
lookupReprPair userType reprType env = case lookupRepr userType env of
    [] -> Nothing
    xs -> case Prelude.filter (\(rn, _, _, _, _) -> rn == reprType) xs of
        ((_, _, toR, fromR, _):_) -> Just (toR, fromR)
        [] -> Nothing

-- Check if a type name is a repr target (appears as reprType in some repr declaration)
isReprTarget :: Name -> Environment -> Bool
isReprTarget nm env = Prelude.any (Prelude.any (\(rn, _, _, _, _) -> rn == nm)) (Map.elems (reprMap env))

addManyLambdas :: [(Name, Lambda)] -> Environment -> Environment
addManyLambdas ls env = env { topLambdas = Prelude.foldl (\acc (n1,l1) -> Map.insert n1 l1 acc) (topLambdas env) ls }


------------------ Monadic interface to the Environment ---------------------
-- lookupLambdaM :: Name -> IntState (Maybe Lambda)
-- lookupLambdaM n = get >>= pure . currentEnvironment >>= pure . (lookupLambda n)

-- outputs a message only if tracing is on
trace :: String -> IntState ()
trace msg = do
    tr <- currentFlags <$> get >>= pure . tracing
    if tr then liftIO (putStrLn msg) else pure ()

-- outputs a message only if verbose is on
verboseLog :: String -> IntState ()
verboseLog msg = do
    v <- currentFlags <$> get >>= pure . verbose
    if v then liftIO (putStrLn msg) else pure ()

-- lifted versions of the IOLogger monad functions
logError :: LogPayload -> IntState ()
logError    = lift . Log.logError
logWarning :: LogPayload -> IntState ()
logWarning  = lift . Log.logWarning
logInfo :: LogPayload -> IntState ()
logInfo     = lift . Log.logInfo
logVerbose :: LogPayload -> IntState ()
logVerbose  = lift . Log.logVerbose
logDebug :: LogPayload -> IntState ()
logDebug    = lift . Log.logDebug
logTrace :: LogPayload -> IntState ()
logTrace    = lift . Log.logTrace

showAllLogsWSource :: IntState ()
showAllLogsWSource = do
    st <- get
    let src = currentSource st
    let srcMap = loadedSources st
    lift (Logs.showAllLogsWSourceMap srcMap src)

clearAllLogs :: IntState ()
clearAllLogs = lift Log.clearAllLogs    

getAllLogs :: IntState (Seq (LogMessage LogPayload))
getAllLogs = lift Log.getAllLogs

showAllLogs :: IntState ()
showAllLogs = State.getAllLogs >>= \logs -> liftIO (mapM_ (putStrLn . ppr) logs)
