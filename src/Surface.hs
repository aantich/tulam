{-# LANGUAGE OverloadedStrings, NamedFieldPuns, GADTs, PatternSynonyms, ViewPatterns, DeriveGeneric #-}

-- This is our PRIMARY CORE LANGUAGE, based on HoTT with some changes and extensions, notably -
-- we are using N-tuples explicitly, but there's more.

module Surface
where

import Util.PrettyPrinting
import Logs
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics (Generic)
import Data.Binary (Binary)

type Name = String
type ModulePath = [Name]  -- e.g. ["Algebra", "Ring"]

data Var = Var {
    name :: Name,
    typ :: Expr,
    val :: Expr
} deriving (Show, Eq, Generic)

instance Binary Var

-- type Tuple a = [a]
type Record = [Var]

-- Classification of structures (typeclasses)
data StructKind = SGeneral | SAlgebra | SMorphism deriving (Show, Eq, Generic)
instance Binary StructKind

-- Classification for class modifiers
data ClassModifier = ClassNormal | ClassAbstract | ClassSealed deriving (Show, Eq, Generic)
instance Binary ClassModifier

-- Method modifier (for methods inside class bodies)
data MethodModifier = MNone | MOverride | MFinal | MStatic deriving (Show, Eq, Generic)
instance Binary MethodModifier

-- Extended structure metadata
data StructInfo = StructInfo {
    structKind      :: StructKind,
    structExtends   :: [Expr],
    structRequires  :: [Expr],
    structMandatory :: [Name],
    structDerive    :: [Expr]       -- derive { } block contents (functions for auto-deriving)
} deriving (Show, Eq, Generic)

instance Binary StructInfo

defaultStructInfo :: StructInfo
defaultStructInfo = StructInfo SGeneral [] [] [] []

-- Class metadata (for OOP class declarations)
data ClassInfo = ClassInfo {
    classParent      :: Maybe (Name, [Expr]),  -- parent name + super constructor args
    classImplements  :: [Expr],                -- algebra names: Eq, Show, etc.
    classModifier    :: ClassModifier,
    classExtern      :: Maybe Name,            -- Just "dotnet" | Just "js" | Just "native" | Nothing
    classMethodMods  :: [(Name, MethodModifier)]  -- per-method modifiers
} deriving (Show, Eq, Generic)

instance Binary ClassInfo

defaultClassInfo :: ClassInfo
defaultClassInfo = ClassInfo Nothing [] ClassNormal Nothing []
-- | Universe levels for level polymorphism.
-- LConst n = concrete level (0, 1, 2, ...)
-- LVar name = level variable (for level-polymorphic functions)
-- LSucc l = successor level (l + 1)
-- LMax l1 l2 = max of two levels (needed for universe-polymorphic functions)
data Level
  = LConst !Int
  | LVar Name
  | LSucc Level
  | LMax Level Level
  deriving (Show, Eq, Generic)

instance Binary Level

-- | Smart constructors and helpers for Level
levelZero :: Level
levelZero = LConst 0

levelOne :: Level
levelOne = LConst 1

levelSucc :: Level -> Level
levelSucc (LConst n) = LConst (n + 1)
levelSucc l = LSucc l

levelMax :: Level -> Level -> Level
levelMax (LConst a) (LConst b) = LConst (max a b)
levelMax a b = LMax a b

-- | Check if a level is concretely less than or equal to another
levelLeq :: Level -> Level -> Maybe Bool
levelLeq (LConst a) (LConst b) = Just (a <= b)
levelLeq _ _ = Nothing  -- can't decide with variables

-- | Check if a level is concretely equal to another
levelEq :: Level -> Level -> Maybe Bool
levelEq (LConst a) (LConst b) = Just (a == b)
levelEq (LVar a) (LVar b) = Just (a == b)
levelEq _ _ = Nothing

-- | Pretty-print a Level
showLevel :: Level -> String
showLevel (LConst 0) = "0"
showLevel (LConst n) = show n
showLevel (LVar name) = name
showLevel (LSucc l) = showLevel l ++ "+1"
showLevel (LMax l1 l2) = "max(" ++ showLevel l1 ++ ", " ++ showLevel l2 ++ ")"

-- | Substitute a level variable
substLevel :: Name -> Level -> Level -> Level
substLevel n r (LVar m) | n == m = r
substLevel n r (LSucc l) = levelSucc (substLevel n r l)
substLevel n r (LMax l1 l2) = levelMax (substLevel n r l1) (substLevel n r l2)
substLevel _ _ l = l

-- constructor tag placeholder type
data ConsTag = ConsTag Name !Int deriving (Show, Eq, Generic)
instance Binary ConsTag

-- this is "naive" arity as it does not take into account functions
-- that return other functions as a result
arity :: Lambda -> Int
arity lam = length (params lam)

-- Import specification for module imports
data ImportSpec = ImportAll | ImportOnly [Name] | ImportHiding [Name] | ImportAs Name
    deriving (Show, Eq, Generic)
instance Binary ImportSpec

-- Visibility modifier for declarations
data Visibility = Public | Private deriving (Show, Eq, Generic)
instance Binary Visibility

-- Action statement types for do-notation in action blocks
data ActionStmt
  = ActionBind Name Expr      -- name <- expr (monadic bind)
  | ActionLet Name Expr       -- name = expr (let binding)
  | ActionExpr Expr           -- expr (execute for side effect)
  deriving (Show, Eq, Generic)
instance Binary ActionStmt

-- Operator associativity for fixity declarations
data Assoc = AssocLeft | AssocRight | AssocNone deriving (Show, Eq, Generic)
instance Binary Assoc

-- Operator fixity: associativity + precedence level (0-9)
data OperatorFixity = OperatorFixity { fixAssoc :: Assoc, fixPrec :: Int } deriving (Show, Eq, Generic)
instance Binary OperatorFixity

-- | Pattern check for expanded case guards (mirrors CLMPatternCheck at CLM level)
data PatternCheck = PCheckTag ConsTag | PCheckLit Literal
  deriving (Show, Eq, Generic)
instance Binary PatternCheck

data Literal = LInt !Int | LFloat !Double | LChar !Char | LString !String | LList [Expr] | LVec [Expr] | LNTuple [(Maybe Name, Expr)]
  -- Fixed-width signed integers
  | LInt8 !Int8 | LInt16 !Int16 | LInt32 !Int32 | LInt64 !Int64
  -- Unsigned integers
  | LWord8 !Word8 | LWord16 !Word16 | LWord32 !Word32 | LWord64 !Word64
  -- Single-precision float
  | LFloat32 !Float
  deriving (Eq, Show, Generic)
instance Binary Literal

-- | Canonical mapping from a Literal to its type name.
-- Single source of truth — used by CLM, Interpreter, reflection intrinsics.
litTypeName :: Literal -> Maybe Name
litTypeName (LInt _)     = Just "Int"
litTypeName (LFloat _)   = Just "Float64"
litTypeName (LString _)  = Just "String"
litTypeName (LChar _)    = Just "Char"
litTypeName (LInt8 _)    = Just "Int8"
litTypeName (LInt16 _)   = Just "Int16"
litTypeName (LInt32 _)   = Just "Int32"
litTypeName (LInt64 _)   = Just "Int64"
litTypeName (LWord8 _)   = Just "UInt8"
litTypeName (LWord16 _)  = Just "UInt16"
litTypeName (LWord32 _)  = Just "UInt32"
litTypeName (LWord64 _)  = Just "UInt64"
litTypeName (LFloat32 _) = Just "Float32"
litTypeName _            = Nothing

-- Lambda - represents EVERYTHING pretty much (see README in HoTT folder). Its type signature is
-- also obvious from the definition, so it encodes Pi types already!
data Lambda = Lambda {
    lamName    :: Name
  , params     :: Record -- (x1:t1 = v1, ..., xn:tn = vn) - supporting default values
  , body       :: Expr -- whatever our lambda is bound to
  , lamType    :: Expr -- return type of the function, used in type checking:
   -- Type for type constructors, Sigma for typeclasses etc, specific type for type constructors and functions etc
  , lamRequires :: [Expr] -- per-function constraints: requires Monoid(a), Ord(b)
   -- Stored as [App (Id "Monoid") [Id "a"], App (Id "Ord") [Id "b"]]
   -- Type checker substitutes concrete types and emits CStructure constraints at call site
  , lamSrcInfo :: SourceInfo -- source location of this lambda definition
} deriving (Show, Eq, Generic)
instance Binary Lambda

-- | Convenience constructor for Lambda with no source info (SourceInteractive).
-- Use this in compiler-generated lambdas (desugaring, passes, etc.)
mkLambda :: Name -> Record -> Expr -> Expr -> Lambda
mkLambda n p b t = Lambda n p b t [] SourceInteractive

-- lambda is only a constructor if its body is a tuple and it has a clear
-- return type!
isLambdaConstructor :: Lambda -> Bool
isLambdaConstructor lam = case body lam of
    NTuple _ -> lamType lam /= UNDEFINED
    _ -> False

data Expr =
    UNDEFINED -- used basically instead of Nothing in default values etc
  | Id Name
  | Lit Literal
  | Typed Expr Expr -- expression with type
  | Binding Var -- Var contains both the name and the expression to be bound to, used inside Actions etc
  | Function Lambda -- defining a function by abstracting a bunch of variables in a tuple
  | Action Lambda -- Action is simply a list of expressions in order
  | Constructors [Lambda] -- only for constructor list inside sum types
  | Structure Lambda StructInfo -- storing type classes / type families etc.
  -- the body of lambda is a list of functions / items that are part of the
  -- structure
  -- StructInfo carries classification (algebra/morphism/general), extends, requires, mandatory names
  | App Expr [Expr] -- application
  | PatternGuard PatternCheck Expr -- pattern check: does Expr match a constructor tag or literal?
  | RecFieldAccess (Name,Int) Expr -- access a field of the Expr by name or index
  | CaseOf Record Expr SourceInfo -- in the course of optimizations we transfer pattern matches
  -- to the case x of val -> expr statements. However, since we prefer lists vs trees
  -- in our implementation, we are actually combining function variables with
  -- the expressions they have to match in the Record, e.g.:
  -- f(x:t1,y:t2) = { {a,b} -> expr } turns into:
  -- CaseOf [Var "x" t1 a, Var "y" t2 b] expr!
  -- this way we can try if else, case of etc approaches depending on the 
  -- compilation target
  
  | ExpandedCase [Expr] Expr SourceInfo -- this is what CaseOf gets converted
  -- into in the course of optimizations and expansions - first part 
  -- is simply a list of comparisons of Expr to specific ConsTag or a value eventually,
  -- ALL of them need to be True for the case to work.
  
  | PatternMatches [Expr] -- only CaseOf or ExpandedCase is allowed inside this!!!
  | NTuple [(Maybe Name, Expr)] -- unified tuple/record: {1,2} = positional, {x=1,y=2} = named
  | ConTuple ConsTag [Expr] -- only CONSTRUCTORS return it - tagged by the constructor tag!
  | DeclBlock [Expr] -- declaration block: list of members in structures/instances (NOT a value-level tuple)
  | Statements [Expr] -- for Action body, simply a list of statements to execute in order
  | SumType Lambda -- sum type definition, which is also basically a lambda with 
  -- body expression being a tuple of Lambdas which are constructors
  | UnaryOp Name Expr
  | BinaryOp Name Expr Expr
  | U Level -- Universe hierarchy: U (LConst 0) = Type, U (LConst 1) = Kind, etc.
  -- Prim/PrimCall removed: use Function + Intrinsic instead
  | Meta !Int -- Unification metavariable (created during type checking)
  | RowEmpty -- Closed row (no more fields)
  | RowExtend Name Expr Expr -- Row field extension: {name:Type | rest}
  | Sigma (Maybe Name) Expr Expr -- Dependent pair type: (x:A) * B(x)
  | Implicit Expr -- current solution for implicit parameter functions
  -- that result e.g. from structure (typeclass) expansions -
  -- only used in the TYPE place!!!
  | Instance Name (Maybe Name) [Expr] [Expr] [Expr] -- Instance structureName maybeTag [typeArgs] [function implementations] [requires]
  | NamedRef Expr Name -- Tagged struct reference: Semigroup(a) as Additive
  | IfThenElse Expr Expr Expr -- if cond then e1 else e2 (desugared in afterparse)
  | LetIn [(Var, Expr)] Expr -- let x = e1, y = e2 in body (desugared in afterparse)
  | Law Lambda Expr -- law declaration: law name(params) = lawBody
  | PropEq Expr Expr -- propositional equality: lhs === rhs
  | Implies Expr Expr -- implication: premise ==> conclusion
  | Pi (Maybe Name) Expr Expr -- Pi type: Pi Nothing A B = A -> B, Pi (Just "x") A B = (x:A) -> B(x)
  | Value Var Expr -- value declaration: Var carries name+type, Expr is body (UNDEFINED for abstract)
  | Primitive Lambda -- primitive type declaration: primitive Int;
  | Intrinsic        -- intrinsic body marker for functions/instances
  | Derive           -- derive marker for auto-derived instances
  | Repr Expr Expr Bool [Expr] (Maybe Expr)
    -- Repr userTypeExpr reprType isDefault [toRepr, fromRepr] maybeInvariant
    -- userTypeExpr is Id "Name" for simple types, App (Id "Name") [args] for parameterized
  | ReprCast Expr Expr
    -- ReprCast exprToCast targetType (desugars to toRepr/fromRepr call)
  | RecordType [(Name, Expr)] Bool  -- Record type: {x:Int, y:Bool}, Bool = isOpen (has ..)
  -- Module system nodes
  | ModuleDecl ModulePath               -- module Algebra.Ring;
  | Import ModulePath ImportSpec (Maybe Name)  -- import with optional target (dotnet/js/native)
  | Open ModulePath                     -- open Algebra.Ring;
  | Export ModulePath (Maybe [Name])    -- export Algebra.Ring; or export Algebra.Ring (Semiring);
  | PrivateDecl Expr                    -- private function/type/etc.
  | OpaqueTy Lambda Expr                -- opaque type Name = Type;
  | TargetBlock Name [Expr]             -- target dotnet { ... };
  | TargetSwitch [(Name, Expr)]         -- target | dotnet -> e | js -> e
  -- Array node
  | ArrayLit [Expr]                     -- [e1, e2, ...] array literal
  -- Record system completion nodes
  | RecordConstruct Name [(Name, Expr)] -- Named construction: Point { x = 1, y = 2 }
  | RecordUpdate Expr [(Name, Expr)]    -- Record update: p { x = 5 }
  | RecordPattern Name [(Name, Expr)]   -- Named field pattern: Point { x = Z, y = _ }
  -- Effect system nodes
  | EffectDecl Name [Var] [(Lambda)]    -- effect Name(params) = { op1, op2, ... }
  | HandlerDecl Name Name Bool [Var] [Expr]  -- handler Name(params) : EffectName = [default] { lets + impls } (Bool = isDefault)
  | HandleWith Expr Expr                -- handle expr with handlerExpr
  | ActionBlock [ActionStmt]            -- action { stmt, stmt, ... } (desugared to bind chains)
  | EffType Expr Expr                   -- Eff { row } resultType (in type positions)
  -- Class system nodes
  | ClassDecl Lambda ClassInfo          -- class Name(fields) [extends Parent(args)] [implements A,B] = { methods }
  | FixityDecl Assoc Int [Name]        -- infixl/infixr/infix prec (op1), (op2), ...
  | ExternFunc Name [Var] Expr (Maybe (Name, String))
    -- ^ extern function name(params) : RetType [= inline "add" | = llvm "llvm.sqrt.f64"];
    -- Nothing = plain C function call (runtime). Just ("inline", "add") = inline instruction.
    -- Just ("llvm", "llvm.sqrt.f64") = LLVM intrinsic call.
  -- Handler override at call site: func [Console = SilentConsole, FileIO = MockFileIO] (args)
  | OverrideApp Expr [(Maybe Name, Expr)] [Expr]
    -- OverrideApp func [(effectName?, handler)] args
    -- Named: [Console = SilentConsole]  -> [(Just "Console", Id "SilentConsole")]
    -- Positional: [SilentConsole]       -> [(Nothing, Id "SilentConsole")]
    -- Wildcard: [_]                     -> [(Nothing, Id "_")]
  | ERROR String


  -- ^^^ in case of anonymous application, `name` fields will be empty or index;
  -- typ is calculated for type checking. Optional + Explicit params.
    deriving (Show, Eq, Generic)

instance Binary Expr

-- Convenience pattern synonym so existing code matching on Type keeps working
pattern Type :: Expr
pattern Type = U (LConst 0)

-- | Non-dependent arrow type: A -> B = Pi Nothing A B
-- Backward-compatible pattern synonym — existing code using ArrowType still works.
pattern ArrowType :: Expr -> Expr -> Expr
pattern ArrowType a b = Pi Nothing a b

-- | Legacy pattern synonyms for pattern guards (backward compatibility)
pattern ExprConsTagCheck :: ConsTag -> Expr -> Expr
pattern ExprConsTagCheck ct e = PatternGuard (PCheckTag ct) e

pattern ExprLiteralCheck :: Literal -> Expr -> Expr
pattern ExprLiteralCheck lit e = PatternGuard (PCheckLit lit) e

-- | Backward-compatible pattern: positional tuple {e1, e2, ...}
-- Matches NTuple where all fields have Nothing names
pattern Tuple :: [Expr] -> Expr
pattern Tuple es <- NTuple (allPositional -> Just es)
  where Tuple es = NTuple (map (\e -> (Nothing, e)) es)

-- | Backward-compatible pattern: named record literal {x=1, y=2}
-- Matches NTuple where all fields have Just names
pattern RecordLit :: [(Name, Expr)] -> Expr
pattern RecordLit fields <- NTuple (allNamed -> Just fields)
  where RecordLit fields = NTuple (map (\(n,e) -> (Just n, e)) fields)

-- | Backward-compatible pattern: LTuple for positional tuple types
pattern LTuple :: [Expr] -> Literal
pattern LTuple es <- LNTuple (allPositional -> Just es)
  where LTuple es = LNTuple (map (\e -> (Nothing, e)) es)

-- | Helper: extract positional fields (all Nothing names)
allPositional :: [(Maybe Name, Expr)] -> Maybe [Expr]
allPositional fields
  | all (\(mn, _) -> mn == Nothing) fields = Just (map snd fields)
  | otherwise = Nothing

-- | Helper: extract named fields (all Just names)
allNamed :: [(Maybe Name, Expr)] -> Maybe [(Name, Expr)]
allNamed fields
  | all (\(mn, _) -> case mn of Just _ -> True; Nothing -> False) fields = Just [(n, e) | (Just n, e) <- fields]
  | otherwise = Nothing

-- | Check if an NTuple has any named fields
hasNamedFields :: [(Maybe Name, Expr)] -> Bool
hasNamedFields = any (\(mn, _) -> case mn of Just _ -> True; Nothing -> False)

-- | Get field expressions from NTuple (regardless of naming)
ntupleExprs :: [(Maybe Name, Expr)] -> [Expr]
ntupleExprs = map snd

{-
We will also handle types via Expr quite easily:
- Id Name - concrete
- App Expr [Expr] - type constructor (Maybe a etc) or a function calculating type
- Function Lambda - arrow type
-}

-- function that generates built-in operation to access i-th field
-- in a given tuple
mkTupleFieldAccessExpr :: Int -> Expr -> Expr
mkTupleFieldAccessExpr i e = RecFieldAccess ("",i) e

-- non-monadic traverse
traverseExpr :: (Expr -> Expr) -> Expr -> Expr
traverseExpr f UNDEFINED = UNDEFINED
traverseExpr f e@(Id name) = f e
traverseExpr f e@(Typed e1 e2) = Typed (f $ traverseExpr f e1) (f $ traverseExpr f e2)
traverseExpr f (App ex exs) = App (f $ traverseExpr f ex) (map f (map (traverseExpr f) exs) )
traverseExpr f (CaseOf args ex si) = CaseOf args (f $ traverseExpr f ex) si
traverseExpr f (PatternMatches exs) = PatternMatches (map f (map (traverseExpr f) exs))
traverseExpr f (Lit (LList exs)) = Lit (LList (map f (map (traverseExpr f) exs)))
traverseExpr f (Lit (LVec exs)) = Lit (LVec (map f (map (traverseExpr f) exs)))
traverseExpr f (Lit (LNTuple fields)) = Lit (LNTuple (map (\(mn,e) -> (mn, f $ traverseExpr f e)) fields))
traverseExpr f (Lit e) = Lit e
traverseExpr f (RecFieldAccess a ex) = RecFieldAccess a (f $ traverseExpr f ex)
traverseExpr f (PatternGuard pc ex) = PatternGuard pc (f $ traverseExpr f ex)
traverseExpr f (NTuple fields) = NTuple (map (\(mn,e) -> (mn, f $ traverseExpr f e)) fields)
traverseExpr f (ConTuple ct exs) = ConTuple ct (map f (map (traverseExpr f) exs))
traverseExpr f (DeclBlock exs) = DeclBlock (map f (map (traverseExpr f) exs))
traverseExpr f (ExpandedCase exs ex si) = ExpandedCase (map f (map (traverseExpr f) exs)) (f $ traverseExpr f ex) si
traverseExpr f (Statements exs) = Statements (map f (map (traverseExpr f) exs))
traverseExpr f (UnaryOp nm ex) = UnaryOp nm (f $ traverseExpr f ex)
traverseExpr f (BinaryOp nm e1 e2) = BinaryOp nm (f $ traverseExpr f e1) (f $ traverseExpr f e2)
-- for functions only traversing the body for now:
-- probably need to add type sig as well???
traverseExpr f (Function lam) = Function $ lam { body = f $ traverseExpr f (body lam) }
traverseExpr f (Action lam) = Action $ lam { body = f $ traverseExpr f (body lam) }
traverseExpr f (Structure lam si) = Structure (lam { body = f $ traverseExpr f (body lam) }) si
traverseExpr f (SumType lam) = SumType $ lam { body = f $ traverseExpr f (body lam) }
-- TODO: CHECK IF THIS IS THE CORRECT TRAVERSAL: !!!!
traverseExpr f (Constructors lams) = Constructors $ map (\l-> l {body = f $ traverseExpr f (body l) } ) lams
traverseExpr f (Binding (Var nm tp val)) = Binding (Var nm (f $ traverseExpr f tp) (f $ traverseExpr f val))
traverseExpr _ e@(U _) = e
traverseExpr _ e@(Meta _) = e
traverseExpr _ RowEmpty = RowEmpty
traverseExpr f (RowExtend n t r) = RowExtend n (f $ traverseExpr f t) (f $ traverseExpr f r)
traverseExpr f (Sigma mn e1 e2) = Sigma mn (f $ traverseExpr f e1) (f $ traverseExpr f e2)
traverseExpr f (Instance nm mTag targs impls reqs) = Instance nm mTag (map (f . traverseExpr f) targs) (map (f . traverseExpr f) impls) (map (f . traverseExpr f) reqs)
traverseExpr f (NamedRef e tag) = NamedRef (f $ traverseExpr f e) tag
traverseExpr f (IfThenElse c t e) = IfThenElse (f $ traverseExpr f c) (f $ traverseExpr f t) (f $ traverseExpr f e)
traverseExpr f (LetIn binds body) = LetIn (map (\(v,ex) -> (v, f $ traverseExpr f ex)) binds) (f $ traverseExpr f body)
traverseExpr f (Law lam ex) = Law (lam { body = f $ traverseExpr f (body lam) }) (f $ traverseExpr f ex)
traverseExpr f (PropEq e1 e2) = PropEq (f $ traverseExpr f e1) (f $ traverseExpr f e2)
traverseExpr f (Implies e1 e2) = Implies (f $ traverseExpr f e1) (f $ traverseExpr f e2)
traverseExpr f (Pi mn e1 e2) = Pi mn (f $ traverseExpr f e1) (f $ traverseExpr f e2)
traverseExpr f (Implicit e) = Implicit (f $ traverseExpr f e)
traverseExpr f (Value v ex) = Value v (f $ traverseExpr f ex)
traverseExpr _ e@(Primitive _) = e
traverseExpr _ Intrinsic = Intrinsic
traverseExpr _ Derive = Derive
traverseExpr f (Repr userTp tp def fns inv) = Repr (f $ traverseExpr f userTp) (f $ traverseExpr f tp) def (map (f . traverseExpr f) fns) (fmap (f . traverseExpr f) inv)
traverseExpr f (ReprCast e tp) = ReprCast (f $ traverseExpr f e) (f $ traverseExpr f tp)
traverseExpr f (RecordType fields isOpen) = RecordType (map (\(n,e) -> (n, f $ traverseExpr f e)) fields) isOpen
-- Module system nodes (leaf nodes, no child expressions to traverse)
traverseExpr _ e@(ModuleDecl _) = e
traverseExpr _ e@(Import _ _ _) = e
traverseExpr _ e@(Open _) = e
traverseExpr _ e@(Export _ _) = e
traverseExpr f (PrivateDecl e) = PrivateDecl (f $ traverseExpr f e)
traverseExpr f (OpaqueTy lam e) = OpaqueTy (lam { body = f $ traverseExpr f (body lam) }) (f $ traverseExpr f e)
traverseExpr f (TargetBlock nm exs) = TargetBlock nm (map (f . traverseExpr f) exs)
traverseExpr f (TargetSwitch cases) = TargetSwitch (map (\(n,e) -> (n, f $ traverseExpr f e)) cases)
traverseExpr f (ArrayLit exs) = ArrayLit (map (f . traverseExpr f) exs)
-- Record system completion nodes
traverseExpr f (RecordConstruct nm fields) = RecordConstruct nm (map (\(n,e) -> (n, f $ traverseExpr f e)) fields)
traverseExpr f (RecordUpdate e fields) = RecordUpdate (f $ traverseExpr f e) (map (\(n,ex) -> (n, f $ traverseExpr f ex)) fields)
traverseExpr f (RecordPattern nm fields) = RecordPattern nm (map (\(n,e) -> (n, f $ traverseExpr f e)) fields)
-- Effect system nodes
traverseExpr f (EffectDecl nm ps ops) = EffectDecl nm ps (map (\l -> l { body = f $ traverseExpr f (body l) }) ops)
traverseExpr f (HandlerDecl nm eff isDef ps impls) = HandlerDecl nm eff isDef ps (map (f . traverseExpr f) impls)
traverseExpr f (HandleWith e h) = HandleWith (f $ traverseExpr f e) (f $ traverseExpr f h)
traverseExpr f (ActionBlock stmts) = ActionBlock (map (traverseActionStmt f) stmts)
traverseExpr f (EffType row res) = EffType (f $ traverseExpr f row) (f $ traverseExpr f res)
-- Class system
traverseExpr f (ClassDecl lam ci) = ClassDecl (lam { body = f $ traverseExpr f (body lam) }) ci
-- Fixity declarations (leaf node)
traverseExpr _ e@(FixityDecl _ _ _) = e
traverseExpr _ e@(ExternFunc _ _ _ _) = e
-- Handler override at call site
traverseExpr f (OverrideApp func overrides args) =
    OverrideApp (f $ traverseExpr f func)
                (map (\(mn, e) -> (mn, f $ traverseExpr f e)) overrides)
                (map (f . traverseExpr f) args)
traverseExpr _ e@(ERROR _) = e
traverseExpr f e = ERROR $ "Traverse not implemented for: " ++ ppr e

-- Helper for traversing action statements
traverseActionStmt :: (Expr -> Expr) -> ActionStmt -> ActionStmt
traverseActionStmt f (ActionBind nm e) = ActionBind nm (f $ traverseExpr f e)
traverseActionStmt f (ActionLet nm e) = ActionLet nm (f $ traverseExpr f e)
traverseActionStmt f (ActionExpr e) = ActionExpr (f $ traverseExpr f e)

-- | Deep traversal variant that also traverses into CaseOf pattern variables (val fields).
-- Use this when your transformation needs to reach into pattern match arguments.
-- Regular traverseExpr skips CaseOf Var val fields (by design — many passes don't need them).
-- This is a full recursive traversal — delegates to traverseExpr for all non-CaseOf nodes,
-- but replaces `f` in recursive calls to ensure deep traversal throughout.
traverseExprDeep :: (Expr -> Expr) -> Expr -> Expr
traverseExprDeep f = go
  where
    traverseVar v = v { typ = f $ go (typ v)
                      , val = f $ go (val v) }
    go (CaseOf args ex si) =
        CaseOf (map traverseVar args) (f $ go ex) si
    -- For all other nodes, use traverseExpr but with `go` as the recursive function
    -- so that CaseOf nodes deeper in the tree are also handled.
    go (PatternMatches exs) = PatternMatches (map (f . go) exs)
    go (Function lam) = Function $ lam { body = f $ go (body lam) }
    go (App ex exs) = App (f $ go ex) (map (f . go) exs)
    go (NTuple fields) = NTuple (map (\(mn,e) -> (mn, f $ go e)) fields)
    go (DeclBlock exs) = DeclBlock (map (f . go) exs)
    go (ConTuple ct exs) = ConTuple ct (map (f . go) exs)
    go (ExpandedCase exs ex si) = ExpandedCase (map (f . go) exs) (f $ go ex) si
    go e = traverseExpr f e  -- all other cases delegate to traverseExpr (no CaseOf inside)

-- App (Id "Succ") [App (Id "plus") [Id "n",Id "x"]]

-- f(x) = expr, x = val, substituting all x appearances in expr for val
betaReduce :: Var -> Expr -> Expr
-- substituting all nm occurences in expr for val
-- no typechecking whatsoever
betaReduce (Var nm tp val) expr = traverseExpr subs expr
  where subs :: Expr -> Expr
        subs e@(Id name) = if (nm == name) then val else e
        subs e = e

-- --------------------------------- IMPLICIT / STRUCTURE LAMBDAS --------------------------------------------
{-
Since we are representing functions inside structures as functions with implicit
type parameters, we need some helper methods to manipulate them
-}
-- first, identifying such a function:
hasImplicit :: Lambda -> Bool
hasImplicit lam = case params lam of
    (Var _ (Implicit _) _):_ -> True
    _ -> False

-- | Check if a Var has an Implicit type wrapper
isImplicitVar :: Var -> Bool
isImplicitVar (Var _ (Implicit _) _) = True
isImplicitVar _ = False

-- getting "default" case from inside any function that has cases:
getDefaultCase :: Lambda -> Expr
getDefaultCase lam = case body lam of
    PatternMatches ( (CaseOf [] ex _):_ ) -> ex
    PatternMatches ( (ExpandedCase [] ex _):_ ) -> ex
    b -> b

-- | Collect all Id name references in an expression (shallow, non-recursive into sub-lambdas' params)
collectIdRefs :: Expr -> [Name]
collectIdRefs = go
  where
    go (Id n)             = [n]
    go (App e es)         = go e ++ concatMap go es
    go (NTuple fields)    = concatMap (go . snd) fields
    go (ConTuple _ es)    = concatMap go es
    go (BinaryOp _ e1 e2) = go e1 ++ go e2
    go (UnaryOp _ e)      = go e
    go (Function lam)     = go (body lam)
    go (PatternMatches es) = concatMap go es
    go (CaseOf _ e _)     = go e
    go (ExpandedCase _ e _) = go e
    go (Typed e _)        = go e
    go (Lit _)            = []
    go UNDEFINED          = []
    go (RowExtend _ t r)  = go t ++ go r
    go (Sigma _ a b)      = go a ++ go b
    go _                  = []

-- --------------------------------- PRETTY PRINTING --------------------------------------------

instance PrettyPrint Lambda where
  ppr lam = lamName lam ++ " "
    ++ showListRoBr ppr (params lam)
    ++ pprTyp (lamType lam)
    ++ if (body lam == UNDEFINED) then "" else " = " ++ ppr (body lam)

pprTyp ex = if (ex == UNDEFINED) then "" else ":" ++ ppr ex

instance PrettyPrint Var where
  ppr (Var n t _) = as [bold] n ++ if (t == UNDEFINED) then "" else ":" ++ ppr t

ppVarCaseOf :: Var -> String
ppVarCaseOf (Var n t val) = "case " ++ n ++ " of " ++ ppr val

pprRecordRo :: (Var -> String) -> Record -> String
pprRecordRo f r = showListRoBr f r

instance PrettyPrint Expr where
  ppr UNDEFINED = ""
  ppr (Id v) = as [bold] v
  ppr (CaseOf e1 e2 _) = showListCuBr ppVarCaseOf e1 ++ " -> " ++ ppr e2
  ppr (PatternGuard (PCheckTag (ConsTag nm i)) ex) = (as [bold,lblue] "checkConsTag")
    ++ "(" ++ nm ++ ", " ++ ppr ex ++ ")"
  ppr (PatternGuard (PCheckLit lit) ex) = (as [bold,lblue] "checkLit")
    ++ "(" ++ show lit ++ ", " ++ ppr ex ++ ")"
  ppr (ExpandedCase exs ex _) = showListSqBr ppr exs ++ " -> " ++ ppr ex
  ppr (PatternMatches ps) = "match " ++ concatMap (\p -> "| " ++ ppr p ++ " ") ps
  ppr (RecFieldAccess (nm,i) e) = ppr e ++ "." ++ nm ++"("++show i ++")"
  ppr (App e ex) = (ppr e) ++ showListRoBr ppr ex
  ppr (NTuple fields) = "{" ++ showListPlainSep pprNTupleField ", " fields ++ "}"
  ppr (DeclBlock ex) = showListCuBr ppr ex
  ppr (Structure lam si) = (as [bold,yellow] (pprStructKind (structKind si) ++ " "))
    ++ lamName lam ++ " "
    ++ showListRoBr ppr (params lam)
    ++ pprTyp (lamType lam)
    ++ pprExtends (structExtends si)
    ++ pprRequires (structRequires si)
    ++ " = "
    ++ ppr (body lam)
  ppr (Function lam) = (as [bold,red] "function ") ++ ppr lam
  ppr (Action lam) = (as [bold,blue] "action ") ++ ppr lam
  ppr (ConTuple (ConsTag nm i) ex) = (as [bold] nm) ++ " "  ++ show i ++ " " ++ showListCuBr ppr ex
  ppr (Statements ex) = showListCuBr ppr ex
  ppr (Binding (Var nm tp val)) = as [bold] nm ++ pprTyp tp ++ " = " ++ ppr val 
  ppr (Constructors cs) = showListCuBr ppr cs
  ppr (UnaryOp nm ex) = nm ++ ppr ex
  ppr (BinaryOp nm e1 e2) = ppr e1 ++ nm ++ (ppr e2)
  ppr (SumType lam) = lamName lam ++ " "
    ++ showListRoBr ppr (params lam)
    ++ pprTyp (lamType lam) ++ " = "
    ++ pprConstructors (body lam)
  ppr (Instance nm mTag targs impls reqs) = (as [bold,green] "instance ") ++ nm
    ++ showListRoBr ppr targs
    ++ maybe "" (\t -> " as " ++ t) mTag
    ++ pprRequires reqs
    ++ " = "
    ++ showListCuBr ppr impls
  ppr (NamedRef e tag) = ppr e ++ " as " ++ tag
  ppr (IfThenElse c t e) = "if " ++ ppr c ++ " then " ++ ppr t ++ " else " ++ ppr e
  ppr (LetIn binds body) = "let " ++ showListPlainSep (\(v,ex) -> ppr v ++ " = " ++ ppr ex) ", " binds ++ " in " ++ ppr body
  ppr (Implicit e) = "Implicit (" ++ ppr e ++ ")"
  ppr (Law lam ex) = (as [bold,cyan] "law ") ++ ppr lam ++ " = " ++ ppr ex
  ppr (PropEq e1 e2) = ppr e1 ++ " === " ++ ppr e2
  ppr (Implies e1 e2) = ppr e1 ++ " ==> " ++ ppr e2
  ppr (Pi Nothing e1 e2) = pprArrowLhs e1 ++ " -> " ++ ppr e2
  ppr (Pi (Just n) e1 e2) = "(" ++ n ++ ":" ++ ppr e1 ++ ") -> " ++ ppr e2
  ppr (Value v ex) = (as [bold,magenta] "value ") ++ ppr v ++ if ex == UNDEFINED then "" else " = " ++ ppr ex
  ppr (Primitive lam) = (as [bold,magenta] "primitive ") ++ ppr lam
  ppr Intrinsic = as [bold,cyan] "intrinsic"
  ppr Derive = as [bold,cyan] "derive"
  ppr (Repr nm tp def fns inv) = (as [bold,magenta] "repr ") ++ ppr nm ++ " as " ++ ppr tp
    ++ (if def then " default" else "") ++ " where { " ++ showListPlainSep ppr ", " fns ++ " }"
  ppr (ReprCast e tp) = ppr e ++ " as " ++ ppr tp
  ppr (RecordType fields isOpen) = "{" ++ showListPlainSep (\(n,e) -> n ++ ":" ++ ppr e) ", " fields ++ (if isOpen then ", .." else "") ++ "}"
  -- Module system
  ppr (ModuleDecl path) = "module " ++ showModPath path
  ppr (Import path spec tgt) = "import " ++ showModPath path ++ pprImportSpec spec ++ maybe "" (\t -> " target " ++ t) tgt
  ppr (Open path) = "open " ++ showModPath path
  ppr (Export path mNames) = "export " ++ showModPath path ++ maybe "" (\ns -> " (" ++ showListPlainSep id ", " ns ++ ")") mNames
  ppr (PrivateDecl e) = "private " ++ ppr e
  ppr (OpaqueTy lam e) = "opaque type " ++ ppr lam ++ " = " ++ ppr e
  ppr (TargetBlock nm exs) = "target " ++ nm ++ " { " ++ showListPlainSep ppr "; " exs ++ " }"
  ppr (TargetSwitch cases) = "target " ++ showListPlainSep (\(n,e) -> "| " ++ n ++ " -> " ++ ppr e) " " cases
  ppr (ArrayLit exs) = "[" ++ showListPlainSep ppr ", " exs ++ "]"
  ppr (RecordConstruct nm fields) = nm ++ " {" ++ showListPlainSep (\(n,e) -> n ++ " = " ++ ppr e) ", " fields ++ "}"
  ppr (RecordUpdate e fields) = ppr e ++ " {" ++ showListPlainSep (\(n,ex) -> n ++ " = " ++ ppr ex) ", " fields ++ "}"
  ppr (RecordPattern nm fields) = nm ++ " {" ++ showListPlainSep (\(n,e) -> n ++ " = " ++ ppr e) ", " fields ++ "}"
  -- Effect system
  ppr (EffectDecl nm ps ops) = (as [bold,cyan] "effect ") ++ nm ++ showListRoBr ppr ps ++ " = {" ++ showListPlainSep ppr ", " (map Function ops) ++ "}"
  ppr (HandlerDecl nm eff isDef ps impls) = (as [bold,cyan] "handler ") ++ nm ++ (if null ps then "" else showListRoBr ppr ps) ++ " : " ++ eff ++ " = " ++ (if isDef then "default " else "") ++ "{" ++ showListPlainSep ppr ", " impls ++ "}"
  ppr (HandleWith e h) = "handle " ++ ppr e ++ " with " ++ ppr h
  ppr (ActionBlock stmts) = "action {" ++ showListPlainSep pprActionStmt ", " stmts ++ "}"
  ppr (EffType row res) = "Eff " ++ ppr row ++ " " ++ ppr res
  -- Class system
  ppr (ClassDecl lam ci) = (as [bold,magenta] (pprClassMod (classModifier ci) ++ "class "))
    ++ lamName lam ++ " "
    ++ showListRoBr ppr (params lam)
    ++ pprClassParent (classParent ci)
    ++ pprImplements (classImplements ci)
    ++ " = " ++ ppr (body lam)
  ppr (FixityDecl assoc prec ops) = pprAssoc assoc ++ " " ++ show prec ++ " " ++ showListPlainSep (\o -> "(" ++ o ++ ")") ", " ops
  ppr (ExternFunc nm ps ret Nothing) = "extern function " ++ nm ++ "(" ++ showListPlainSep ppr ", " ps ++ ") : " ++ ppr ret
  ppr (ExternFunc nm ps ret (Just (kind, spec))) = "extern function " ++ nm ++ "(" ++ showListPlainSep ppr ", " ps ++ ") : " ++ ppr ret ++ " = " ++ kind ++ " \"" ++ spec ++ "\""
  ppr (OverrideApp func overrides args) = ppr func ++ " [" ++ showListPlainSep pprOverride ", " overrides ++ "] (" ++ showListPlainSep ppr ", " args ++ ")"
    where pprOverride (Nothing, e) = ppr e
          pprOverride (Just n, e) = n ++ " = " ++ ppr e
  ppr (U (LConst 0)) = "Type"
  ppr (U (LConst n)) = "Type" ++ show n
  ppr (U l) = "Type(" ++ showLevel l ++ ")"
  ppr (Meta n) = "?" ++ show n
  ppr RowEmpty = "{}"
  ppr (RowExtend n t r) = "{" ++ n ++ ":" ++ ppr t ++ " | " ++ ppr r ++ "}"
  ppr (Sigma Nothing a b) = ppr a ++ " * " ++ ppr b
  ppr (Sigma (Just n) a b) = "(" ++ n ++ ":" ++ ppr a ++ ") * " ++ ppr b
  ppr e = show e
  -- λ

-- Pretty print constructors with pipe separators (new syntax)
pprConstructors :: Expr -> String
pprConstructors (Constructors cs) = showListPlainSep ppr " | " cs
pprConstructors e = ppr e

-- Pretty-print a single NTuple field: named (x = expr) or positional (expr)
pprNTupleField :: (Maybe Name, Expr) -> String
pprNTupleField (Nothing, e) = ppr e
pprNTupleField (Just n, e) = n ++ " = " ++ ppr e

pprArrowLhs :: Expr -> String
pprArrowLhs e@(Pi _ _ _) = "(" ++ ppr e ++ ")"
pprArrowLhs e = ppr e

pprActionStmt :: ActionStmt -> String
pprActionStmt (ActionBind nm e) = nm ++ " <- " ++ ppr e
pprActionStmt (ActionLet nm e) = nm ++ " = " ++ ppr e
pprActionStmt (ActionExpr e) = ppr e

pprStructKind :: StructKind -> String
pprStructKind SGeneral  = "structure"
pprStructKind SAlgebra  = "trait"
pprStructKind SMorphism = "bridge"

showModPath :: ModulePath -> String
showModPath = showListPlainSep id "."

pprImportSpec :: ImportSpec -> String
pprImportSpec ImportAll = ""
pprImportSpec (ImportOnly ns) = " (" ++ showListPlainSep id ", " ns ++ ")"
pprImportSpec (ImportHiding ns) = " hiding (" ++ showListPlainSep id ", " ns ++ ")"
pprImportSpec (ImportAs n) = " as " ++ n

pprExtends :: [Expr] -> String
pprExtends [] = ""
pprExtends es = " extends " ++ showListPlainSep ppr ", " es

pprRequires :: [Expr] -> String
pprRequires [] = ""
pprRequires es = " requires " ++ showListPlainSep ppr ", " es

pprAssoc :: Assoc -> String
pprAssoc AssocLeft  = "infixl"
pprAssoc AssocRight = "infixr"
pprAssoc AssocNone  = "infix"

pprClassMod :: ClassModifier -> String
pprClassMod ClassNormal   = ""
pprClassMod ClassAbstract = "abstract "
pprClassMod ClassSealed   = "sealed "

pprClassParent :: Maybe (Name, [Expr]) -> String
pprClassParent Nothing = ""
pprClassParent (Just (nm, [])) = " extends " ++ nm
pprClassParent (Just (nm, args)) = " extends " ++ nm ++ showListRoBr ppr args

pprImplements :: [Expr] -> String
pprImplements [] = ""
pprImplements es = " implements " ++ showListPlainSep ppr ", " es