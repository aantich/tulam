# Class System Design: First-Class OOP with 1-1 Target Mapping

## 1. Motivation

tulam targets three OOP-heavy platforms (.NET, JS, C++) but currently has no native class concept. The interop design treats external classes as second-class citizens requiring translation layers. This document introduces first-class classes into tulam such that:

1. **tulam classes map 1-1 to target classes** — no encoding tricks, no wrapping
2. **External .NET/JS/C++ classes can be subclassed transparently** from tulam
3. **Classes compose with existing features** — algebras, effects, records, pattern matching
4. **The "tuples + lambdas" foundation is preserved** — classes desugar cleanly

### Design Principle

**A class = a tagged tuple (data) + a method table (behavior) + an inheritance chain (subtyping).**

At the CLM level, objects are plain `CLMCON` values. Method dispatch is a new `CLMMCALL` node resolved via class metadata in Environment. Codegen emits native classes for each target — the CLM-level mechanism is an interpreter concern only.

---

## 2. Syntax

### 2.1 Class Declaration

```tulam
class Animal(name:String, age:Int) = {
    function speak(self:Animal) : String;

    function info(self:Animal) : String =
        self.name ++ " (age " ++ show(self.age) ++ ")"
};
```

- **Fields** in the header — like record/constructor params
- **Methods** in the body — like structure declarations
- **Explicit `self`** — no implicit `this` (consistent with tulam philosophy)
- **Abstract methods** have no body (must be overridden by subclasses)
- **Concrete methods** have a default body (can be overridden)
- All methods are **virtual by default** (like Java/Kotlin, maps cleanly to all targets)

### 2.2 Inheritance

```tulam
class Dog(breed:String) extends Animal = {
    override function speak(self:Dog) : String = "Woof!";

    function fetch(self:Dog, item:String) : String =
        self.name ++ " fetches " ++ item
};

class Cat() extends Animal = {
    override function speak(self:Cat) : String = "Meow!"
};
```

- **Single inheritance** via `extends` (covers .NET, JS, Java; avoids diamond problem)
- Subclass declares **only its own new fields** (parent fields are inherited)
- `override` keyword is **optional but checked** — if present, compiler verifies a parent method exists
- `final` keyword on a method prevents override in subclasses

### 2.3 Construction

```tulam
// Parent fields first, then own fields
let d = Dog.new("Rex", 3, "Labrador");

// Named construction (optional, reuses record syntax)
let d = Dog.new(name = "Rex", age = 3, breed = "Labrador");
```

`ClassName.new(args)` is the universal constructor syntax. The compiler generates it automatically. Arguments are ordered: parent fields first (in inheritance order), then own fields.

### 2.4 Super Constructor (for extern class extension)

When extending an external class, the super constructor call specifies how to initialize the parent:

```tulam
import System.Windows.Forms target dotnet;

class MyButton(label:String) extends Button(label) = {
    override function OnClick(self:MyButton, e:EventArgs) : Unit =
        putStrLn("Clicked: " ++ self.label)
};
```

`extends Button(label)` means: call `Button`'s constructor with `label`. The super-args are expressions evaluated at construction time.

For tulam-native inheritance, super-args are implicit (parent fields are passed through):

```tulam
class Dog(breed:String) extends Animal = { ... };
// Dog.new(name, age, breed) → internally: super = Animal(name, age), own = (breed)
```

Explicit super-args are only needed when the parent constructor signature differs from a simple field pass-through (common with extern classes).

### 2.5 Abstract Classes

```tulam
abstract class Shape(color:String) = {
    function area(self:Shape) : Float64; // abstract
    function describe(self:Shape) : String =     // concrete
        "A " ++ self.color ++ " shape"
};

// Shape.new("red") → compile error: cannot instantiate abstract class
```

### 2.6 Sealed Classes

```tulam
sealed class Result(a:Type) = {};

class Ok(value:a) extends Result(a) = {};
class Err(message:String) extends Result(a) = {};

// Exhaustive pattern matching enabled:
function unwrap(r:Result(a)) : a = match r
    | Ok(v) -> v
    | Err(msg) -> error(msg);
```

`sealed` means: all subclasses must be declared in the same module. This enables exhaustive matching (bridging the sum-type / class-hierarchy gap).

Target mapping:
- .NET: `sealed` (C# sealed classes) + subclass restriction
- Kotlin: `sealed class`
- JS: runtime-only enforcement (or trusted)
- C++: `final` on leaf classes + design convention

### 2.7 Implementing Algebras

```tulam
class Circle(radius:Float64) extends Shape implements Eq, Show = {
    override function area(self:Circle) : Float64 =
        3.14159 * self.radius * self.radius;

    function (==)(self:Circle, other:Circle) : Bool =
        self.radius == other.radius;

    function show(self:Circle) : String =
        "Circle(r=" ++ show(self.radius) ++ ")"
};
```

`implements Eq, Show` generates `instance Eq(Circle)` and `instance Show(Circle)` declarations using the methods defined in the class body. Methods matching an algebra's interface are extracted automatically.

### 2.8 Method Call and Field Access

```tulam
let d = Dog.new("Rex", 3, "Lab");
d.speak()           // method call → dynamic dispatch
d.name              // field access → direct index
d.fetch("ball")     // method call with args
```

Dot notation resolves in order:
1. **Data field** (from class fields) → `CLMFieldAccess`
2. **Method** (from class methods including inherited) → `CLMMCALL`
3. **Algebra method** (from `implements` declarations) → existing `CLMIAP`
4. **Error** if nothing matches

### 2.9 Subtype Polymorphism

```tulam
function greet(a:Animal) : String = a.speak();

let d = Dog.new("Rex", 3, "Lab");
greet(d)     // implicit upcast Dog → Animal, dynamic dispatch → "Woof!"

// Downcast is explicit and safe:
function tryGetDog(a:Animal) : Maybe(Dog) = a as Dog;
```

- **Upcasts are implicit** — Dog can be passed where Animal is expected (zero-cost)
- **Downcasts are explicit** via `as` → returns `Maybe(TargetType)` (reuses existing reserved keyword)

### 2.10 Static Methods

```tulam
class MathUtils() = {
    static function lerp(a:Float64, b:Float64, t:Float64) : Float64 =
        a + (b - a) * t
};

MathUtils.lerp(0.0, 10.0, 0.5)   // 5.0 — no instance needed
```

Static methods have no `self` parameter. They're called on the class name directly. In CLM, they're just regular top-level functions namespaced by class.

---

## 3. Extern Class Integration

### 3.1 Unified Class Model

The key insight: **extern classes and tulam classes share the same `ClassMeta` structure.** An extern class is simply a `ClassMeta` populated from target metadata rather than from tulam source.

```
                      ClassMeta
                     /         \
          tulam source        extern metadata
          (parser)            (.NET assembly / .d.ts / C++ header)
              |                    |
         ClassDecl AST       Metadata Resolver
              |                    |
              \----> Environment <----/
                    classDecls map
```

Once in `classDecls`, an extern class is indistinguishable from a tulam class. Subclassing, method calls, field access — everything works uniformly.

### 3.2 Importing Extern Classes

```tulam
import System.Windows.Forms target dotnet;
// Metadata resolver populates classDecls with:
//   "Form"    → ClassMeta { parent = Just "ContainerControl", fields = [...], methods = {...} }
//   "Button"  → ClassMeta { parent = Just "ButtonBase", fields = [...], methods = {...} }
//   "Control" → ClassMeta { parent = Just "Component", fields = [...], methods = {...} }
//   ... entire hierarchy
```

The metadata resolver reads:
- **.NET**: Assembly metadata via Mono.Cecil — classes, methods, properties, inheritance, generics, interfaces
- **JS**: `.d.ts` files — class declarations, extends, method signatures
- **C++**: Headers via libclang — class definitions, virtual methods, inheritance

Each becomes a `ClassMeta` entry. Properties become field+method pairs. Interfaces become algebra instances.

### 3.3 Subclassing Extern Classes

```tulam
import System.Windows.Forms target dotnet;

class CounterForm() extends Form("Counter App") = {
    private counterRef : Ref(Int) = newRef(0);

    function setupUI(self:CounterForm) : Unit = action {
        let label = Label.new();
        label.Text = "0";
        label.Location = Point.new(100, 50);
        self.Controls.Add(label);

        let btn = Button.new();
        btn.Text = "Increment";
        btn.Location = Point.new(90, 100);
        btn.Click.Add(\s, e -> action {
            modifyRef(self.counterRef, \n -> n + 1);
            count <- readRef(self.counterRef);
            label.Text = show(count)
        });
        self.Controls.Add(btn)
    };

    override function OnLoad(self:CounterForm, e:EventArgs) : Unit = action {
        self.setupUI();
        super.OnLoad(e)
    }
};

action main() = {
    Application.EnableVisualStyles();
    Application.Run(CounterForm.new())
};
```

Key points:
- `extends Form("Counter App")` — super constructor call passes title to Form
- `override function OnLoad` — overrides a virtual .NET method
- `super.OnLoad(e)` — calls parent implementation (new syntax, see Section 3.4)
- `self.Controls.Add(btn)` — calls inherited .NET method via dynamic dispatch
- `self.counterRef` — accesses a tulam-side field that only exists on CounterForm
- Mutation via `Ref` + effect system — no new mutability model needed

### 3.4 Super Calls

```tulam
super.methodName(args)    // calls the parent class's implementation
```

In CLM: `CLMSCALL obj "methodName" [args]` — like `CLMMCALL` but always resolves to the **parent** class's method, not the object's runtime class.

Target mapping:
- .NET: `base.MethodName(args)`
- JS: `super.methodName(args)`
- C++: `Base::methodName(args)`

### 3.5 Extern Field Layout

For tulam-native classes, fields are laid out in the `CLMCON` tuple and we control the layout. For extern classes, the object is **opaque** at the CLM level — fields and methods are accessed through the target's own mechanisms.

At the interpreter level, extern objects are represented as a special value:

```haskell
| CLMEXTERN Name (HashMap Name CLMExpr)  -- className, field cache
```

But for codegen (the primary path), extern objects are native target objects — no translation needed. The interpreter's `CLMEXTERN` is only for testing/REPL purposes.

### 3.6 Mixed Hierarchy Field Layout

When a tulam class extends an extern class:

```tulam
class MyButton(label:String) extends Button("") = { ... };
```

**Codegen output (.NET):**
```csharp
public class MyButton : Button {
    public string Label { get; }
    public MyButton(string label) : base(label) {
        this.Label = label;
    }
}
```

The extern parent's fields are managed by the target runtime. Only the tulam-added fields (`label`) are explicitly declared.

**Interpreter**: The object carries both the extern field cache (from parent) and own fields:
```haskell
CLMCON (ConsTag "MyButton" 200) [CLMLIT (LString "my label")]
-- + extern parent state is managed via CLMEXTERN proxy
```

This means: in the interpreter, method calls on extern parents must delegate to an extern bridge (stub layer for testing). For full extern interop, codegen is the real path.

---

## 4. AST Representation

### 4.1 New Surface AST Nodes

In `Surface.hs`:

```haskell
-- Classification for class modifiers
data ClassModifier = ClassNormal | ClassAbstract | ClassSealed
    deriving (Show, Eq)

-- Method modifier
data MethodModifier = MNone | MOverride | MFinal | MStatic
    deriving (Show, Eq)

-- Class metadata
data ClassInfo = ClassInfo {
    classParent      :: Maybe (Name, [Expr]),  -- parent name + super constructor args
    classImplements  :: [Expr],                -- algebra names: Eq, Show, etc.
    classModifier    :: ClassModifier;
    classExtern      :: Maybe Name             -- Just "dotnet" | Just "js" | Just "native" | Nothing
} deriving (Show, Eq)

defaultClassInfo :: ClassInfo
defaultClassInfo = ClassInfo Nothing [] ClassNormal Nothing

-- Added to Expr:
| ClassDecl Lambda ClassInfo
  -- Lambda.lamName   = class name ("Animal")
  -- Lambda.params    = own data fields [Var "name" String, Var "age" Int]
  -- Lambda.body      = DeclBlock [Function method1, Function method2]
  -- Lambda.lamType   = Type (or App for generic classes)
```

Method lambdas carry their modifier via a naming convention or a new field:

```haskell
-- Option A: Convention — prefix method name
-- "override$speak", "final$info", "static$lerp"

-- Option B: Add modifier to Lambda (cleaner but more invasive)
data Lambda = Lambda {
    lamName    :: Name;
    params     :: Record;
    body       :: Expr;
    lamType    :: Expr;
    lamSrcInfo :: SourceInfo;
    lamMod     :: MethodModifier    -- NEW (default MNone for non-class methods)
} deriving (Show, Eq)
```

Recommendation: **Option B** is cleaner. The `lamMod` field defaults to `MNone` for all existing code (backward-compatible). `mkLambda` sets it to `MNone`.

### 4.2 Examples

`class Animal(name:String, age:Int) = { ... }` parses to:

```haskell
ClassDecl
  (Lambda "Animal"
    [ Var "name" (Id "String") UNDEFINED
    , Var "age" (Id "Int") UNDEFINED ]
    (DeclBlock
      [ Function (Lambda "speak" [Var "self" (Id "Animal") UNDEFINED] UNDEFINED (Id "String") si MNone)
      , Function (Lambda "info"  [Var "self" (Id "Animal") UNDEFINED] body     (Id "String") si MNone)
      ])
    Type si MNone)
  (ClassInfo { classParent = Nothing, classImplements = [], classModifier = ClassNormal, classExtern = Nothing })
```

`class Dog(breed:String) extends Animal = { ... }` parses to:

```haskell
ClassDecl
  (Lambda "Dog"
    [ Var "breed" (Id "String") UNDEFINED ]
    (DeclBlock
      [ Function (Lambda "speak" [Var "self" (Id "Dog") UNDEFINED] body (Id "String") si MOverride)
      , Function (Lambda "fetch" [Var "self" (Id "Dog") UNDEFINED, Var "item" (Id "String") UNDEFINED] body (Id "String") si MNone)
      ])
    Type si MNone)
  (ClassInfo { classParent = Just ("Animal", []), classImplements = [], classModifier = ClassNormal, classExtern = Nothing })
```

---

## 5. Environment Representation

### 5.1 ClassMeta in State.hs

```haskell
data ClassMeta = ClassMeta {
    cmParent        :: Maybe Name,             -- parent class name
    cmAllFields     :: [Var],                  -- ALL fields: inherited then own
    cmOwnFields     :: [Var],                  -- this class's new fields only
    cmMethods       :: NameMap CLMLam,          -- ALL methods (inherited + overridden + new)
    cmOwnMethods    :: NameMap Lambda,          -- this class's declared methods (Surface AST)
    cmStaticMethods :: NameMap CLMLam,          -- static methods
    cmFieldIndices  :: NameMap Int,             -- field name → positional index
    cmModifier      :: ClassModifier,           -- Normal | Abstract | Sealed
    cmChildren      :: [Name],                  -- direct subclass names (for sealed checking)
    cmImplements    :: [Name],                  -- algebra names
    cmSuperArgs     :: [Expr],                  -- super constructor argument expressions
    cmExtern        :: Maybe Name,              -- target name if extern, Nothing if tulam-native
    cmTag           :: Int                       -- unique class tag for ConsTag
} deriving (Show, Eq)
```

### 5.2 Environment Extension

```haskell
data Environment = Environment {
    types           :: NameMap Expr;
    constructors    :: NameMap (Lambda, Int);
    topLambdas      :: NameMap Lambda;
    topBindings     :: NameMap Var;
    clmLambdas      :: NameMap CLMLam;
    clmBindings     :: NameMap CLMVar;
    instanceLambdas :: NameMap Lambda;
    clmInstances    :: NameMap CLMLam;
    structInheritance :: NameMap [Name];
    reprMap         :: NameMap [(Name, Bool, Lambda, Lambda, Maybe Expr)];
    effectDecls     :: NameMap ([Var], [Lambda]);
    effectHandlers  :: NameMap (Name, [Expr]);
    classDecls      :: NameMap ClassMeta,        -- NEW: class hierarchy
    classTagCounter :: !Int,                     -- NEW: global class tag allocator
    outProgram      :: NameMap String;
    rawCLMLambdas   :: NameMap CLMLam;
    rawCLMInstances :: NameMap CLMLam
} deriving Show
```

### 5.3 Helper Functions

```haskell
-- Look up a class, walking the hierarchy
lookupClass :: Name -> Environment -> Maybe ClassMeta
lookupClass nm env = Map.lookup nm (classDecls env)

-- Check if className is a subtype of targetName
isSubclassOf :: Name -> Name -> Environment -> Bool
isSubclassOf className targetName env
    | className == targetName = True
    | otherwise = case lookupClass className env of
        Just cm -> case cmParent cm of
            Just parent -> isSubclassOf parent targetName env
            Nothing -> False
        Nothing -> False

-- Find method in class hierarchy (always pre-merged in cmMethods, but
-- this walks up for super calls)
lookupParentMethod :: Name -> Name -> Environment -> Maybe CLMLam
lookupParentMethod className methodName env =
    case lookupClass className env >>= cmParent of
        Just parentName -> case lookupClass parentName env of
            Just parentMeta -> Map.lookup methodName (cmMethods parentMeta)
            Nothing -> Nothing
        Nothing -> Nothing

-- Resolve a dot-access: is it a field or a method?
data DotResolution = DotField Int | DotMethod | DotStatic | DotNotFound

resolveDot :: Name -> Name -> Environment -> DotResolution
resolveDot className memberName env = case lookupClass className env of
    Just cm
        | Just idx <- Map.lookup memberName (cmFieldIndices cm) -> DotField idx
        | Map.member memberName (cmMethods cm)                  -> DotMethod
        | Map.member memberName (cmStaticMethods cm)            -> DotStatic
    _ -> DotNotFound

-- Allocate a unique class tag
allocClassTag :: Environment -> (Int, Environment)
allocClassTag env =
    let tag = classTagCounter env
    in (tag, env { classTagCounter = tag + 1 })
```

---

## 6. CLM Representation

### 6.1 New CLM Nodes

```haskell
data CLMExpr =
    ...
  | CLMMCALL CLMExpr Name [CLMExpr]   -- object.method(args)  — dynamic dispatch
  | CLMSCALL CLMExpr Name [CLMExpr]   -- super.method(args)   — static parent dispatch
  | CLMNEW Name [CLMExpr]             -- ClassName.new(args)   — construction
    deriving (Show, Eq)
```

**`CLMMCALL obj methodName args`**: Dynamic method dispatch.
- Interpreter: evaluate `obj` → get ConsTag class name → look up method in `cmMethods` → apply with `(obj : args)`
- Codegen: `obj.methodName(args)` on all targets

**`CLMSCALL obj methodName args`**: Super/parent dispatch.
- Interpreter: get ConsTag class name → look up parent → find method in parent's `cmMethods` → apply
- Codegen: `base.method()` (.NET), `super.method()` (JS), `Base::method()` (C++)

**`CLMNEW className args`**: Object construction.
- Interpreter: look up `ClassMeta` → validate not abstract → build `CLMCON` with tag + all fields (inherited + own)
- Codegen: `new ClassName(args)` on all targets

### 6.2 Object Representation in CLM

A tulam-native object is a plain `CLMCON` — no vtable, no hidden fields:

```
Dog.new("Rex", 3, "Lab")
→ CLMNEW "Dog" [CLMLIT (LString "Rex"), CLMLIT (LInt 3), CLMLIT (LString "Lab")]
→ (after eval) CLMCON (ConsTag "Dog" 101) [CLMLIT "Rex", CLMLIT 3, CLMLIT "Lab"]
```

Field layout: parent fields first, own fields last, in declaration order:
```
Dog object: [name(0), age(1), breed(2)]
                ^Animal fields^  ^Dog^
```

### 6.3 CLM Conversion Rules (Pass 4)

In `exprToCLM`:

```
-- Construction: Dog.new(args)
App (RecFieldAccess ("new", -1) (Id "Dog")) args
    where "Dog" ∈ classDecls
  → CLMNEW "Dog" (map (exprToCLM env) args)

-- Field access: d.name
RecFieldAccess ("name", -1) d
    where d's type ∈ classDecls, "name" ∈ cmFieldIndices
  → CLMFieldAccess ("name", idx) (exprToCLM env d)

-- Method call: d.speak()
App (RecFieldAccess ("speak", -1) d) args
    where d's type ∈ classDecls, "speak" ∈ cmMethods
  → CLMMCALL (exprToCLM env d) "speak" (map (exprToCLM env) args)

-- Static method call: MathUtils.lerp(a, b, t)
App (RecFieldAccess ("lerp", -1) (Id "MathUtils")) args
    where "MathUtils" ∈ classDecls, "lerp" ∈ cmStaticMethods
  → CLMAPP (CLMID "MathUtils$lerp") (map (exprToCLM env) args)

-- Super call: super.methodName(args)
App (RecFieldAccess ("methodName", -1) (Id "super")) args
    inside a class method for class C
  → CLMSCALL (CLMID "self") "methodName" (map (exprToCLM env) args)

-- Downcast: a as Dog
ReprCast expr (Id "Dog")
    where "Dog" ∈ classDecls
  → CLMDOWNCAST (exprToCLM env expr) "Dog"
    -- (or desugar to pattern check + Maybe wrapping, see Section 6.4)

-- Upcast: implicit, no CLM change
-- When type checker sees Dog where Animal expected, it's a no-op
-- because CLMCON "Dog" already has Animal's fields at the same indices
```

### 6.4 Downcast Desugaring

Rather than a new CLM node, downcasts can desugar using existing infrastructure:

```
a as Dog
→ let __obj = eval(a) in
  if classTag(__obj) ∈ {Dog's tag, Dog's subclass tags}
  then Just(__obj)
  else Nothing
```

In CLM terms, this becomes a lambda that checks the ConsTag against the class hierarchy. The set of valid tags (class + all subclasses) is computed at compile time from `cmChildren` (transitively).

---

## 7. Pipeline Integration

### 7.1 Pass 0: Parsing & Desugaring

**Parser** (`pClassDecl`):
```
class_decl ::= [abstract | sealed] "class" Name "(" fields ")"
               ["extends" Name ["(" super_args ")"]]
               ["implements" name_list]
               "=" "{" method_decls "}" ";"
```

Keywords to reserve in Lexer.hs: `class`, `abstract`, `sealed`, `implements`, `override`, `final`, `static`, `super`.

**Desugaring**: Minimal at Pass 0. The `ClassDecl` node survives intact into Pass 1.

### 7.2 Pass 1: Environment Building

New case in `processBinding`:

```haskell
processBinding (ClassDecl lam cinfo, si) env = do
    -- 1. Validate
    validateClassDecl lam cinfo env si

    -- 2. Resolve parent
    (parentFields, parentMethods, parentTag) <- case classParent cinfo of
        Nothing -> pure ([], Map.empty, Nothing)
        Just (parentName, _superArgs) -> case lookupClass parentName env of
            Just pm -> pure (cmAllFields pm, cmMethods pm, Just parentName)
            Nothing -> do
                logError (mkLogPayload si ("Parent class " ++ parentName ++ " not found"))
                pure ([], Map.empty, Nothing)

    -- 3. Build field list (inherited + own)
    let ownFields = params lam
    let allFields = parentFields ++ ownFields
    let fieldIndices = Map.fromList (zip (map name allFields) [0..])

    -- 4. Allocate class tag
    let (tag, env1) = allocClassTag env

    -- 5. Convert own methods to CLM, merge with inherited
    let ownMethods = extractMethods (body lam)  -- DeclBlock → [(Name, Lambda)]
    ownCLMMethods <- mapM (lambdaToCLMLambdaM env1) ownMethods
    let mergedMethods = Map.union (Map.fromList ownCLMMethods) parentMethods
        -- union prefers left → own methods override parent

    -- 6. Validate overrides
    forM_ ownMethods $ \(mname, mlam) ->
        when (lamMod mlam == MOverride && not (Map.member mname parentMethods)) $
            logWarn (mkLogPayload si ("override on " ++ mname ++ " but no parent method found"))

    -- 7. Build ClassMeta
    let cm = ClassMeta
            { cmParent        = fst <$> classParent cinfo
            , cmAllFields     = allFields
            , cmOwnFields     = ownFields
            , cmMethods       = mergedMethods
            , cmOwnMethods    = Map.fromList ownMethods
            , cmStaticMethods = Map.fromList (extractStaticMethods (body lam))
            , cmFieldIndices  = fieldIndices
            , cmModifier      = classModifier cinfo
            , cmChildren      = []
            , cmImplements    = map exprToName (classImplements cinfo)
            , cmSuperArgs     = maybe [] snd (classParent cinfo)
            , cmExtern        = classExtern cinfo
            , cmTag           = tag
            }

    -- 8. Register in environment
    let env2 = env1 { classDecls = Map.insert (lamName lam) cm (classDecls env1) }

    -- 9. Update parent's children list
    let env3 = case cmParent cm of
            Just pn -> env2 { classDecls = Map.adjust (\p -> p { cmChildren = lamName lam : cmChildren p }) pn (classDecls env2) }
            Nothing -> env2

    -- 10. Register constructor in constructors map (for CLMCON compatibility)
    let consLam = Lambda (lamName lam) allFields (Tuple (map (\v -> Id (name v)) allFields)) (Id (lamName lam)) si MNone
    let env4 = addNamedConstructor (lamName lam) (consLam, tag) env3

    -- 11. Register type
    let env5 = env4 { types = Map.insert (lamName lam) (ClassDecl lam cinfo) (types env5) }

    -- 12. Generate algebra instances from `implements`
    env6 <- foldM (generateAlgebraInstance (lamName lam) ownMethods) env5 (classImplements cinfo)

    pure env6
```

### 7.3 Pass 2: Case Optimization

Class methods stored in `cmMethods` are CLMLam values. If they contain pattern matches, they need case optimization like any other lambda. Pass 2 should iterate over `classDecls` in addition to `topLambdas` and `instanceLambdas`:

```haskell
-- In caseOptimizationPass:
let classMethodLams = concatMap (Map.toList . cmMethods) (Map.elems (classDecls env))
-- optimize each, update classDecls
```

### 7.4 Pass 3: Type Checking

New type checker features for classes:

**Subtype judgments**: When checking `f(x)` where `f : Animal -> T` and `x : Dog`:
```haskell
-- In unify or check:
unify expected actual = ...
    | isClassType expected && isClassType actual ->
        if isSubclassOf (tyName actual) (tyName expected) env
        then pure ()  -- upcast is valid
        else mismatch expected actual
```

**Sealed exhaustiveness**: When pattern matching on a sealed class, verify all direct subclasses are covered:
```haskell
checkExhaustive sealedClassName cases = do
    cm <- lookupClass sealedClassName
    let children = cmChildren cm  -- only direct, not transitive (each level is sealed independently)
    let covered = extractMatchedClassNames cases
    let missing = children \\ covered
    unless (null missing) $
        tcWarn ("Non-exhaustive match on sealed " ++ sealedClassName ++ ", missing: " ++ show missing)
```

**Method type checking**: Verify override signatures are compatible (covariant return, contravariant params — or exact match for simplicity in v1).

### 7.5 Pass 4: CLM Conversion

See Section 6.3 for the conversion rules. The key change is in `exprToCLM` for `App (RecFieldAccess ...)` — we check `classDecls` before falling through to record field access.

The resolution requires knowing the type of the receiver expression. For the initial implementation, we can use a simple heuristic:
1. If receiver is `Id name` → look up name's type from bindings/lambdas
2. If receiver is a construction → type is known
3. Otherwise → defer to interpreter (emit `CLMFieldAccess` with -1, interpreter resolves)

For a more robust solution, Pass 3 (type checker) annotates expressions with types, and Pass 4 reads those annotations.

### 7.6 Pass 4.5: CLM Optimization

No special handling needed. `CLMMCALL`, `CLMSCALL`, `CLMNEW` are treated as opaque by the optimizer (like `CLMIAP`). Future optimization: devirtualization — when the concrete type is known statically, replace `CLMMCALL` with direct `CLMAPP` of the known method.

---

## 8. Interpreter

### 8.1 CLMNEW — Object Construction

```haskell
evalCLM i (CLMNEW className argExprs) = do
    args <- mapM (evalCLM (i+1)) argExprs
    env <- gets currentEnvironment
    case lookupClass className env of
        Just cm
            | cmModifier cm == ClassAbstract ->
                pure $ CLMERR ("[RT] cannot instantiate abstract class " ++ className) si
            | length args /= length (cmAllFields cm) ->
                pure $ CLMERR ("[RT] wrong number of args for " ++ className ++ ".new") si
            | otherwise ->
                pure $ CLMCON (ConsTag className (cmTag cm)) args
        Nothing ->
            pure $ CLMERR ("[RT] class " ++ className ++ " not found") si
```

### 8.2 CLMMCALL — Dynamic Method Dispatch

```haskell
evalCLM i (CLMMCALL objExpr methodName argExprs) = do
    obj <- evalCLM (i+1) objExpr
    args <- mapM (evalCLM (i+1)) argExprs
    case obj of
        CLMCON (ConsTag className _) fields -> do
            env <- gets currentEnvironment
            case lookupClass className env of
                Just cm -> case Map.lookup methodName (cmMethods cm) of
                    Just methodLam ->
                        -- Apply method: self is the object, then extra args
                        evalCLM (i+1) $ applyCLMLam methodLam (obj : args)
                    Nothing ->
                        pure $ CLMERR ("[RT] no method " ++ methodName ++ " on " ++ className) si
                Nothing ->
                    pure $ CLMERR ("[RT] " ++ className ++ " is not a class") si
        CLMERR _ _ -> pure obj  -- propagate errors
        _ -> pure $ CLMERR ("[RT] method call on non-object: " ++ ppr obj) si
```

### 8.3 CLMSCALL — Super Dispatch

```haskell
evalCLM i (CLMSCALL objExpr methodName argExprs) = do
    obj <- evalCLM (i+1) objExpr
    args <- mapM (evalCLM (i+1)) argExprs
    case obj of
        CLMCON (ConsTag className _) _ -> do
            env <- gets currentEnvironment
            case lookupParentMethod className methodName env of
                Just parentLam ->
                    evalCLM (i+1) $ applyCLMLam parentLam (obj : args)
                Nothing ->
                    pure $ CLMERR ("[RT] no parent method " ++ methodName ++ " for " ++ className) si
        _ -> pure $ CLMERR ("[RT] super call on non-object") si
```

### 8.4 CLMIAP Dispatch Chain Update

Add class method dispatch to the existing chain, AFTER reflection intrinsics and BEFORE type inference:

```
IO intrinsics → Ref intrinsics → MutArray intrinsics →
Reflection intrinsics → Array HOFs →
**Class method dispatch** ← NEW (for bare method calls like speak(d))
→ type inference → instance dispatch
```

This handles the case where a class method is called in function-call syntax rather than dot syntax: `speak(d)` instead of `d.speak()`. The dispatcher checks if `funcName` is a method name in any class, infers the class from the first argument's ConsTag, and dispatches.

---

## 9. Codegen Mapping

### 9.1 .NET

```
tulam                          .NET (C#)
─────────────────────────────────────────────
class Animal(...)              public class Animal
  extends X                      : X
  implements Eq                  , IEquatable<Animal>
  abstract                       abstract
  sealed                         (sealed on leaf subclasses)
  function speak(self)           public virtual string Speak()
  override function speak        public override string Speak()
  final function info            public sealed override string Info()
  static function lerp           public static float Lerp(...)
  field name:String              public string Name { get; }
  Dog.new(args)                  new Dog(args)
  d.speak()                      d.Speak()
  super.OnLoad(e)                base.OnLoad(e)
  a as Dog                       a is Dog d ? new Maybe<Dog>(d) : Nothing
```

### 9.2 JavaScript

```
tulam                          JavaScript (ES6+)
─────────────────────────────────────────────
class Animal(...)              class Animal
  extends X                      extends X
  implements Eq                  (structural / Symbol-based)
  function speak(self)           speak() { ... }
  override function speak        speak() { ... }  (implicit override)
  static function lerp           static lerp(...) { ... }
  field name:String              constructor(name) { this.name = name; }
  Dog.new(args)                  new Dog(args)
  d.speak()                      d.speak()
  super.OnLoad(e)                super.onLoad(e)
  a as Dog                       a instanceof Dog ? Just(a) : Nothing
```

### 9.3 C++

```
tulam                          C++
─────────────────────────────────────────────
class Animal(...)              class Animal
  extends X                      : public X
  implements Eq                  (concept / CRTP / virtual interface)
  abstract                       (= 0 on methods)
  function speak(self)           virtual std::string speak() const
  override function speak        std::string speak() const override
  final function info            std::string info() const final
  static function lerp           static float lerp(...)
  field name:String              std::string name_;
  Dog.new(args)                  std::make_unique<Dog>(args)
  d.speak()                      d->speak()  or  d.speak()
  super.OnLoad(e)                Base::onLoad(e)
  a as Dog                       dynamic_cast<Dog*>(&a) (wrapped in optional)
```

### 9.4 CLM → Target Translation Table

| CLM Node | .NET | JS | C++ |
|----------|------|-----|-----|
| `CLMNEW "Dog" [a,b,c]` | `new Dog(a,b,c)` | `new Dog(a,b,c)` | `make_unique<Dog>(a,b,c)` |
| `CLMMCALL obj "speak" []` | `obj.Speak()` | `obj.speak()` | `obj->speak()` |
| `CLMSCALL obj "onLoad" [e]` | `base.OnLoad(e)` | `super.onLoad(e)` | `Base::onLoad(e)` |
| `CLMCON "Dog" 101 [...]` | N/A (objects live on heap) | N/A | N/A |
| `CLMFieldAccess "name" obj` | `obj.Name` | `obj.name` | `obj->name_` |

`CLMCON` is the **interpreter-only** representation. Codegen never emits CLMCON for class objects — it emits actual construction calls.

---

## 10. Interaction with Existing Features

### 10.1 Classes vs. Records

Records remain for pure data. Classes add behavior + inheritance.

```tulam
record Point = Point(x:Float64, y:Float64);      // pure data, no methods
class Shape(color:String) = { function area... };  // data + behavior
```

A record can be promoted to a class by changing `record` to `class` and adding methods. No other syntax changes needed (field declarations are the same).

### 10.2 Classes vs. Sum Types

Sum types are closed discriminated unions. Classes are open hierarchies.

```tulam
type Maybe(a) = Just(a) | Nothing;   // closed — exhaustive matching guaranteed
class Animal(...) = { ... };          // open — new subclasses can be added anytime
sealed class Result(a) = {};          // middle ground — closed hierarchy with inheritance
```

### 10.3 Classes vs. Algebras

Algebras provide ad-hoc polymorphism (typeclasses). Classes provide subtype polymorphism. They are orthogonal and composable:

```tulam
algebra Printable(a:Type) = {
    function print(x:a) : String
};

class Animal(name:String) = {
    function speak(self:Animal) : String
};

// A class can implement an algebra
instance Printable(Animal) = {
    function print(x:Animal) : String = x.speak()
};

// Or via `implements` shorthand
class Dog(...) extends Animal implements Printable = {
    override function speak(self:Dog) : String = "Woof!";
    function print(self:Dog) : String = self.speak()
};
```

### 10.4 Classes and Effects

Mutation in classes goes through the effect system:

```tulam
class Counter(countRef:Ref(Int)) = {
    function increment(self:Counter) : Unit = action {
        modifyRef(self.countRef, \n -> n + 1)
    };
    function get(self:Counter) : Int = readRef(self.countRef)
};

let c = Counter.new(newRef(0));
c.increment();
c.get()   // 1
```

No new mutability model. Fields themselves are immutable (the `Ref` is a value). Mutation happens through the `Ref` API + effect system.

### 10.5 Classes and Pattern Matching

You can pattern match on class objects to extract fields:

```tulam
function getName(a:Animal) : String = match a
    | Animal(name, _) -> name;
```

For sealed classes, matching is exhaustive:

```tulam
function eval(e:Expr) : Int = match e
    | Lit(n) -> n
    | Add(l, r) -> eval(l) + eval(r)
    | Mul(l, r) -> eval(l) * eval(r);
// Compiler checks: all subclasses of sealed Expr covered
```

For non-sealed classes, matching is **not** exhaustive — a wildcard `_` is required.

Type-test matching (downcast in pattern position):

```tulam
function describeAnimal(a:Animal) : String = match a
    | Dog(_, _, breed) -> "Dog: " ++ breed
    | Cat() -> "Cat"
    | _ -> "Unknown animal";
```

This uses the ConsTag to distinguish subclasses at runtime — same mechanism as sum type matching.

---

## 11. Implementation Phases

### Phase 1: Core Class System (Milestone 1)

**Goal**: tulam-native classes with single inheritance, basic method dispatch.

1. **Lexer**: Reserve `class`, `abstract`, `sealed`, `implements`, `override`, `final`, `static`, `super`
2. **Surface.hs**: Add `ClassDecl Lambda ClassInfo`, `ClassInfo`, `ClassModifier`, update `Lambda` with `lamMod` (defaulting to `MNone`)
3. **Parser.hs**: `pClassDecl` production — fields, methods, extends, implements
4. **State.hs**: `ClassMeta`, `classDecls` + `classTagCounter` in `Environment`, helper functions
5. **Pipeline.hs Pass 1**: `processBinding` for `ClassDecl` — build `ClassMeta`, register constructor, merge methods
6. **CLM.hs**: Add `CLMMCALL`, `CLMSCALL`, `CLMNEW` nodes + pretty printing + traversals
7. **Pipeline.hs Pass 4**: `exprToCLM` rules for class construction, method calls, field access, super calls
8. **Interpreter.hs**: `evalCLM` cases for `CLMMCALL`, `CLMSCALL`, `CLMNEW`
9. **Test file**: `tests/programs/P20_Classes.tl` — basic class, inheritance, method dispatch, field access
10. **CLM optimization**: Update `descendCLM` and `traverseCLMExpr` for new nodes

### Phase 2: Sealed Classes + Pattern Matching (Milestone 2)

1. Sealed class validation in Pass 1 (all subclasses in same module)
2. Exhaustive match checking for sealed hierarchies in type checker
3. Type-test patterns in match expressions
4. Downcast via `as` → `Maybe` (extend `ReprCast` resolution)
5. Tests: `tests/programs/P21_SealedClasses.tl`

### Phase 3: Algebra Integration (Milestone 3)

1. `implements` clause processing — extract algebra method bodies, generate instances
2. Validate that all required algebra methods are provided
3. Allow classes in `instance` declarations: `instance Eq(Dog) = derive;`
4. Tests: `tests/programs/P22_ClassAlgebras.tl`

### Phase 4: Extern Class Foundation (Milestone 4)

1. Metadata resolver stub — hardcoded `ClassMeta` entries for testing
2. `extern class` syntax in parser
3. Extern classes populate `classDecls` (same structure as tulam classes)
4. Field access on extern objects → `CLMFieldAccess` (deferred to codegen)
5. Method calls on extern objects → `CLMMCALL` (deferred to codegen)
6. Interpreter stubs for extern (limited REPL testing)
7. Tests with mock extern classes

### Phase 5: Subclassing Extern Classes (Milestone 5)

1. `class MyButton extends Button(args)` — super constructor args
2. Mixed field layout: extern parent fields are opaque, own fields are managed
3. Override validation against extern method signatures
4. Super calls across tulam/extern boundary
5. Property access on extern objects (getter/setter desugaring)

### Phase 6: .NET Codegen (Milestone 6)

1. Class → C# class emission
2. Method → virtual/override/sealed method emission
3. Field → property emission
4. Constructor → constructor with base() call
5. CLMMCALL → virtual call
6. CLMSCALL → base.method() call
7. Upcast/downcast → C# cast expressions
8. `implements` → interface implementation

### Phase 7: JS Codegen (Milestone 7)

1. Class → ES6 class emission
2. Method → method definition
3. Field → constructor assignment
4. Constructor → constructor with super() call
5. CLMMCALL → method call
6. CLMSCALL → super.method() call

### Phase 8: C++ Codegen (Milestone 8)

1. Class → C++ class emission with virtual methods
2. Memory management strategy (unique_ptr / shared_ptr / GC)
3. Constructor → initializer list
4. CLMMCALL → virtual call
5. Downcast → dynamic_cast wrapped in optional

---

## 12. Open Questions

1. **Multiple interfaces on extern classes**: .NET classes can implement many interfaces. Should all interfaces auto-generate algebra instances, or only explicitly mapped ones?

2. **Generic classes**: `class Container(a:Type)(items:List(a)) = { ... }` — type parameters need to thread through the class hierarchy. Deferred to after basic classes work.

3. **Visibility**: Should class fields/methods support `private`? Initial recommendation: all public. Add `private` keyword later if needed (maps to target's private/protected).

4. **Equality and identity**: Should `==` on class objects compare by value (fields) or by identity (reference)? Recommendation: no default `==`; require explicit `implements Eq` with user-provided implementation or `derive`.

5. **Nested classes**: `class Outer = { class Inner = { ... } }`. Useful for .NET interop (many nested types). Defer to later.

6. **Companion objects / class-level state**: Kotlin has `companion object`, Scala has `object`. Can we model this with static methods + module-level state? Probably yes — defer.

---

## 13. Relation to Other Design Documents

| Document | Relationship |
|----------|-------------|
| `InteropDesign.md` | Classes replace the ad-hoc `extern class` concept with a unified model. Sections 3-5 of InteropDesign are superseded by this design. Import/metadata resolver (Section 2), type mapping (Section 6), and codegen strategy (Section 11) remain valid and complementary. |
| `CategoricalDesign.md` | Algebras/morphisms remain the abstraction mechanism. Classes provide the concrete types that implement algebras. `implements` bridges the two. |
| `RecordDesign.md` | Records remain for pure data types. Classes extend records with methods + inheritance. Implementation can reuse record field access machinery. |
| `PrimitiveDesign.md` | Primitive types stay separate (no inheritance). But primitive-backed classes could exist (e.g., a class wrapping an Int with methods). |
| `EffectDesign.md` | Effects handle mutation in class methods. No interaction with class dispatch. |
| `ImplementationPlan.md` | Class system phases should be inserted as Phase 11 (after current Phase 10.4 CLM optimization). |
