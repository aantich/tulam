# Effect System Design: Row-Polymorphic Effects

## 1. Design Philosophy

tulam's effect system tracks side effects using **row-polymorphic effect types** — the same row polymorphism machinery already used for records. This means:

- **Zero runtime overhead**: Effect rows are erased at CLM conversion. Runtime code is just function calls.
- **Minimal new type machinery**: The existing `Row` type (`RExtend`, `REmpty`, `RVar`, `RRigid`), `rowExtract`, `unifyRows`, and `rowSubst` all transfer directly.
- **Composable by default**: Row unification handles effect merging automatically — no manual lifting or transformer stacking.
- **Inferred by default**: The compiler infers effect rows from function bodies. Explicit annotations are optional.
- **Granular**: Each side-effecting operation carries a specific effect label, not a monolithic `IO`.

### Why Not Other Approaches?

| Approach | Why Not |
|----------|---------|
| Haskell-style `IO` monad | Coarse-grained (tells you nothing about what effects occur), monad transformer hell |
| Algebraic effects + handlers | Requires delimited continuations or CPS transform — high implementation complexity |
| Graded/indexed monads | Requires type-level set algebra (union, intersection) — adds complexity without leveraging existing infra |
| Linear/uniqueness types | Requires usage tracking, multiplicity inference — significant type checker extension orthogonal to effect tracking |
| Capability-passing | Needs explicit capability algebras — more boilerplate than auto-inferred effect rows |

Row-polymorphic effects are proven in production (PureScript's `Eff`/`Aff`, Koka's effect system) and are the natural extension of tulam's existing type checker.

---

## 2. Effect Declarations

Effects are named sets of side-effecting operations. They are distinct from algebras: **algebras describe pure structure** (Eq, Ord, Monoid), **effects describe impure operations** (reading files, printing to console, generating random numbers).

### Syntax

```tulam
effect Console = {
    function readLine() : String;
    function putStrLn(s:String) : Unit;
    function putStr(s:String) : Unit
};

effect FileIO = {
    function readFile(path:String) : String;
    function writeFile(path:String, content:String) : Unit;
    function appendFile(path:String, content:String) : Unit;
    function fileExists(path:String) : Bool
};

effect Timer = {
    function sleep(ms:Int) : Unit;
    function now() : Int
};
```

### Parameterized Effects

Effects can take type parameters:

```tulam
effect State(s:Type) = {
    function get() : s;
    function put(x:s) : Unit;
    function modify(f: s -> s) : Unit
};

effect Exception(e:Type) = {
    function throw(err:e) : a
};

effect Reader(r:Type) = {
    function ask() : r;
    function local(f: r -> r, body: Eff { reader: Reader(r) | e } a) : Eff { reader: Reader(r) | e } a
};
```

### Semantics

An effect declaration:
1. Registers a new effect name in the environment (`effectDecls` map)
2. Makes the effect's operations available as functions (resolved during type checking)
3. Each operation implicitly carries the effect label in its return type

---

## 3. Effect Row Types

Effect rows describe **which side effects a computation may perform**. They reuse the existing `Row` type from the record system.

### Type Syntax

```tulam
// Closed effect row — exactly these effects, no others
Eff { console: Console, fileio: FileIO } Unit

// Open effect row — these effects plus possibly more
Eff { console: Console | r } Unit

// Pure — no effects (equivalent to a plain value type)
Eff {} Unit

// Multiple parameterized effects
Eff { state: State(Int), exception: Exception(String) | r } Bool
```

### Internal Representation

In the type checker (`src/TypeCheck.hs`):

```haskell
data Ty = ...
    | TEffect Row Ty    -- Effect type: row of effects + result type
```

The `Row` type is unchanged from record rows:

```haskell
data Row
  = REmpty                   -- No effects (pure)
  | RExtend Name Ty Row      -- Effect label + effect type + rest
  | RVar TyVar               -- Open effect variable
  | RRigid Name              -- Rigid effect variable (from quantification)
```

Example: `Eff { console: Console, fileio: FileIO | r } String` becomes:
```haskell
TEffect (RExtend "console" (TCon "Console") (RExtend "fileio" (TCon "FileIO") (RVar freshVar))) (TCon "String")
```

### Pure Functions

Functions without effects have no `TEffect` wrapper — they return plain types:

```tulam
function add(x:Int, y:Int) : Int = x + y;
// Type: Int -> Int -> Int (no TEffect)
```

A function returning `Eff {} a` is equivalent to one returning `a`. The type checker treats them as interchangeable.

---

## 4. Function Signatures with Effects

### Explicit Annotation

```tulam
function greet() : Eff { console: Console | r } Unit = action {
    name <- readLine();
    putStrLn("Hello, " ++ name)
};

function readConfig(path:String) : Eff { fileio: FileIO | r } String =
    readFile(path);
```

### Effect Inference

When no explicit effect annotation is given, the compiler **infers the effect row** from the function body:

```tulam
// No effect annotation — compiler infers { console: Console }
function greet() = action {
    name <- readLine();
    putStrLn("Hello, " ++ name)
};
```

Inference works by:
1. Each effect operation (e.g., `readLine`) introduces its effect label into the current row
2. Composing operations merges effect rows via row unification
3. The final inferred row is the union of all effects in the body

### Polymorphic Effects

The `| r` row variable makes functions **effect-polymorphic** — they work in any effect context:

```tulam
// Works in any context that has at least Console
function greet() : Eff { console: Console | r } Unit = action {
    putStrLn("Hello!")
};

// Can be called from a context with more effects
function main() : Eff { console: Console, fileio: FileIO } Unit = action {
    greet(); // OK: { console | r } unifies with { console, fileio }
    config <- readFile("app.cfg");
    putStrLn(config)
};
```

---

## 5. Effect Composition

### Automatic Row Merging

When effectful computations are composed, their effect rows are **unified** (merged) using the existing row unification algorithm:

```tulam
function readConfig(path:String) : Eff { fileio: FileIO | r } String =
    readFile(path);

function showConfig() : Eff { console: Console, fileio: FileIO | r } Unit = action {
    config <- readConfig("app.cfg"); // contributes fileio
    putStrLn(config)                   // contributes console
};
```

The type checker performs:
1. `readConfig` has effect row `{ fileio: FileIO | r1 }`
2. `putStrLn` has effect row `{ console: Console | r2 }`
3. Row unification merges: `{ fileio: FileIO | r1 } ∪ { console: Console | r2 }` → `{ console: Console, fileio: FileIO | r3 }`
4. This is the same `rowExtract` + `unifyRows` algorithm used for record types

### Sequencing

Within an `action` block, each statement's effects are merged into the block's overall effect row:

```tulam
action doWork() : Eff { console: Console, fileio: FileIO, net: NetworkIO } Unit = {
    config <- readFile("config.txt"); // + fileio
    data <- httpGet("https://api.example.com"); // + net
    putStrLn("Got: " ++ data)                  // + console
};
```

### Subsumption

A computation with fewer effects can be used where more effects are expected:

```tulam
function pureGreet(name:String) : Eff { console: Console | r } Unit =
    putStrLn("Hello, " ++ name);

// OK: { console | r } is a subset of { console, fileio, net }
function main() : Eff { console: Console, fileio: FileIO, net: NetworkIO } Unit = action {
    pureGreet("Alice");
    data <- readFile("data.txt");
    putStrLn(data)
};
```

This works because open rows (`| r`) unify with any extension — the row variable absorbs the extra effects.

---

## 6. `action` Blocks

The `action` keyword provides do-notation for sequencing effectful operations. This corresponds to Phase 9 (Do-Notation) in `ImplementationPlan.md`.

### Syntax

```tulam
action functionName(params) : ReturnType = {
    name <- effectfulExpr; // monadic bind: run expr, bind result to name
    localName = pureExpr; // let-binding: pure computation
    effectfulExpr; // monadic sequence: run for side effect, discard result
    lastExpr                     // final expression: the return value
};
```

### Desugaring

Action blocks desugar to chains of `bind`, `seq`, and `let`:

```tulam
// Source:
action main() : Eff { console: Console } Unit = {
    name <- readLine();
    greeting = "Hello, " ++ name;
    putStrLn(greeting)
};

// Desugars to:
function main() : Eff { console: Console } Unit =
    bind(readLine(), \name ->
        let greeting = "Hello, " ++ name in
        bind(putStrLn(greeting), \_ -> pure(()))
    );
```

### Desugaring Rules

| Action Statement | Desugared Form |
|-----------------|----------------|
| `name <- expr, rest` | `bind(expr, \name -> rest)` |
| `name = expr, rest` | `let name = expr in rest` |
| `expr, rest` | `seq(expr, rest)` — which is `bind(expr, \_ -> rest)` |
| `expr` (last) | `expr` (or `pure(expr)` if pure) |

### Nested Actions

Action blocks can be nested:

```tulam
action main() = {
    result <- action {
        x <- readLine();
        y <- readLine();
        pure(x ++ " " ++ y)
    };
    putStrLn(result)
};
```

---

## 7. Effect Handlers

Handlers provide concrete implementations for effect operations. They **eliminate** an effect label from the row.

### v1: Implicit Default Handlers

For the initial implementation, built-in effects have **implicit default handlers** that execute directly:

```tulam
// Just works — Console operations execute via default handler (intrinsics)
action main() = {
    putStrLn("Hello, World!")
};
```

Built-in effects map to intrinsics:
- `Console.readLine` → Haskell `getLine`
- `Console.putStrLn` → Haskell `putStrLn`
- `FileIO.readFile` → Haskell `readFile`
- etc.

### v2: Explicit Handlers (Future)

For testing, mocking, and custom interpretations:

```tulam
// Handler declaration
handler StdConsole : Console = {
    function readLine() = intrinsic;
    function putStrLn(s) = intrinsic
};

handler MockConsole(inputs:List(String)) : Console = {
    state lines = inputs;
    function readLine() = action {
        current <- get();
        match current
            | Cons(h, t) -> action { put(t), pure(h) }
            | Nil -> pure("")
    };
    function putStrLn(s) = pure(())  // silently discard output
};

// Running with a handler
let result = handle greet() with MockConsole(["Alice"]);
// greet : Eff { console: Console | r } Unit
// result : Eff { | r } Unit  — console effect eliminated
```

### Handler Type Rule

```
handle : Eff { label: E | r } a → Handler(E) → Eff { | r } a
```

A handler removes exactly one effect label from the row. Multiple handlers can be composed:

```tulam
let result = handle (handle computation with StdConsole) with StdFileIO;
// Removes console, then fileio
```

When all effects are handled, `Eff {} a ≅ a` — the computation is pure.

---

## 8. Interop Integration

### Auto-Inferred Effect Labels

When importing extern types via `import ... target`, the compiler assigns effect labels based on the extern namespace:

```tulam
import System.IO target dotnet;
// File.ReadAllText : String -> Eff { fileio: FileIO } String
// File.WriteAllText : (String, String) -> Eff { fileio: FileIO } Unit

import System.Net.Http target dotnet;
// HttpClient.GetAsync : String -> Eff { net: NetworkIO } String

import { document } from globals target js;
// document.getElementById : String -> Eff { dom: DOM } Maybe(Element)

import { printf } from stdio target native;
// printf : (String, ...) -> Eff { stdio: StdIO } Int
```

### Effect Mapping Table

The compiler uses a configurable mapping from extern namespaces to effect labels:

| Extern Source | Effect Label | Effect Type |
|---|---|---|
| `System.IO.*` | `fileio` | `FileIO` |
| `System.Net.*` | `net` | `NetworkIO` |
| `System.Console` | `console` | `Console` |
| `System.Threading.*` | `async` | `Async` |
| JS `document.*`, DOM APIs | `dom` | `DOM` |
| JS `fetch`, `XMLHttpRequest` | `net` | `NetworkIO` |
| C `stdio.h` | `stdio` | `StdIO` |
| C `stdlib.h` (`malloc`, `free`) | `mem` | `Memory` |

Users can override or extend this mapping:

```tulam
// Project-level effect mapping configuration
effect_map {
    "MyCompany.Logging.*" -> logging: Logging;
    "MyCompany.Cache.*" -> cache: CacheIO
};
```

### Sandboxing

Effect rows integrate with the capability-denial system from `DependencyDesign.md`:

```tulam
import UntrustedLib with { deny NetworkIO };
// The type checker rejects any function from UntrustedLib
// whose inferred effect row contains net: NetworkIO
```

Implementation: When loading a module with `deny` restrictions, the type checker enforces that no exported function's effect row contains the denied effect labels. This is a row-level check — `rowExtract` for the denied label must fail (return `MissingField`).

### Action Blocks for Extern Mutation

The existing `action` blocks from `InteropDesign.md` Section 8.3 gain precise effect types:

```tulam
action setupUI() : Eff { gui: GUI } Unit = {
    let form = Form.new("My App");
    let btn = Button.new("Click me");
    btn.Location = Point.new(10, 10);
    form.Controls.Add(btn);
    form.ShowDialog()
};
```

---

## 9. Relationship to Existing Abstractions

### Effects vs Algebras

| Aspect | Algebra | Effect |
|--------|---------|--------|
| **Purpose** | Pure structure (Eq, Ord, Functor) | Side-effecting operations |
| **Keyword** | `algebra` / `structure` | `effect` |
| **Operations** | Pure functions | Impure operations |
| **Dispatch** | Instance resolution (implicit params) | Effect handler / intrinsic |
| **Type impact** | Constraints on type variables | Row labels in effect type |
| **Runtime** | Dictionary passing or inlining | Direct function calls (after erasure) |

### Effects vs Monads

Effects and Monads **coexist**:

```tulam
// Eff is a Monad (for each fixed effect row)
instance Monad(Eff(e)) = {
    function pure(x) = ...;
    function bind(m, f) = ...;
    function seq(m1, m2) = bind(m1, \_ -> m2)
};

// Pure monads (Maybe, List, Either) don't use effect rows
// They work exactly as they do now
function safeDiv(x:Int, y:Int) : Maybe(Int) =
    match y | 0 -> Nothing | _ -> Just(x / y);
```

**Effect rows replace monad transformers:**

```tulam
// Haskell: StateT s (ReaderT r IO) a — painful, requires lift
// tulam:   Eff { state: State(s), reader: Reader(r), console: Console } a — flat, composable
```

### Effects and the Categorical Hierarchy

From `CategoricalDesign.md`, the categorical structure is:

```
Algebra (single-type)  →  pure structure
Morphism (multi-type)  →  pure cross-type relationships
Effect                 →  impure operations (NEW)
```

Effects are orthogonal to the algebra/morphism hierarchy. An algebra can have effectful derived operations, but its core operations are pure:

```tulam
// Algebra: pure
algebra Printable(a:Type) = {
    function toString(x:a) : String
};

// Using algebra within an effect:
function printAll [a:Type] (xs:List(a)) : Eff { console: Console } Unit requires Printable(a) =
    action { forEach(\x -> putStrLn(toString(x)), xs) };
```

---

## 10. Standard Library Effects

Located in `lib/Effect/`:

### `lib/Effect/Console.tl`

```tulam
module Effect.Console;

effect Console = {
    function readLine() : String;
    function putStrLn(s:String) : Unit;
    function putStr(s:String) : Unit;
    function readChar() : Char
};
```

### `lib/Effect/FileIO.tl`

```tulam
module Effect.FileIO;

effect FileIO = {
    function readFile(path:String) : String;
    function writeFile(path:String, content:String) : Unit;
    function appendFile(path:String, content:String) : Unit;
    function fileExists(path:String) : Bool;
    function deleteFile(path:String) : Unit;
    function listDirectory(path:String) : List(String)
};
```

### `lib/Effect/State.tl`

```tulam
module Effect.State;

effect State(s:Type) = {
    function get() : s;
    function put(x:s) : Unit;
    function modify(f: s -> s) : Unit
};
```

### `lib/Effect/Exception.tl`

```tulam
module Effect.Exception;

effect Exception(e:Type) = {
    function throw(err:e) : a
};

// catch is a handler combinator, not an effect operation
function catch(body: Eff { exception: Exception(e) | r } a, handler: e -> Eff { | r } a)
    : Eff { | r } a = handle body with ExceptionHandler(handler);
```

### `lib/Effect/Random.tl`

```tulam
module Effect.Random;

effect Random = {
    function randomInt(lo:Int, hi:Int) : Int;
    function randomFloat() : Float64;
    function randomBool() : Bool
};
```

### `lib/Effect/NetworkIO.tl`

```tulam
module Effect.NetworkIO;

effect NetworkIO = {
    function httpGet(url:String) : String;
    function httpPost(url:String, body:String) : String
};
```

### `lib/Effect/Async.tl`

```tulam
module Effect.Async;

effect Async = {
    function fork(body: Eff { async: Async | r } a) : Task(a);
    function await(task:Task(a)) : a;
    function sleep(ms:Int) : Unit
};

// Task is an opaque type representing an asynchronous computation
primitive Task(a:Type);
```

---

## 11. Implementation Changes

### Type Checker (`src/TypeCheck.hs`)

**New Ty variant:**
```haskell
data Ty = ...
    | TEffect Row Ty    -- Effect type: Eff { effects } resultType
```

**Reused (no changes needed):**
- `Row` data type (`REmpty`, `RExtend`, `RVar`, `RRigid`)
- `rowExtract` — extract an effect label from the row
- `unifyRows` — merge effect rows during composition
- `rowSubst` — substitution map for row variables
- `applySubstRow` — apply substitutions with chain chasing
- `bindRow` — bind row variables with occurs check
- `freeRowVars` — collect free row variables for generalization

**New additions:**
- Effect inference in `infer` for action block expressions
- Effect row merging when composing `bind` chains
- `CEffect Name [Ty]` constraint variant (for explicit effect requirements)
- Subsumption check: `Eff { ... | r } a` subsumes `Eff { ... } a` when row variable absorbs extras

### Surface AST (`src/Surface.hs`)

```haskell
-- New AST nodes
| EffectDecl Name [(Name, [Expr], Expr)]  -- effect Name = { ops }
| ActionBlock [ActionStmt]                 -- action { stmts }
| HandleWith Expr Expr                     -- handle expr with handler

-- Action statement type
data ActionStmt
  = ActionBind Name Expr      -- name <- expr
  | ActionLet Name Expr       -- name = expr
  | ActionExpr Expr           -- expr (for side effects)
  deriving (Show, Eq)
```

### Parser (`src/Parser.hs`)

- `pEffectDecl`: Parse `effect Name = { function op(params) : Type, ... };`
- `pActionBlock`: Parse `action { stmt, stmt, ... }` with `<-` for bind and `=` for let
- `pEffType`: Parse `Eff { label: Type, ... | r } ResultType` in type positions
- `pHandleWith`: Parse `handle expr with handlerExpr`
- `pActionFunc`: Parse `action funcName(params) : Type = { ... };` as sugar

### Lexer (`src/Lexer.hs`)

- Add `effect` to reserved words
- Add `handler` to reserved words
- Add `handle` to reserved words
- `<-` operator for bind in action blocks
- `action` already reserved

### Pipeline (`src/Pipeline.hs`)

**New pass — Pass 0.5 (Action Desugaring):**
- Runs after parser, before environment building
- Transforms `ActionBlock [stmts]` → nested `bind`/`let` applications
- Transforms `action funcName(params) = { ... }` → `function funcName(params) = desugared`

**Pass 1 (Environment Building):**
- Register effect declarations in `effectDecls` map
- Register effect operations as available functions (with effect-tagged types)

**Pass 3 (Type Checking):**
- Infer effect rows for functions containing effect operations
- Unify effect rows when composing effectful expressions
- Check explicit effect annotations against inferred rows

**Pass 4 (CLM Conversion):**
- Erase `TEffect` wrappers — effect rows do not survive to runtime
- Effect operations map to intrinsic calls or handler dispatch

### State (`src/State.hs`)

```haskell
-- Add to Environment
effectDecls :: HashMap Name EffectDecl
effectHandlers :: HashMap Name HandlerDecl

-- Effect declaration data
data EffectDecl = EffectDecl
  { effectName :: Name
  , effectParams :: [Name]           -- type parameters
  , effectOps :: [(Name, [Expr], Expr)]  -- (opName, paramTypes, returnType)
  }

-- Handler declaration data
data HandlerDecl = HandlerDecl
  { handlerName :: Name
  , handlerEffect :: Name
  , handlerOps :: [(Name, Lambda)]   -- concrete implementations
  }
```

### CLM (`src/CLM.hs`) & Interpreter (`src/Interpreter.hs`)

- Minimal changes — effects are erased before CLM
- Built-in effect operations dispatched via existing intrinsic mechanism
- `CLMPRIMCALL` handles effect operation calls at runtime
- New intrinsic entries for Console, FileIO, etc. in `src/Intrinsics.hs`

---

## 12. Implementation Phases

### Phase A: Effect Declaration Parsing
- Add `effect` keyword to lexer
- Parse `effect Name = { ... }` declarations
- Store in `effectDecls` in environment
- No type checking yet — just parse and register

### Phase B: Action Block Desugaring (= Phase 9 from ImplementationPlan.md)
- Parse `action { stmt, stmt, ... }` with `<-` and `=`
- Desugar to `bind`/`let` chains
- Parse `action funcName(params) = { ... }` as function sugar
- This phase works independently of effect types — desugaring is purely syntactic

### Phase C: Effect Type Checking
- Add `TEffect Row Ty` to internal type representation
- Infer effect rows from function bodies
- Unify effect rows during composition (via existing `unifyRows`)
- Parse `Eff { ... } Type` in type annotations
- Subsumption checking for effect polymorphism

### Phase D: Effect Handlers
- Parse `handler Name : Effect = { ... }` declarations
- Implement handler application: `handle expr with handler`
- Effect elimination: handling removes the label from the row
- Built-in default handlers for Console, FileIO, etc.

### Phase E: Interop Effect Inference
- Auto-assign effect labels from extern metadata namespaces
- Configurable effect mapping table
- Sandboxing enforcement (deny effect labels at import boundaries)

### Phase F: Standard Library Effects
- Define `lib/Effect/` modules: Console, FileIO, State, Exception, Random, NetworkIO, Async
- Built-in intrinsic implementations for core effects
- Update `lib/Base.tl` to include Effect modules

---

## 13. Worked Examples

### Hello World

```tulam
action main() = {
    putStrLn("Hello, World!")
};
// Inferred type: Eff { console: Console } Unit
```

### Interactive Program

```tulam
action main() = {
    putStr("What is your name? ");
    name <- readLine();
    putStr("How old are you? ");
    ageStr <- readLine();
    putStrLn("Hello, " ++ name ++ "! You are " ++ ageStr ++ " years old.")
};
// Inferred type: Eff { console: Console } Unit
```

### File Processing

```tulam
action processFile(input:String, output:String) = {
    content <- readFile(input);
    let processed = toUpper(content);
    writeFile(output, processed);
    putStrLn("Processed " ++ input ++ " -> " ++ output)
};
// Inferred type: Eff { fileio: FileIO, console: Console } Unit
```

### Stateful Computation

```tulam
action counter(n:Int) : Eff { state: State(Int), console: Console } Unit = {
    match n
        | 0 -> pure(())
        | _ -> action {
            current <- get();
            put(current + 1);
            putStrLn("Count: " ++ toString(current + 1));
            counter(n - 1)
        }
};

// Run with initial state
let result = handle (counter 5) with StateHandler(0);
// result : Eff { console: Console } Unit  — state effect eliminated
```

### Testing with Mock Handler

```tulam
// Production handler
handler StdConsole : Console = {
    function readLine() = intrinsic;
    function putStrLn(s) = intrinsic;
    function putStr(s) = intrinsic
};

// Test handler — captures output, provides scripted input
handler TestConsole(inputs:List(String)) : Console = {
    state inputQueue = inputs;
    state outputLog = Nil;

    function readLine() = action {
        queue <- get();
        match queue
            | Cons(h, t) -> action { put(t), pure(h) }
            | Nil -> pure("")
    };
    function putStrLn(s) = action {
        log <- get();
        put(Cons(s, log))
    };
    function putStr(s) = action {
        log <- get();
        put(Cons(s, log))
    }
};

// Test
action testGreeting() = {
    let (result, state) = handle greet() with TestConsole(["Alice"]);
    assert(state.outputLog == ["Hello, Alice!"])
};
```

### Interop with Effects

```tulam
import System.IO target dotnet;

action readAndParse(path:String) : Eff { fileio: FileIO } List(Int) = {
    content <- File.ReadAllText(path); // .NET call, auto-tagged as fileio
    let lines = String.Split(content, "\n");
    pure(map(parseInt, lines))
};
```

---

## 14. Target-Specific Effect Handler Compilation

Effect handlers compile differently per backend target. The key principle: **effect declarations are pure contracts** (platform-agnostic), while **handler implementations are target-qualified** with actual platform calls in the method bodies. The compiler selects the appropriate handler for the compilation target.

### 14.1 The Pattern

Each effect has:
1. A **pure contract** (the `effect` declaration) — never inside a `target` block
2. A **pure tulam handler** (intrinsic-backed) — works in interpreter and as fallback on all targets
3. **Target-qualified handlers** — one per backend, calling that platform's actual API

```tulam
// 1. Pure contract (lib/Effect.tl)
effect Console = {
    function readLine() : String;
    function putStrLn(s:String) : Unit;
    function putStr(s:String) : Unit
};

// 2. Pure tulam handler (interpreter fallback)
handler StdConsole : Console = {
    function readLine() = intrinsic;
    function putStrLn(s) = intrinsic;
    function putStr(s) = intrinsic
};

// 3. Target-qualified handlers
target dotnet {
    import System target dotnet;
    handler StdConsole : Console = {
        function putStrLn(s) = System.Console.WriteLine(s);
        function putStr(s)   = System.Console.Write(s);
        function readLine()  = System.Console.ReadLine()
    };
};

target js {
    import { console, prompt } from globals target js;
    handler StdConsole : Console = {
        function putStrLn(s) = console.log(s);
        function putStr(s)   = process.stdout.write(s);
        function readLine()  = prompt("")
    };
};

target native {
    extern function tlm_print_string(s:String) : Unit target native;
    extern function tlm_print_newline() : Unit target native;
    extern function tlm_read_line() : String target native;
    handler StdConsole : Console = {
        function putStrLn(s) = action { tlm_print_string(s); tlm_print_newline() };
        function putStr(s)   = tlm_print_string(s);
        function readLine()  = tlm_read_line()
    };
};
```

### 14.2 Compilation Strategy

| Aspect | Interpreter | .NET | JS | Native (LLVM) |
|--------|-------------|------|----|---------------|
| **IO effects** | Haskell intrinsics (`dispatchIOIntrinsic`) | Direct CLR API calls | Direct JS API calls | Direct C extern function calls |
| **Custom effects** | Handler stack (`CLMHANDLE`) | Evidence passing (interface params) | Handler dict objects | Vtable struct pointers |
| **Effect erasure** | Effect rows erased at CLM conversion | Effect rows erased at IL emission | Effect rows erased at JS emission | Effect rows erased at LIR lowering |
| **Handler selection** | Runtime (handler stack) | Compile-time (target block) | Compile-time (target block) | Compile-time (target block) |

For **IO effects** (Console, FileIO, etc.), the target-qualified handler is resolved at compile time and the handler indirection is eliminated — the compiler **inlines the handler body** at the call site, emitting direct platform calls. No handler dict, no vtable, no wrapper functions, no overhead. This uses the same inline expansion mechanism as algebra intrinsics (see PrimitiveDesign.md §2.5).

For **custom effects** (State, Exception, user-defined), the handler is passed as evidence (dictionary/vtable/interface) and dispatch goes through it at runtime.

### 14.3 Native Backend Effect Compilation

On the native (LLVM) backend, effect handler compilation works as follows:

1. **Extern function declarations** (`extern function ... target native;`) register C-callable functions in the extern environment with their LLVM-level signatures
2. **Target-qualified handler bodies** are lowered as normal function calls — `tlm_print_string(s)` becomes `CLMAPP "tlm_print_string" [s]` in CLM, then `call void @tlm_print_string(ptr %s)` in LLVM IR
3. **No special IO dispatch in CLMToLIR** — the lowering module doesn't need to know about IO. It just emits `call` instructions for any function in the extern/function registry
4. **Runtime library** (`runtime/LLVM/tlm_runtime.cpp`) provides the C implementations linked at compile time

This means **adding a new IO operation** (e.g., `readFile`) requires:
- Declaring the effect operation in the `effect` declaration
- Adding a C function to `runtime/LLVM/tlm_runtime.cpp`
- Adding the handler body in `lib/Backend/LLVM/Native.tl` that calls the primitive op
- No changes to the compiler's Haskell code

**The same pattern applies to algebras.** Adding `(+)` for a new numeric type requires: declaring the algebra, adding the primitive op to the runtime (if needed), writing the target instance in `lib/Backend/LLVM/Native.tl`. See PrimitiveDesign.md §2.5 for the three-layer architecture (primitive ops → target stdlib → user code) that unifies algebra intrinsics and effect handler compilation.

### 14.4 Repr at Effect Boundaries

When effect handler parameters cross the tulam/platform boundary, `repr` conversions are automatically inserted by the compiler:

```tulam
target native {
    // String in tulam → const char* in C
    // Compiler auto-inserts toRepr(s) at the call boundary
    handler StdConsole : Console = {
        function putStrLn(s) = action { tlm_print_string(s); tlm_print_newline() };
        //                                              ^
        //                     compiler inserts: toRepr(s) if String repr exists for native
    };
};
```

For primitive types (`Int`, `String`, `Float64`), the repr is typically identity (zero-cost). For user-defined types, the repr's `toRepr`/`fromRepr` functions are called at the boundary. See `InteropDesign.md §9.4` for purity enforcement on repr.

### 14.5 Standard Library Target Files (Future)

The stdlib will include per-backend handler files, mirroring the Haskell source and runtime directory conventions:

```
src/Backends/
  LLVM/               -- Haskell compiler modules for LLVM backend
  DotNet/             -- (future) Haskell compiler modules for .NET backend
  JS/                 -- (future) Haskell compiler modules for JS backend

runtime/
  LLVM/               -- C++ runtime for LLVM backend (tlm_object.hpp, tlm_runtime.cpp)
  DotNet/             -- (future) .NET runtime support
  JS/                 -- (future) JS runtime support

lib/
  Effect.tl           -- Pure effect contracts (backend-agnostic)
  Backend/
    LLVM/
      Native.tl       -- target native { handler StdConsole, StdFileIO, ... + algebra instances }
    DotNet/
      Native.tl       -- target dotnet { handler StdConsole, ... + algebra instances }
    JS/
      Native.tl       -- target js { handler StdConsole, ... + algebra instances }
```

All three layers (Haskell compiler, C++ runtime, tulam stdlib) use the same `Backend/<Name>/` convention. The build system selects the appropriate backend files based on the compilation target. The interpreter uses the pure tulam handlers (intrinsic-backed) from `Effect.tl`.

---

## 15. Open Questions and Future Directions

### Effect Polymorphism and Higher-Order Functions

How do higher-order functions interact with effects?

```tulam
// map is pure — the mapping function should be pure too
function map(f: a -> b, xs:List(a)) : List(b) = ...;

// But what about effectful mapping?
function mapM(f: a -> Eff { | e } b, xs:List(a)) : Eff { | e } List(b) = ...;
// The effect row variable `e` is polymorphic over all effects
```

Decision: Provide both pure (`map`, `filter`, `fold`) and effectful (`mapM`, `filterM`, `foldM`) variants in the standard library. The `M` suffix follows Haskell convention.

### Effect Subtyping vs Effect Polymorphism

Two approaches to effect compatibility:

1. **Subtyping**: `Eff { console } a <: Eff { console, fileio } a` — fewer effects is a subtype of more effects
2. **Polymorphism**: `Eff { console | r } a` — row variable absorbs extra effects

tulam uses **polymorphism** (option 2) because it's consistent with row polymorphism for records and doesn't require a separate subtyping relation.

### Linear Effects (Future)

When linear types are added (future phase), they can complement effects for resource safety:

```tulam
// Linear effect for resources that must be closed
effect Resource(r:Type) = {
    function open(path:String) : !r; // returns linear handle
    function close(handle: !r) : Unit; // consumes linear handle
    function read(handle: &r) : String  // borrows handle (doesn't consume)
};
```

This is deferred — row-poly effects handle IO tracking, linear types handle resource lifecycle. They are independent concerns that compose.

### Async Effects

Async/await can be modeled as an effect:

```tulam
action fetchAll(urls:List(String)) : Eff { async: Async, net: NetworkIO } List(String) = {
    tasks <- mapM(\url -> fork(httpGet(url)), urls); // fork = Async effect
    mapM(await, tasks)                                 // await = Async effect
};
```

The exact semantics of `fork`/`await` (green threads? OS threads? event loop?) are determined by the handler and the compilation target.

---

## 16. Relation to Other Design Documents

| Document | Relationship |
|----------|-------------|
| `CategoricalDesign.md` | Effects are orthogonal to algebras/morphisms. `action` keyword from Section 9 becomes effect-typed do-notation. |
| `ImplementationPlan.md` | Phase 9 (Do-Notation) becomes Phase B of the effect system. New phases A, C-F extend the plan. |
| `PrimitiveDesign.md` | Intrinsics provide the built-in implementations for effect operations (Console, FileIO). |
| `InteropDesign.md` | Extern operations auto-tagged with effect labels from metadata. Sandboxing uses effect row restriction. Section 8.3 action blocks gain effect types. **Section 9: Method purity classification** — determines which extern methods can appear in algebra instances vs effect handlers. |
| `InteropPattern.md` | Purity-aware interop section describes how the three-part pattern (algebra + repr + target instance) respects purity: pure observations → algebras, effectful operations → handlers, repr bridges pure-to-pure only. |
| `DependencyDesign.md` | Capability-based security maps to effect label denial at import boundaries. |
| `RecordDesign.md` | Row polymorphism is shared infrastructure — same `Row` type, same unification algorithm. |
