# Bytecode VM Design

## Overview

The tulam bytecode VM replaces the tree-walking CLM interpreter with a register-based
bytecode machine that compiles CLM expressions to a flat instruction stream. This
provides 10-100x speedup over tree-walking while maintaining full feature parity.

## Architecture

```
Source -> Parser -> Surface AST -> Pass 0-3 -> Monomorphize -> CLM -> Bytecode Compiler -> BytecodeModule
                                                                  \-> CLMToLIR -> LIR -> LLVM IR (native)
```

Both the bytecode backend and the LLVM native backend share:
- `Monomorphize.hs` - resolves implicit-param dispatch at the Surface level
- `CompileDriver.hs` - reachability analysis and compilation plan building
- `CLM.hs` - the Core List Machine intermediate representation
- `CLMOptimize.hs` - CLM-level optimization passes

## 1. Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Stack vs register | Register-based | ~30% fewer instructions (Lua 5 paper); better for functional code with many small calls |
| Value representation | Tagged union (Phase 1), NaN-boxing (Phase 2+) | Phase 1 prioritizes correctness; NaN-boxing avoids boxing ints/floats |
| Closures | Flat (copy captures) | Cache-friendly; matches native backend philosophy |
| Memory management | Phase 1: Haskell GC. Phase 2+: explicit RC | Start simple, optimize later |
| Dispatch | Pre-monomorphized + REPL fallback | Reuse existing Monomorphize.hs; eliminates 10-step dispatch cascade |
| Tail calls | TAILCALL instruction reuses frame | Enables unlimited recursion depth |

## 2. Value Representation

### Phase 1: Tagged Union
```haskell
data Val
  = VInt    !Int
  | VFloat  !Double
  | VBool   !Bool
  | VChar   !Char
  | VString !Text
  | VUnit
  | VEmpty
  | VObj    !HeapObj
```

### Phase 2+: NaN-boxing (64-bit)
```
Float64: any valid IEEE 754 double (non-NaN payload)
Tagged:  [1111_1111_1111_TTTT] [48-bit payload]

Tag (TTTT):
  0000 = Heap pointer
  0001 = Int (signed 48-bit; >48-bit -> heap-boxed)
  0010 = Bool (0 or 1)
  0011 = Char (Unicode code point)
  0100 = Small nullary constructor (tag index)
  0101 = Unit
  0110 = Empty/Void
```

## 3. Heap Objects

### Constructor
```
[tag:32][arity:16][flags:16][field0:Val][field1:Val]...[fieldN:Val]
```

### Closure (flat, copied captures)
```
funcIdx:Int | nUpvals:Int | upval0:Val | ... | upvalN:Val
```

### Partial Application
```
funcIdx:Int | expectedArity:Int | appliedArgs:[Val]
```

### Array
```
length:Int | elem0:Val | ... | elemN:Val
```

## 4. Bytecode Instruction Set

32-bit fixed-width instructions, register-addressed (8-bit index = 256 registers per frame).

### Encoding Formats
```
A: [op:8][dst:8][src1:8][src2:8]     -- 3-register
B: [op:8][dst:8][imm16:16]           -- register + 16-bit immediate
C: [op:8][dst:8][src:8][idx:8]       -- register + field index
D: [op:8][operand:24]                -- wide operand (jumps)
```

### Instruction Categories

#### Constants & Moves
- `LOADK r, k` - load from constant pool
- `LOADINT r, i16` - load small integer immediate
- `LOADTRUE/LOADFALSE/LOADUNIT/LOADNIL r` - load constants
- `MOV r1, r2` - register copy

#### Arithmetic (Int)
- `ADDI/SUBI/MULI/DIVI/REMI r, a, b` - integer ops
- `NEGI r, a` - negate

#### Arithmetic (Float64)
- `ADDF/SUBF/MULF/DIVF r, a, b` - float ops
- `NEGF r, a` - negate

#### Comparison
- `EQI/LTI/LEI/GTI/GEI r, a, b` - integer comparison
- `EQF/LTF r, a, b` - float comparison
- `EQP r, a, b` - pointer equality

#### Bitwise
- `BAND/BOR/BXOR/BSHL/BSHR r, a, b`

#### Control Flow
- `JMP off24` - unconditional jump (signed offset)
- `JMPT/JMPF r, off` - conditional jump
- `SWITCH r, tableIdx` - jump table dispatch on constructor tag
- `RET r` - return value

#### Function Calls
- `CALL r, funcIdx, nargs` - call function, result in r
- `TAILCALL funcIdx, nargs` - tail call (reuse frame)
- `CALLCLS r, closureReg, nargs` - call closure object
- `TAILCALLCLS closureReg, nargs` - tail call closure
- `CALLEXT r, externIdx, nargs` - call extern/IO function

#### Closures & Partial Application
- `CLOSURE r, funcIdx, nupvals` - create closure capturing r+1..r+nupvals
- `GETUPVAL r, idx` - load upvalue from current closure
- `PAP r, funcIdx, nargs` - create partial application

#### Constructors & Pattern Matching
- `NEWCON r, tag, nfields` - allocate constructor
- `GETFIELD r, obj, idx` - extract field
- `GETTAG r, obj` - extract constructor tag

#### Arrays
- `NEWARRAY r, len` - allocate array
- `ARRGET r, arr, idx` - array index
- `ARRSET arr, idx, val` - array update
- `ARRLEN r, arr` - array length

#### Mutable References
- `NEWREF r, val` - create ref
- `READREF r, ref` - dereference
- `WRITEREF ref, val` - write ref

#### Effect Handlers
- `PUSHHANDLER handlerIdx` - push handler frame
- `POPHANDLER` - pop handler frame
- `PERFORM r, opIdx, nargs` - perform effect operation

#### IO
- `PRINT r` - print value
- `PRINTLN r` - print with newline
- `READLINE r` - read line from stdin

#### Dispatch (REPL slow path)
- `DISPATCH r, nameIdx, nargs` - runtime instance dispatch

#### Debug & Misc
- `DEBUGLOC srcIdx` - set current source location
- `ERROR msgIdx` - raise runtime error
- `NOP` - no operation
- `HALT` - stop execution

## 5. CLM to Bytecode Compilation

### Per-Node Mapping

| CLM Node | Bytecode |
|----------|----------|
| `CLMLIT (LInt n)` | `LOADINT r,n` or `LOADK r,k` |
| `CLMLIT (LFloat d)` | `LOADK r,k` |
| `CLMLIT (LString s)` | `LOADK r,k` |
| `CLMID name` | `MOV r, regOf(name)` |
| `CLMAPP (CLMID f) args` | compile args, then `CALL r,funcIdx,n` or `TAILCALL` |
| `CLMCON tag fields` | compile fields, then `NEWCON r,tag,n` |
| `CLMFieldAccess (_,idx) e` | compile e, then `GETFIELD r,obj,idx` |
| `CLMLAM (CLMLam vs body)` | emit function, then `CLOSURE r,funcIdx,nCaptures` |
| `CLMLAM (CLMLamCases vs cases)` | emit function with GETTAG+SWITCH dispatch |
| `CLMCASE checks body` | conditional jumps on tag/literal checks |
| `CLMPROG exprs` | compile sequentially; last expr result is value |
| `CLMBIND name expr` | compile expr, bind name to register |
| `CLMIAP` | after monomorphize: shouldn't appear. Fallback: `DISPATCH` |
| `CLMTYPED inner _` | compile inner (type hint erased) |
| `CLMARRAY elems` | `NEWARRAY` + `ARRSET` sequence |
| `CLMMCALL obj meth args` | resolve method at compile time, `CALL` |
| `CLMHANDLE body eff lets ops` | `PUSHHANDLER`, compile body, `POPHANDLER` |
| `CLMERR msg si` | `ERROR msgIdx` |
| `CLMEMPTY/CLMU` | `LOADUNIT r` |

### Tail Call Detection

A call is in tail position when it is the last expression before RET in:
- Function body (top level)
- Each arm of a pattern match
- Last expression in CLMPROG
- Body of a let-binding's continuation

### Pattern Match Compilation

`CLMLamCases vars cases` compiles to:
1. `GETTAG` to extract scrutinee's constructor tag
2. `SWITCH` for small tag sets (<=8 constructors), or linear `JMPTAG` chain
3. Each arm: `GETFIELD` to bind destructured fields, then compile body
4. Default: `ERROR "pattern match failure"`

## 6. Tail Call Optimization

The VM implements proper tail calls:

1. **Self-tail-call** (most common): detected at compile time, emitted as `JMP` back
   to function entry with updated registers. Equivalent to a loop.

2. **General tail call**: `TAILCALL funcIdx, nargs` copies args into parameter
   positions of the target function, sets PC to target entry, does NOT push a new
   frame. Current frame is reused.

3. **Closure tail call**: `TAILCALLCLS closureReg, nargs` - same but for closures.

This means recursive functions consume O(1) stack space regardless of recursion depth.

## 7. Execution Engine

### VM State
```haskell
data VMState = VMState
  { vmCode       :: !(Vector Word32)       -- bytecode instructions
  , vmConstants  :: !(Vector Val)          -- constant pool
  , vmFunctions  :: !(Vector FuncInfo)     -- function table
  , vmRegisters  :: !(IOVector Val)        -- register file
  , vmPC         :: !(IORef Int)           -- program counter
  , vmFP         :: !(IORef Int)           -- frame pointer
  , vmCallStack  :: !(IORef [Frame])       -- return stack
  , vmHandlers   :: !(IORef [HandlerFrame]) -- effect handler stack
  , vmDebugLoc   :: !(IORef SourceInfo)    -- current source location
  , vmGlobals    :: !(IOVector Val)        -- global variables
  , vmEnv        :: !Environment           -- for REPL dispatch fallback
  }
```

### Core Dispatch Loop

The inner loop decodes one 32-bit instruction per iteration and dispatches on the
opcode. In Haskell, this is a tight tail-recursive function that GHC compiles to
efficient machine code. Key properties:
- No allocation in the hot path (values are strict/unboxed)
- No Haskell stack growth (tail-recursive loop)
- No convergence re-evaluation (unlike `_contEval` in the tree-walker)

## 8. Debug Information

### Source Map
Each bytecode function carries a source map: `Vector (Int, SourceInfo)` mapping
bytecode offsets to source locations.

### DEBUGLOC Instruction
Emitted at expression boundaries. The VM updates its current source location on
each DEBUGLOC, enabling:
- Stack traces with source locations on errors
- Breakpoint setting by source line
- Step debugging (future)

### Type Information
The bytecode module stores the original Environment type information alongside the
bytecode. This allows the REPL to inspect types via `:list types` / `:list functions`.

## 9. Module Format

```
BytecodeModule (.tlb):
  Header:      magic "TLBC" | version:u16 | flags:u16
  Constants:   count:u32 | [Int64|Float64|String|FuncRef]
  Functions:   count:u32 | [name, arity, numRegs, bcOffset, bcLen]
  Bytecode:    [u32 instructions]
  ConsTable:   [(name, tag, arity)]
  DebugInfo:   [per-function source maps + locals + type sigs]
  Environment: (optional) serialized Environment for REPL
```

Cached in `.tulam-cache/bc1/` alongside existing `.tli` caches.

## 10. Implementation Phases

### Phase 1: Core VM
- Value type (tagged union)
- Instruction set definition + disassembler
- CLM -> bytecode compiler for: literals, arithmetic, function calls, tail calls,
  pattern matching, constructors, field access, let bindings
- VM dispatch loop with proper TCO
- Test: fibonacci(50), factorial(100), simple pattern matching

### Phase 2: Closures & Higher-Order
- Closure creation/invocation, partial application
- Arrays
- Test: map/filter/fold, higher-order functions

### Phase 3: Full Feature Parity
- Monomorphization integration
- REPL fallback dispatch
- Effect handlers, mutable refs, IO intrinsics
- String operations, class method dispatch
- Test: all 1054 existing tests pass

### Phase 4: REPL Integration & Debug
- `:s backend bytecode` command
- `:bc <name>` disassembly
- Source-mapped error messages and stack traces
- Bytecode module caching

### Phase 5: Performance Optimization
- NaN-boxing (Word64 value representation)
- Unboxed register file (MutableByteArray#)
- Superinstruction fusion
- Inline caching for dispatch

## 11. Performance Expectations

The primary speedup sources:
1. **No tree traversal**: bytecode is flat, sequential; CPU prefetcher works
2. **No Haskell allocation in inner loop**: strict values avoid GHC boxing
3. **No HashMap lookup for dispatch**: monomorphized to direct calls
4. **Proper tail calls**: no stack growth, no depth limit
5. **No `_contEval` fixpoint loop**: single-pass evaluation (biggest win, ~5-10x alone)

## 12. File Structure

```
src/Backends/Bytecode/
  Value.hs        -- Val type + operations
  Instruction.hs  -- Opcode enum, encoding, disassembler
  Compile.hs      -- CLM -> BytecodeModule compiler
  VM.hs           -- Execution engine
  Module.hs       -- BytecodeModule type, constant pool
  Debug.hs        -- DebugInfo, source maps, stack traces
  Heap.hs         -- Heap objects (constructor, closure, array, PAP)
```
