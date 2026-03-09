# Concurrency & Multithreading Design: Effect-Based Structured Concurrency

## 1. Design Philosophy

tulam's concurrency model is built on three pillars:

1. **Concurrency as effects** — async operations are row-polymorphic effects, composing naturally with State, Exception, IO, and all other effects. The type `Eff { async: Async } a` tells you exactly what a function does.
2. **Structured concurrency** — every spawned task has a lexically scoped lifetime. No leaked goroutines, no orphaned promises, no fire-and-forget threads. All child tasks must complete (or be cancelled) before their parent scope exits.
3. **Swappable runtime model** — the same concurrent code runs on green threads (native), OS threads, JS Promises, .NET Tasks, or single-threaded (testing) by swapping the handler. The effect system makes the concurrency backend a pluggable implementation detail.

### Why Not Other Approaches?

| Approach | Why Not |
|----------|---------|
| Go-style goroutines | Implicit concurrency (no type tracking), no structured lifetimes, leaked goroutine problem |
| Rust async/await | Requires pinning, lifetime annotations, colored function problem — too much ceremony |
| Java virtual threads | Good model, but no effect tracking — concurrency is invisible in types |
| Haskell `forkIO` + `MVar` | Low-level, no structured lifetimes, easy to leak threads |
| Actor model only | Forces message-passing even when shared state is simpler; good as library pattern, not sole primitive |

tulam's approach is closest to **Koka** and **OCaml 5** (algebraic effects for concurrency) but adds structured concurrency guarantees inspired by **Trio (Python)** and **JEP 453 (Java)**, plus OOP class integration and multi-target codegen.

### Design Principles

- **Explicit over implicit**: Concurrent operations are tracked in effect types. You always know if a function can fork/await.
- **Safe by default**: Structured concurrency prevents resource leaks. Cancellation is cooperative.
- **Progressive complexity**: Simple parallel combinators (`par`, `pmap`) for common cases; channels/STM for complex coordination; session types for protocol safety.
- **Target-agnostic source**: Write once, compile to any concurrency backend. Handler selection determines the runtime model.
- **Composable primitives**: Channels compose with STM. STM composes with effects. Effects compose with each other. No "monad transformer" stacking.

---

## 2. Core Async Effect

The fundamental concurrency primitive is the `Async` effect, which provides task creation, awaiting, cancellation, and yielding.

### 2.1 Task Type

```tulam
/// An opaque handle to a concurrent computation.
/// The type parameter `a` is the result type of the computation.
/// Tasks are created by `fork` or `spawn` and consumed by `await`.
primitive Task(a: Type);
```

`Task` is an opaque primitive — no constructors, no pattern matching. At runtime:
- **Interpreter**: Wraps a Haskell `Async a` (from `Control.Concurrent.Async`)
- **JS target**: Compiles to `Promise<A>`
- **.NET target**: Compiles to `Task<A>`
- **Native target**: A pointer to a work-stealing scheduler task descriptor

### 2.2 Async Effect Declaration

```tulam
module Effect.Async;

/// Core concurrency effect. Provides task creation, synchronization, and cooperative scheduling.
/// The runtime model (green threads, OS threads, promises) is determined by the handler.
effect Async = {
    /// Fork a computation into a new concurrent task.
    /// The task begins executing immediately (or is scheduled for execution).
    /// Returns a handle that can be awaited for the result.
    function fork(body: Unit -> a) : Task(a);

    /// Wait for a task to complete and return its result.
    /// If the task failed with an exception, re-raises the exception in the caller.
    /// If the task was cancelled, raises a CancelledException.
    function await(task: Task(a)) : a;

    /// Request cancellation of a task.
    /// Cancellation is cooperative — the task will be cancelled at the next yield point.
    /// Awaiting a cancelled task raises CancelledException.
    function cancel(task: Task(a)) : Unit;

    /// Yield control to the scheduler, allowing other tasks to run.
    /// In cooperative concurrency, long-running computations should yield periodically.
    function yield() : Unit
};
```

### 2.3 Basic Usage

```tulam
/// Fetch two URLs concurrently and return both results
action fetchBoth(u1: String, u2: String) : Eff { async: Async, net: NetworkIO } (String, String) = {
    t1 <- fork(\_ -> httpGet(u1));
    t2 <- fork(\_ -> httpGet(u2));
    r1 <- await(t1);
    r2 <- await(t2);
    pure((r1, r2))
};
```

### 2.4 Async Handlers

The handler determines the concurrency backend. All handlers implement the same `Async` effect interface.

```tulam
/// Green threads backed by a work-stealing scheduler.
/// Lightweight (thousands of concurrent tasks), preemptive at yield points.
/// Used by: interpreter (via Haskell forkIO), native target.
handler GreenThreads : Async = {
    function fork(body) = intrinsic;
    function await(task) = intrinsic;
    function cancel(task) = intrinsic;
    function yield() = intrinsic;
};

/// JavaScript Promise-based concurrency.
/// fork compiles to async function invocation, await to JS await.
/// Used by: JS codegen target.
handler PromiseAsync : Async = {
    function fork(body) = intrinsic;    /// => new Promise((resolve) => resolve(body()))
    function await(task) = intrinsic;   /// => await task
    function cancel(task) = intrinsic;  /// => AbortController.abort()
    function yield() = intrinsic;       /// => await new Promise(r => setTimeout(r, 0))
};

/// .NET Task-based concurrency.
/// fork compiles to Task.Run, await to C# await.
/// Used by: .NET codegen target.
handler DotNetTasks : Async = {
    function fork(body) = intrinsic;    /// => Task.Run(() => body())
    function await(task) = intrinsic;   /// => await task
    function cancel(task) = intrinsic;  /// => CancellationTokenSource.Cancel()
    function yield() = intrinsic;       /// => await Task.Yield()
};

/// Sequential execution — no actual concurrency.
/// fork runs the body immediately and returns the result wrapped as a completed task.
/// Used for: deterministic testing, debugging, single-threaded environments.
handler SequentialAsync : Async = {
    function fork(body) = CompletedTask(body(Unit));
    function await(task) = taskResult(task);
    function cancel(task) = Unit;
    function yield() = Unit;
};
```

### 2.5 Running Async Code

```tulam
/// Run an async computation with a specific handler.
/// All tasks forked inside must complete before this returns.
function runAsync(body: Unit -> Eff { async: Async } a) : a =
    handle body(Unit) with GreenThreads;

/// Run async computation with sequential execution (for testing)
function runSequential(body: Unit -> Eff { async: Async } a) : a =
    handle body(Unit) with SequentialAsync;
```

---

## 3. Structured Concurrency

### 3.1 The Problem With Unstructured Concurrency

In Go, Java, and most languages, `fork`/`go`/`Thread.start` creates a task with no lexical owner:

```
// Pseudocode — the bad pattern
function handle(request) {
    go processInBackground(request)  // Who owns this goroutine?
    return "accepted"                // Function returns, goroutine lives on
}   // Goroutine may outlive the function, the request, even the server
```

This leads to: leaked goroutines, dangling references, uncollected errors, impossible-to-test nondeterminism, resource exhaustion.

### 3.2 Scoped Task Nurseries

Structured concurrency ties task lifetime to lexical scope via **nurseries** (term from Trio) or **task scopes**:

```tulam
/// A scope that owns child tasks. When the scope exits:
/// 1. All unfinished child tasks are cancelled
/// 2. The scope waits for all tasks to complete (cancelled or not)
/// 3. If any child threw an exception, it propagates to the scope owner
effect Scope = {
    /// Spawn a new task within this scope.
    /// The task's lifetime is bounded by the scope.
    function spawn(body: Unit -> a) : Task(a);

    /// Spawn a linked task — if it fails, all siblings are cancelled.
    /// Used for "all-or-nothing" parallel operations.
    function spawnLinked(body: Unit -> a) : Task(a)
};
```

### 3.3 The `withScope` Combinator

```tulam
/// Run a scoped block. All tasks spawned within the scope must complete
/// before withScope returns. Leaked tasks are impossible.
///
/// @param body  A function receiving the scope handle. Use scope to spawn tasks.
/// @returns     The result of body, after all spawned tasks have completed.
function withScope [a:Type] (body: Scope -> Eff { async: Async } a) : Eff { async: Async } a =
    handle body(currentScope) with StructuredScope;

handler StructuredScope : Scope = {
    let tasks = newRef(Nil : List(Task(Any)));

    function spawn(body) = {
        t <- fork(body);
        modifyRef(tasks, \ts -> Cons(t, ts));
        t
    };

    function spawnLinked(body) = {
        t <- fork(\_ ->
            match catch(body, \e -> cancelSiblings(tasks) >> throw(e))
            | result -> result
        ),
        modifyRef(tasks, \ts -> Cons(t, ts));
        t
    };
};
```

### 3.4 Usage Patterns

```tulam
/// Fan-out/fan-in pattern — parallel map with bounded scope
action parallelMap [a:Type, b:Type] (f: a -> Eff { async: Async } b, xs: List(a))
    : Eff { async: Async } List(b) = {
    withScope(\scope -> {
        tasks <- mapM(\x -> scope.spawn(\_ -> f(x)), xs);
        mapM(await, tasks)
    })
};

/// Race pattern — first result wins, others are cancelled
action race [a:Type] (computations: List(Unit -> Eff { async: Async } a))
    : Eff { async: Async } a = {
    withScope(\scope -> {
        resultCh <- newChan();
        mapM_(\c -> scope.spawn(\_ -> send(resultCh, c(Unit))), computations);
        recv(resultCh)
        /// scope exit cancels all remaining tasks
    })
};

/// Timeout pattern — cancel if too slow
action withTimeout [a:Type] (ms: Int, body: Unit -> Eff { async: Async } a)
    : Eff { async: Async } Maybe(a) = {
    withScope(\scope -> {
        worker <- scope.spawn(body);
        timer  <- scope.spawn(\_ -> { sleep(ms), pure(Nothing) });
        select(
            OnTask(worker, \r -> Just(r));
            OnTask(timer,  \r -> r)
        )
    })
};
```

### 3.5 Scope Nesting

Scopes nest naturally. A child scope's tasks are cleaned up before the parent scope continues:

```tulam
action pipeline(data: List(Item)) : Eff { async: Async } List(Result) = {
    withScope(\outer -> {
        /// Stage 1: parallel fetch
        fetched <- withScope(\inner -> {
            tasks <- mapM(\item -> inner.spawn(\_ -> fetch(item)), data);
            mapM(await, tasks)
        }),
        /// inner scope is done — all fetch tasks completed or cancelled

        /// Stage 2: parallel process
        withScope(\inner -> {
            tasks <- mapM(\f -> inner.spawn(\_ -> process(f)), fetched);
            mapM(await, tasks)
        })
    })
};
```

---

## 4. Channels (CSP Communication)

Channels provide typed, bounded communication between concurrent tasks. Inspired by Go channels and Concurrent ML.

### 4.1 Channel Types

```tulam
/// A typed channel for sending and receiving values between tasks.
/// Channels can be unbuffered (synchronous) or buffered (asynchronous up to capacity).
primitive Channel(a: Type);

/// A receive-only view of a channel.
primitive RecvChan(a: Type);

/// A send-only view of a channel.
primitive SendChan(a: Type);
```

### 4.2 Channel Effect

```tulam
module Effect.Chan;

/// Channel-based communication between concurrent tasks.
effect Chan = {
    /// Create a new unbuffered (synchronous) channel.
    /// Sends block until a receiver is ready; receives block until a sender is ready.
    function newChan() : Channel(a);

    /// Create a new buffered channel with the given capacity.
    /// Sends block only when the buffer is full; receives block when the buffer is empty.
    function newBufferedChan(capacity: Int) : Channel(a);

    /// Send a value on a channel. Blocks until received (unbuffered) or buffer has space (buffered).
    function send(ch: Channel(a), val: a) : Unit;

    /// Receive a value from a channel. Blocks until a value is available.
    function recv(ch: Channel(a)) : a;

    /// Try to receive without blocking. Returns Nothing if no value is available.
    function tryRecv(ch: Channel(a)) : Maybe(a);

    /// Try to send without blocking. Returns False if channel is full.
    function trySend(ch: Channel(a), val: a) : Bool;

    /// Close a channel. Subsequent sends will raise an exception.
    /// Receives will drain remaining buffered values, then return Nothing.
    function close(ch: Channel(a)) : Unit
};
```

### 4.3 Select (Multiplexing)

```tulam
/// A select case: an operation to attempt on a channel.
type SelectCase(a) =
    /// Wait to receive from a channel, transform the value
    OnRecv(Channel(b), b -> a)
    /// Wait to send on a channel, then run continuation
  + OnSend(Channel(b), b, Unit -> a)
    /// If no other case is ready, run the default
  + Default(Unit -> a);

/// Wait on multiple channel operations. The first ready case wins.
/// If multiple cases are ready, one is chosen at random (fairness).
/// If no case is ready and no Default is provided, blocks until one becomes ready.
function select [a:Type] (cases: List(SelectCase(a))) : Eff { async: Async, chan: Chan } a =
    intrinsic;
```

### 4.4 Channel Patterns

```tulam
/// Producer-consumer with bounded backpressure
action producerConsumer(items: List(Item)) : Eff { async: Async, chan: Chan } List(Result) = {
    ch <- newBufferedChan(64); /// buffer up to 64 items
    results <- newChan();

    withScope(\scope -> {
        /// Producer: feed items into channel
        scope.spawn(\_ -> {
            mapM_(\item -> send(ch, item), items);
            close(ch)
        }),

        /// N consumers: process items in parallel
        replicateM_(4, scope.spawn(\_ -> {
            loop(\_ -> match tryRecv(ch)
                | Just(item) -> { send(results, process(item)), Continue }
                | Nothing    -> Stop
            )
        })),

        /// Collector: gather results
        scope.spawn(\_ -> collectN(results, length(items)))
    })
};

/// Fan-out: one sender, multiple receivers
action fanOut [a:Type] (source: Channel(a), workers: Int, process: a -> b)
    : Eff { async: Async, chan: Chan } List(b) = {
    withScope(\scope -> {
        replicateM(workers, scope.spawn(\_ ->
            loop(\_ -> match tryRecv(source)
                | Just(val) -> Yield(process(val))
                | Nothing   -> Stop
            )
        ))
    })
};
```

### 4.5 Channel Direction Subtyping

Channels support direction subtyping for safety:

```tulam
/// A Channel(a) can be used where RecvChan(a) or SendChan(a) is expected.
/// This prevents accidentally sending on a receive-only channel.
morphism ChannelToRecv [a:Type] = {
    function convert(ch: Channel(a)) : RecvChan(a) = intrinsic;
};

morphism ChannelToSend [a:Type] = {
    function convert(ch: Channel(a)) : SendChan(a) = intrinsic;
};
```

---

## 5. Software Transactional Memory (STM)

STM provides composable, deadlock-free shared mutable state for concurrent programs. Unlike locks, STM transactions compose: if `transfer(a, b)` and `transfer(b, c)` are each correct, then `atomically { transfer(a, b); transfer(b, c) }` is automatically correct too.

### 5.1 TVar — Transactional Variables

```tulam
/// A transactional variable — a mutable cell that can only be read/written inside
/// an STM transaction. Reads and writes are automatically atomic and isolated.
primitive TVar(a: Type);
```

### 5.2 STM Effect

```tulam
module Effect.STM;

/// Software Transactional Memory. All operations inside an STM transaction
/// execute atomically — either all succeed or none do. No locks, no deadlocks.
effect STM = {
    /// Create a new transactional variable with an initial value.
    function newTVar(init: a) : TVar(a);

    /// Read the current value of a TVar within a transaction.
    function readTVar(tv: TVar(a)) : a;

    /// Write a new value to a TVar within a transaction.
    function writeTVar(tv: TVar(a), val: a) : Unit;

    /// Abort and retry the current transaction.
    /// The transaction will be automatically re-run when any TVar it has read changes.
    /// This is the key primitive for blocking: "wait until condition is true".
    function retry() : a;

    /// Compose two alternative transactions.
    /// Try the first; if it calls retry, run the second instead.
    /// If both retry, the combined transaction retries when any read TVar changes.
    function orElse(first: Unit -> Eff { stm: STM } a, second: Unit -> Eff { stm: STM } a) : a
};
```

### 5.3 Atomically — Running Transactions

```tulam
/// Execute an STM transaction atomically.
/// The transaction sees a consistent snapshot of all TVars.
/// If a conflict is detected (another thread modified a TVar we read),
/// the transaction is automatically retried from the beginning.
///
/// IMPORTANT: The body must be pure + STM effects only.
/// IO operations inside atomically are forbidden (they might be re-executed on retry).
function atomically [a:Type] (body: Unit -> Eff { stm: STM } a) : Eff { async: Async } a =
    handle body(Unit) with STMRuntime;

handler STMRuntime : STM = {
    function newTVar(init) = intrinsic;     /// allocate TVar with versioned value
    function readTVar(tv) = intrinsic;      /// read into transaction log
    function writeTVar(tv, val) = intrinsic; /// write into transaction log
    function retry() = intrinsic;           /// abort, wait for change, re-run
    function orElse(f, g) = intrinsic;      /// try f, on retry try g
};
```

### 5.4 STM Usage Patterns

```tulam
/// Bank transfer — atomic, deadlock-free, composable
action transfer(from: TVar(Int), to: TVar(Int), amount: Int) : Eff { stm: STM } Unit = {
    balance <- readTVar(from);
    if balance < amount
    then retry()              /// block until funds are available
    else {
        writeTVar(from, balance - amount);
        toBalance <- readTVar(to);
        writeTVar(to, toBalance + amount)
    }
};

/// Bounded buffer using STM
action newBoundedBuffer(capacity: Int) : Eff { stm: STM } BoundedBuffer(a) = {
    items <- newTVar(Nil : List(a));
    size  <- newTVar(0);
    pure(BoundedBuffer(items, size, capacity))
};

action bufferWrite(buf: BoundedBuffer(a), val: a) : Eff { stm: STM } Unit = {
    sz <- readTVar(buf.size);
    if sz >= buf.capacity then retry(); /// block until space available
    modifyTVar(buf.items, \xs -> append(xs, Cons(val, Nil)));
    writeTVar(buf.size, sz + 1)
};

action bufferRead(buf: BoundedBuffer(a)) : Eff { stm: STM } a = {
    xs <- readTVar(buf.items);
    match xs
    | Nil -> retry()                       /// block until item available
    | Cons(x, rest) -> {
        writeTVar(buf.items, rest);
        modifyTVar(buf.size, \n -> n - 1);
        pure(x)
    }
};

/// Compose: transfer-and-log as a single atomic operation
action transferAndLog(from: TVar(Int), to: TVar(Int), amount: Int, log: TVar(List(String)))
    : Eff { stm: STM } Unit = {
    transfer(from, to, amount);
    modifyTVar(log, \xs -> Cons("Transferred " ++ show(amount), xs))
};
/// This is automatically atomic — impossible with locks!
```

### 5.5 STM + Channels Integration

STM and channels serve different purposes and compose naturally:

| Use Case | Primitive |
|----------|-----------|
| Shared mutable state (read/write) | TVar + STM |
| Point-to-point communication | Channel |
| Broadcast / pub-sub | TVar (readers poll) or Channel per subscriber |
| Conditional blocking ("wait until X") | STM `retry` |
| Multiple producers/consumers | Buffered Channel |
| Atomic multi-variable update | STM transaction |

---

## 6. Parallel Combinators

High-level combinators for common concurrent patterns. These are library functions built on the core `Async` effect.

### 6.1 Parallel Algebra

```tulam
module Concurrent.Parallel;

/// Run two computations in parallel, return both results.
function par [a:Type, b:Type]
    (left: Unit -> Eff { async: Async } a, right: Unit -> Eff { async: Async } b)
    : Eff { async: Async } (a, b) =
    withScope(\scope -> {
        t1 <- scope.spawnLinked(left);
        t2 <- scope.spawnLinked(right);
        r1 <- await(t1);
        r2 <- await(t2);
        pure((r1, r2))
    });

/// Parallel map over a list.
function pmap [a:Type, b:Type]
    (f: a -> Eff { async: Async } b, xs: List(a))
    : Eff { async: Async } List(b) =
    withScope(\scope -> {
        tasks <- mapM(\x -> scope.spawn(\_ -> f(x)), xs);
        mapM(await, tasks)
    });

/// Parallel map over an array (more efficient than list version).
function pmapArray [a:Type, b:Type]
    (f: a -> Eff { async: Async } b, xs: Array(a))
    : Eff { async: Async } Array(b) =
    withScope(\scope -> {
        tasks <- arrayMapM(\x -> scope.spawn(\_ -> f(x)), xs);
        arrayMapM(await, tasks)
    });

/// Race: run multiple computations, return the first result, cancel the rest.
function raceList [a:Type]
    (computations: List(Unit -> Eff { async: Async } a))
    : Eff { async: Async } a =
    withScope(\scope -> {
        ch <- newChan();
        mapM_(\c -> scope.spawnLinked(\_ -> send(ch, c(Unit))), computations);
        recv(ch)
    });

/// Wait for all tasks in a list to complete.
function awaitAll [a:Type] (tasks: List(Task(a))) : Eff { async: Async } List(a) =
    mapM(await, tasks);

/// Wait for the first task to complete, return its result.
function awaitAny [a:Type] (tasks: List(Task(a))) : Eff { async: Async } a =
    raceList(map(\t -> \_ -> await(t), tasks));

/// Run with a timeout. Returns Nothing if the computation exceeds the time limit.
function withTimeout [a:Type]
    (ms: Int, body: Unit -> Eff { async: Async } a)
    : Eff { async: Async } Maybe(a) =
    withScope(\scope -> {
        worker <- scope.spawn(body);
        timer  <- scope.spawn(\_ -> { sleep(ms), pure(Nothing) });
        raceList(
            Cons(\_ -> Just(await(worker)),
            Cons(\_ -> await(timer), Nil))
        )
    });
```

### 6.2 Bounded Parallelism

```tulam
/// Parallel map with bounded concurrency — at most N tasks run simultaneously.
/// Essential for controlling resource usage (connections, file handles, memory).
function pmapBounded [a:Type, b:Type]
    (maxConcurrency: Int, f: a -> Eff { async: Async } b, xs: List(a))
    : Eff { async: Async } List(b) = {
    sem <- newSemaphore(maxConcurrency);
    withScope(\scope -> {
        tasks <- mapM(\x -> scope.spawn(\_ -> {
            acquireSem(sem);
            result <- f(x);
            releaseSem(sem);
            pure(result)
        }), xs),
        mapM(await, tasks)
    })
};
```

---

## 7. Cancellation Model

Cancellation is **cooperative** — a cancelled task is not forcibly terminated. Instead, it receives a cancellation signal and is expected to check it at yield points.

### 7.1 Design Rationale

| Model | Pros | Cons |
|-------|------|------|
| Preemptive (Java `Thread.interrupt()`) | Immediate | Unsafe — can interrupt in the middle of an invariant |
| Cooperative (Go `context.Done()`) | Safe — task controls when to stop | May not respond quickly |
| Asynchronous exception (Haskell `throwTo`) | Immediate + maskable | Complex masking semantics |

We choose cooperative cancellation with automatic checking at effect operation boundaries. Every call to `await`, `yield`, `send`, `recv`, etc. is an implicit cancellation check point.

### 7.2 Cancellation API

```tulam
/// Check if the current task has been cancelled. Non-blocking.
function isCancelled() : Eff { async: Async } Bool = intrinsic;

/// Throw CancelledException if the current task has been cancelled.
/// Called implicitly at every effect operation boundary.
function checkCancellation() : Eff { async: Async } Unit = intrinsic;

/// Run a block with cancellation disabled. The body will not be cancelled
/// even if cancel() is called on the task. Used for cleanup/finalization.
function uncancellable [a:Type] (body: Unit -> Eff { async: Async } a)
    : Eff { async: Async } a = intrinsic;

/// Exception type raised when a task is cancelled.
type CancelledException = CancelledException;
```

### 7.3 Structured Cancellation

When a scope exits (normally or via exception), all child tasks are cancelled and awaited:

```
withScope body:
  1. Create scope
  2. Run body(scope)
  3. If body completes normally:
     - Cancel all unfinished child tasks
     - Await all child tasks (let them clean up)
     - Return body's result
  4. If body throws an exception:
     - Cancel all child tasks
     - Await all child tasks
     - Re-raise the exception
  5. If body's scope has spawnLinked tasks and one fails:
     - Cancel all sibling tasks
     - Await all tasks
     - Propagate the first failure
```

---

## 8. Effect Composition

The power of effect-based concurrency is that it composes with all other effects.

### 8.1 Async + State

```tulam
/// Concurrent counter — each worker increments a shared counter
action concurrentCount(n: Int) : Eff { async: Async, state: State(Int) } Int = {
    withScope(\scope -> {
        replicateM_(n, scope.spawn(\_ -> modify(\x -> x + 1)));
    }),
    get()
};

/// NOTE: plain State is not thread-safe. For concurrent shared state, use STM:
action concurrentCountSafe(n: Int) : Eff { async: Async } Int = {
    counter <- atomically(\_ -> newTVar(0));
    withScope(\scope -> {
        replicateM_(n, scope.spawn(\_ ->
            atomically(\_ -> modifyTVar(counter, \x -> x + 1))
        ))
    }),
    atomically(\_ -> readTVar(counter))
};
```

### 8.2 Async + Exception

```tulam
/// Exceptions in child tasks propagate to the awaiter
action riskyFetch(url: String) : Eff { async: Async, exn: Exception(String) } String = {
    task <- fork(\_ ->
        match httpGet(url)
        | Ok(body) -> body
        | Err(msg) -> throw("Fetch failed: " ++ msg)
    ),
    /// If httpGet failed, await re-raises the exception here
    await(task)
};

/// Catch exceptions from concurrent tasks
action safeFetch(url: String) : Eff { async: Async } Maybe(String) = {
    catch(
        \_ -> Just(riskyFetch(url));
        \err -> { putStrLn("Error: " ++ err), pure(Nothing) }
    )
};
```

### 8.3 Async + Console + FileIO

```tulam
/// Concurrent file processor with progress reporting
action processFiles(files: List(String))
    : Eff { async: Async, console: Console, fileio: FileIO } List(Result) = {
    total <- pure(length(files));
    counter <- atomically(\_ -> newTVar(0));

    results <- pmapBounded(8, \file -> {
        content <- readFile(file);
        result <- processContent(content);
        n <- atomically(\_ -> {
            modifyTVar(counter, \x -> x + 1);
            readTVar(counter)
        }),
        putStrLn("[" ++ show(n) ++ "/" ++ show(total) ++ "] " ++ file);
        pure(result)
    }, files),

    putStrLn("Done! Processed " ++ show(total) ++ " files.");
    pure(results)
};
```

---

## 9. Actors (Library Pattern)

Actors are not a language primitive — they are a library pattern built on classes + channels + async. This keeps the core small while providing a familiar abstraction.

### 9.1 Actor Base Class

```tulam
/// Base class for actors. Subclass and implement `receive` to define behavior.
/// Each actor has a mailbox (channel) and runs in its own task.
abstract class Actor(msg: Type) = {
    private mailbox : Channel(msg);

    /// Handle a received message. Override in subclass.
    function receive(self, message: msg) : Eff { async: Async } Unit;

    /// Send a message to this actor (non-blocking).
    function tell(self, message: msg) : Eff { chan: Chan } Unit =
        send(self.mailbox, message);

    /// Start the actor's message loop. Called internally by spawn.
    function run(self) : Eff { async: Async, chan: Chan } Unit =
        loop(\_ -> {
            msg <- recv(self.mailbox);
            self.receive(msg);
            Continue
        });
};

/// An opaque reference to a running actor. Can only send messages.
type ActorRef(msg: Type) = sendChan : SendChan(msg);

/// Spawn an actor in a scope. Returns a reference for sending messages.
function spawnActor [msg:Type] (scope: Scope, actor: Actor(msg))
    : Eff { async: Async, chan: Chan } ActorRef(msg) = {
    scope.spawn(\_ -> actor.run());
    pure(ActorRef(actor.mailbox as SendChan(msg)))
};
```

### 9.2 Actor Example

```tulam
/// A counter actor that maintains internal state
class CounterActor() extends Actor(CounterMsg) = {
    private count : Int = 0;

    override function receive(self, message: CounterMsg) : Eff { async: Async } Unit =
        match message
        | Increment    -> { self.count = self.count + 1 }
        | Decrement    -> { self.count = self.count - 1 }
        | GetCount(ch) -> send(ch, self.count);
};

type CounterMsg = Increment + Decrement + GetCount * ch:Channel(Int);

action main() : Eff { async: Async, chan: Chan, console: Console } Unit = {
    withScope(\scope -> {
        counter <- spawnActor(scope, CounterActor.new());
        tell(counter, Increment);
        tell(counter, Increment);
        tell(counter, Increment);
        tell(counter, Decrement);
        replyCh <- newChan();
        tell(counter, GetCount(replyCh));
        count <- recv(replyCh);
        putStrLn("Count: " ++ show(count))   /// "Count: 2"
    })
};
```

---

## 10. Synchronization Primitives

Built on top of core effects, provided as standard library.

### 10.1 Semaphore

```tulam
module Concurrent.Semaphore;

/// A counting semaphore backed by STM.
type Semaphore = count:TVar(Int);

function newSemaphore(permits: Int) : Eff { async: Async } Semaphore =
    atomically(\_ -> {
        tv <- newTVar(permits);
        pure(Semaphore(tv))
    });

function acquireSem(sem: Semaphore) : Eff { async: Async } Unit =
    atomically(\_ -> {
        n <- readTVar(sem.count);
        if n <= 0 then retry()
        else writeTVar(sem.count, n - 1)
    });

function releaseSem(sem: Semaphore) : Eff { async: Async } Unit =
    atomically(\_ -> modifyTVar(sem.count, \n -> n + 1));
```

### 10.2 Mutex (Mutual Exclusion)

```tulam
module Concurrent.Mutex;

/// A mutual exclusion lock backed by a semaphore.
function newMutex() : Eff { async: Async } Semaphore = newSemaphore(1);

function withMutex [a:Type] (m: Semaphore, body: Unit -> Eff { async: Async } a)
    : Eff { async: Async } a = {
    acquireSem(m);
    result <- catch(\_ -> body(Unit), \e -> { releaseSem(m), throw(e) });
    releaseSem(m);
    pure(result)
};
```

### 10.3 Barrier

```tulam
module Concurrent.Barrier;

/// A cyclic barrier — N tasks must arrive before any can proceed.
type Barrier = remaining:TVar(Int) * generation:TVar(Int);

function newBarrier(parties: Int) : Eff { async: Async } Barrier =
    atomically(\_ -> {
        r <- newTVar(parties);
        g <- newTVar(0);
        pure(Barrier(r, g))
    });

function arriveAndWait(barrier: Barrier) : Eff { async: Async } Unit =
    atomically(\_ -> {
        gen <- readTVar(barrier.generation);
        n <- readTVar(barrier.remaining);
        if n == 1
        then {
            writeTVar(barrier.remaining, parties); /// reset for next cycle
            writeTVar(barrier.generation, gen + 1)  /// release all waiters
        }
        else {
            writeTVar(barrier.remaining, n - 1);
            /// Block until generation changes (others arrive)
            waitForGeneration(barrier, gen)
        }
    });
```

### 10.4 CountDownLatch, ReadWriteLock, Once

```tulam
/// CountDownLatch — wait until count reaches zero
type CountDownLatch = count:TVar(Int);

function newLatch(n: Int) : Eff { async: Async } CountDownLatch =
    atomically(\_ -> { tv <- newTVar(n), pure(CountDownLatch(tv)) });

function countDown(latch: CountDownLatch) : Eff { async: Async } Unit =
    atomically(\_ -> modifyTVar(latch.count, \n -> n - 1));

function awaitLatch(latch: CountDownLatch) : Eff { async: Async } Unit =
    atomically(\_ -> { n <- readTVar(latch.count), if n > 0 then retry() else pure(Unit) });

/// Once — execute a function exactly once, memoize the result
type Once(a: Type) = state : TVar(OnceState(a));
type OnceState(a) = Uninitialized + Running + Done * val:a;

function newOnce() : Eff { async: Async } Once(a) =
    atomically(\_ -> { tv <- newTVar(Uninitialized), pure(Once(tv)) });

function callOnce [a:Type] (once: Once(a), init: Unit -> Eff { async: Async } a)
    : Eff { async: Async } a =
    atomically(\_ -> {
        s <- readTVar(once.state);
        match s
        | Done(v) -> pure(v)
        | Running -> retry()     /// another task is initializing, wait
        | Uninitialized -> {
            writeTVar(once.state, Running);
            v <- init(Unit); /// NOTE: IO inside atomically — special case, see §5.3
            writeTVar(once.state, Done(v));
            pure(v)
        }
    });
```

---

## 11. Runtime Implementation

### 11.1 New CLM Nodes

```haskell
-- In src/CLM.hs, add to CLMExpr:

| CLMTASK CLMExpr                            -- A suspended/running concurrent task
| CLMCHAN                                     -- A channel handle (opaque)
| CLMTVAR                                     -- A transactional variable handle (opaque)
| CLMSCOPE CLMExpr Name [(Name, CLMExpr)] [(Name, CLMExpr)]
    -- Like CLMHANDLE but with scope-exit cleanup semantics
```

### 11.2 Interpreter Changes (src/Interpreter.hs)

```haskell
-- New dispatch function in the CLMIAP chain:
-- Position: after dispatchHandlerOp, before dispatchIOIntrinsic

dispatchAsyncIntrinsic :: Name -> [CLMExpr] -> IntState (Maybe CLMExpr)
dispatchAsyncIntrinsic "fork" [body] = do
    -- Haskell's Control.Concurrent.Async
    asyncHandle <- liftIO $ async (runBody body)
    return $ Just (CLMTASK asyncHandle)

dispatchAsyncIntrinsic "await" [CLMTASK handle] = do
    result <- liftIO $ wait handle
    return $ Just result

dispatchAsyncIntrinsic "cancel" [CLMTASK handle] = do
    liftIO $ cancel handle
    return $ Just CLMUNIT

dispatchAsyncIntrinsic "yield" [] = do
    liftIO $ threadDelay 0   -- yield to scheduler
    return $ Just CLMUNIT

dispatchAsyncIntrinsic _ _ = return Nothing

-- Channel dispatch
dispatchChanIntrinsic :: Name -> [CLMExpr] -> IntState (Maybe CLMExpr)
dispatchChanIntrinsic "newChan" [] = do
    ch <- liftIO newTChanIO
    ref <- liftIO $ newIORef ch
    return $ Just (CLMCHAN ref)
-- ... send, recv, tryRecv, close

-- STM dispatch
dispatchSTMIntrinsic :: Name -> [CLMExpr] -> IntState (Maybe CLMExpr)
dispatchSTMIntrinsic "newTVar" [val] = do
    tv <- liftIO $ newTVarIO val
    return $ Just (CLMTVAR tv)
-- ... readTVar, writeTVar, retry, orElse
```

### 11.3 InterpreterState Changes (src/State.hs)

```haskell
data InterpreterState = InterpreterState {
    -- ... existing fields ...

    -- Concurrency state
    , taskRegistry   :: IORef (HashMap TaskId (Async CLMExpr))  -- all live tasks
    , scopeStack     :: IORef [(ScopeId, [TaskId])]             -- structured scope tracking
    , stmEnabled     :: Bool                                     -- STM runtime loaded
    }
```

### 11.4 Updated Dispatch Chain

```
CLMIAP (CLMID funcNm) args:
  1. Handler stack (dispatchHandlerOp)        -- existing
  2. Effect sequencing (dispatchEffectSequencing)  -- existing
  3. Async intrinsics (dispatchAsyncIntrinsic) -- NEW
  4. Channel intrinsics (dispatchChanIntrinsic) -- NEW
  5. STM intrinsics (dispatchSTMIntrinsic)     -- NEW
  6. IO intrinsics (dispatchIOIntrinsic)       -- existing
  7. Ref intrinsics (dispatchRefIntrinsic)     -- existing
  8. MutArray intrinsics (dispatchMutArrayIntrinsic)  -- existing
  9. Reflection intrinsics                     -- existing
  10. Downcast intrinsic                       -- existing
  11. Array HOFs                               -- existing
  12. Type inference + instance dispatch       -- existing
```

---

## 12. Codegen Target Mapping

### 12.1 JavaScript Target

| tulam | JavaScript |
|-------|-----------|
| `Task(a)` | `Promise<A>` |
| `fork(body)` | `body()` (already returns Promise in async context) |
| `await(task)` | `await task` |
| `cancel(task)` | `AbortController.abort()` |
| `yield()` | `await new Promise(r => setTimeout(r, 0))` |
| `Channel(a)` | Custom `Channel` class (array + promise queue) |
| `send(ch, v)` | `await ch.send(v)` |
| `recv(ch)` | `await ch.recv()` |
| `TVar(a)` | Not supported (single-threaded) — use simple Ref |
| `atomically(f)` | `f()` (no concurrency conflicts possible) |
| `withScope(body)` | `Promise.allSettled` + cleanup |

### 12.2 .NET Target

| tulam | .NET (C#) |
|-------|-----------|
| `Task(a)` | `Task<A>` |
| `fork(body)` | `Task.Run(() => body())` |
| `await(task)` | `await task` |
| `cancel(task)` | `CancellationTokenSource.Cancel()` |
| `yield()` | `await Task.Yield()` |
| `Channel(a)` | `System.Threading.Channels.Channel<A>` |
| `send(ch, v)` | `await ch.Writer.WriteAsync(v)` |
| `recv(ch)` | `await ch.Reader.ReadAsync()` |
| `TVar(a)` | Custom STM library or `Interlocked` operations |
| `atomically(f)` | Custom STM runtime (lock-based fallback) |
| `withScope(body)` | `Task.WhenAll` + `CancellationToken` linking |

### 12.3 Native Target

| tulam | Native (x86/ARM) |
|-------|-------------------|
| `Task(a)` | Work-stealing scheduler task descriptor |
| `fork(body)` | Submit to thread pool work queue |
| `await(task)` | Block on task completion flag (or work-steal while waiting) |
| `Channel(a)` | Lock-free MPMC queue |
| `TVar(a)` | Versioned cell with compare-and-swap |
| `atomically(f)` | Optimistic concurrency with version validation |
| `withScope(body)` | Scope descriptor with child task list + cleanup |

---

## 13. Future Extensions

### 13.1 Session Types (Requires Linear Types)

Session types encode communication protocols in the type system. This ensures that a client and server follow the same protocol at compile time.

```tulam
/// Session type constructors
type Send(a: Type, s: Session) = Send;   /// send a value of type a, then continue with session s
type Recv(a: Type, s: Session) = Recv;   /// receive a value of type a, then continue with session s
type Choose(s1: Session, s2: Session) = Choose;  /// offer a choice between two continuations
type Offer(s1: Session, s2: Session) = Offer;    /// accept a choice from the other party
type End = End;                                    /// session complete

/// Example protocol: client sends a query, server responds with result
type QueryProtocol = Send(String, Recv(Int, End));

/// The dual of a session type (what the other party sees)
type Dual(Send(a, s)) = Recv(a, Dual(s));
type Dual(Recv(a, s)) = Send(a, Dual(s));
type Dual(End) = End;
```

This requires **linear types** (each channel endpoint used exactly once) to be sound. Linear types are a significant type system extension that should be designed separately.

### 13.2 Dataflow / Reactive Streams

```tulam
/// A reactive stream: push-based, backpressure-aware
effect Stream(a: Type) = {
    function emit(val: a) : Unit;
    function complete() : Unit;
    function fail(err: String) : Unit
};

/// Stream combinators
function streamMap(f: a -> b, source: Stream(a)) : Stream(b);
function streamFilter(pred: a -> Bool, source: Stream(a)) : Stream(a);
function streamMerge(s1: Stream(a), s2: Stream(a)) : Stream(a);
function streamBuffer(n: Int, source: Stream(a)) : Stream(Array(a));
```

### 13.3 Distributed Concurrency (Future)

```tulam
/// Remote task execution
effect Distributed = {
    function remoteSpawn(node: NodeId, body: Unit -> a) : Task(a);
    function localNodeId() : NodeId;
    function send(node: NodeId, msg: Serializable(a)) : Unit
};
```

This would require serialization support and network IO, both significant extensions.

---

## 14. Implementation Phases

### Phase C1: Core Async Effect + Task Primitive
**Goal**: Enable writing concurrent code that runs sequentially (for testing/design).
- Add `Task` primitive type (parser, environment)
- Add `Async` effect declaration to `lib/Effect/Async.tl`
- Implement `SequentialAsync` handler (no real concurrency — `fork` runs immediately)
- Add `CLMTASK` to CLM node types
- Tests: sequential execution of concurrent code patterns

### Phase C2: Green Threads (Interpreter)
**Goal**: Real parallelism in the interpreter.
- Add `dispatchAsyncIntrinsic` to interpreter dispatch chain
- Implement `fork` via Haskell `Control.Concurrent.Async`
- Implement `await` via Haskell `wait`
- Implement `cancel` via Haskell `cancel`
- Implement `yield` via `threadDelay 0`
- Add `taskRegistry` to `InterpreterState`
- Add `GreenThreads` handler
- Tests: concurrent execution, fork/await, cancellation

### Phase C3: Structured Concurrency
**Goal**: Scoped task lifetimes, no leaked tasks.
- Add `Scope` effect and `withScope` combinator
- Implement scope tracking in `InterpreterState` (`scopeStack`)
- Scope exit cleanup: cancel + await all child tasks
- Linked tasks: sibling cancellation on failure
- Exception propagation from child to parent scope
- Tests: scope cleanup, linked cancellation, nesting

### Phase C4: Channels
**Goal**: CSP-style typed communication.
- Add `Channel`, `RecvChan`, `SendChan` primitive types
- Add `CLMCHAN` to CLM node types
- Add `dispatchChanIntrinsic` to interpreter
- Implement `newChan`, `newBufferedChan`, `send`, `recv`, `tryRecv`, `trySend`, `close`
- Implement `select` for multiplexing
- Add `lib/Effect/Chan.tl` and `lib/Concurrent/Channel.tl`
- Tests: unbuffered/buffered channels, select, producer-consumer

### Phase C5: Parallel Combinators
**Goal**: High-level ergonomic parallelism.
- Implement `par`, `pmap`, `pmapArray`, `raceList`, `awaitAll`, `awaitAny`, `withTimeout`
- Implement `pmapBounded` with semaphore-based concurrency limiting
- Add `lib/Concurrent/Parallel.tl`
- Tests: parallel map, race, timeout, bounded parallelism

### Phase C6: STM
**Goal**: Composable shared mutable state.
- Add `TVar` primitive type and `CLMTVAR` CLM node
- Add `dispatchSTMIntrinsic` to interpreter
- Implement `newTVar`, `readTVar`, `writeTVar` with transaction log
- Implement `retry` (block until read TVar changes)
- Implement `orElse` (alternative transactions)
- Implement `atomically` with optimistic concurrency + automatic retry on conflict
- Add `lib/Effect/STM.tl`
- Tests: atomic transfers, retry blocking, orElse composition, concurrent counter

### Phase C7: Synchronization Library
**Goal**: Standard concurrency primitives built on STM.
- Implement `Semaphore`, `Mutex`, `Barrier`, `CountDownLatch`, `Once`
- Implement `ReadWriteLock` (multiple readers OR single writer)
- Add `lib/Concurrent/Semaphore.tl`, `lib/Concurrent/Mutex.tl`, `lib/Concurrent/Barrier.tl`
- Tests: bounded resource access, mutual exclusion, barrier synchronization

### Phase C8: Actor Library
**Goal**: Actor pattern as a library on classes + channels.
- Implement `Actor` abstract class, `ActorRef` type
- Implement `spawnActor`, `tell`
- Add `lib/Concurrent/Actor.tl`
- Tests: counter actor, ping-pong actors, supervisor pattern

### Phase C9: JS Codegen Async
**Goal**: Compile concurrent tulam to JavaScript async/await.
- `Task(a)` → `Promise<A>`
- `fork` → async function call
- `await` → JS `await`
- `withScope` → `Promise.allSettled` + cleanup
- Channel → custom JS `Channel` class
- Tests: generated JS runs correctly in Node.js

### Phase C10: .NET Codegen Async
**Goal**: Compile concurrent tulam to .NET Task-based async.
- `Task(a)` → `Task<A>`
- `fork` → `Task.Run`
- `await` → C# `await`
- Channel → `System.Threading.Channels`
- Tests: generated C# compiles and runs correctly

### Phase C11: Session Types (Long-term)
**Goal**: Type-safe communication protocols.
- Requires: linear/uniqueness type system extension (separate design doc)
- Session type constructors (Send, Recv, Choose, Offer, End)
- Dual type computation
- Channel endpoint linearity checking
- Tests: protocol compliance at compile time

---

## 15. Design Decisions Summary

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Concurrency model | Effect-based | Composes with existing effects, type-tracked, handler-swappable |
| Task lifetimes | Structured (scoped) | Prevents resource leaks, enables local reasoning |
| Cancellation | Cooperative | Safe, predictable, fits effect handler boundaries |
| Shared state | STM (not locks) | Composable, deadlock-free, correct by construction |
| Communication | Typed channels | Type-safe, bounded backpressure, integrates with select |
| Actors | Library pattern | Not a primitive — built on classes + channels, keeps core small |
| Default runtime | Green threads | Lightweight, supports millions of tasks, good for structured concurrency |
| Parallelism API | Combinators (`par`, `pmap`) | High-level, declarative, hides scheduling details |
| JS mapping | Promise/async-await | Native platform model, zero abstraction overhead |
| .NET mapping | Task/async-await | Native platform model, zero abstraction overhead |

---

## 16. References

- [Structured Concurrency (Nathaniel J. Smith / Trio)](https://vorpus.org/blog/notes-on-structured-concurrency-or-go-statement-considered-harmful/)
- [JEP 453: Structured Concurrency (Java)](https://openjdk.org/jeps/453)
- [Kotlin Coroutines: Structured Concurrency](https://kotlinlang.org/docs/coroutines-basics.html)
- [Effect Handlers in Scope (Wu, Schrijvers, Hinze)](https://www.cs.ox.ac.uk/people/nicolas.wu/papers/Scope.pdf)
- [Algebraic Effects for the Rest of Us (Overreacted)](https://overreacted.io/algebraic-effects-for-the-rest-of-us/)
- [Composable Memory Transactions (Harris, Marlow, Peyton Jones, Herlihy)](https://research.microsoft.com/en-us/um/people/siMDH/papers/stm.pdf)
- [Koka Language — Effect Handlers](https://koka-lang.github.io/koka/doc/index.html)
- [OCaml 5 Effects Tutorial](https://github.com/ocaml-multicore/ocaml-effects-tutorial)
- [Concurrent ML (Reppy)](https://www.cs.uchicago.edu/~jhr/papers/2009/jfp-cml.pdf)
- See also: `doc/EffectDesign.md`, `doc/ClassDesign.md`, `doc/ImplementationPlan.md`
