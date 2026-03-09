/// Umbrella module that re-exports all standard library modules.
///
/// Loading Base.tl brings every standard library module into scope.
/// The REPL loads this automatically via `loadModuleTree "lib/Base.tl"`.
///
/// Layer order:
///   0. Prelude      — primitive type declarations (loaded separately)
///   1. Core         — algebraic data types + combinators
///   2. Reflection   — runtime type introspection + structural helpers
///   3. Algebra      — algebraic structures (typeclasses)
///   4. Morphism     — type conversion patterns
///   5. Instances    — algebra instances for all types
///   6. HKT          — higher-kinded types (Functor, Applicative, Monad)
///   7. Collection   — Array operations
///   8. Effect       — effect declarations and handlers
///   9. Mutable      — mutable state primitives
///  10. SIMD         — vector types and lane operations
///  11. Repr         — representation mappings
///  12. Str          — UTF-8 string type
module Base;

export Core;
export Reflection;
export Algebra;
export Morphism;
export Instances;
export HKT;
export Collection;
export Effect;
export Mutable;
export SIMD;
export Repr;
export Str;
