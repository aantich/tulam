/// Umbrella module that re-exports all standard library modules.
///
/// Loading Base.tl brings every standard library module into scope.
/// The REPL loads this automatically via `loadModuleTree "lib/Base.tl"`.
///
/// Layer order:
///   0. Prelude      — primitive type declarations (loaded separately)
///   1. Core         — algebraic data types + combinators
///   2. Algebra      — core algebraic structures (typeclasses)
///   3. Repr         — representation mappings (Nat as Int, etc.)
///   4. Reflection   — runtime type introspection + structural helpers
///   5. String       — string algebras (StringOps, StringExt, StringLike)
///   6. Morphism     — type conversion patterns
///   7. Instances    — algebra instances for all types
///   8. Categorical  — category theory (Category, Arrow, Functor, Applicative, Monad)
///   9. Collection   — container algebras (Foldable through Sliceable)
///  10. Effect       — effect declarations and handlers
///  11. Mutable      — mutable state primitives
///  12. SIMD         — vector types and lane operations
///  13. Str          — UTF-8 string type
///  14. Math         — numeric, linear algebra, statistics, transforms,
///                     optimization, random number generation
module Base;

export Core;
export Algebra;
export Repr;
export Reflection;
export String;
export Morphism;
export Instances;
export Categorical;
export Collection;
export Effect;
export Mutable;
export SIMD;
export Str;
export Math.Numeric;
export Math.LinAlg;
export Math.Stats;
export Math.Transform;
export Math.Optimize;
export Math.Random;
export System.Time;
