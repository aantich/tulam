/// Umbrella module that re-exports all standard library modules.
///
/// Loading Base.tl brings every standard library module into scope.
/// The REPL loads this automatically via `loadModuleTree "lib/Base.tl"`.
///
/// Layer order:
///   0. Prelude      — primitive type declarations (loaded separately)
///   1. Core         — algebraic data types + combinators
///   2. Reflection   — runtime type introspection + structural helpers
///   3. Algebra      — core algebraic structures (typeclasses)
///   4. String       — string algebras (StringOps, StringExt, StringLike)
///   5. Morphism     — type conversion patterns
///   6. Instances    — algebra instances for all types
///   7. Categorical  — category theory (Category, Arrow, Functor, Applicative, Monad)
///   8. Collection   — container algebras (Foldable through Sliceable)
///   9. Effect       — effect declarations and handlers
///  10. Mutable      — mutable state primitives
///  11. SIMD         — vector types and lane operations
///  12. Repr         — representation mappings
///  13. Str          — UTF-8 string type
///  14. Math         — numeric, linear algebra, statistics, transforms,
///                     optimization, random number generation
module Base;

export Core;
export Reflection;
export Algebra;
export String;
export Morphism;
export Instances;
export Categorical;
export Collection;
export Effect;
export Mutable;
export SIMD;
export Repr;
export Str;
export Math.Numeric;
export Math.LinAlg;
export Math.Stats;
export Math.Transform;
export Math.Optimize;
export Math.Random;
