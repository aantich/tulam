/// Umbrella module that re-exports all standard library modules.
///
/// Loading Base.tl brings every standard library module into scope.
/// The REPL loads this automatically via `loadModuleTree "lib/Base.tl"`.
module Base;

export Core;
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
