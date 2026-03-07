/// Umbrella module that re-exports all standard library modules.
///
/// Loading Base.tl brings every standard library module into scope.
/// The REPL loads this automatically via `loadModuleTree "lib/Base.tl"`.
module Base;

// Algebra foundations
export Algebra.Semigroup;
export Algebra.Additive;
export Algebra.Multiplicative;
export Algebra.Ring;
export Algebra.Floating;
export Algebra.Absolute;
export Algebra.Bits;
export Algebra.StringOps;
export Algebra.StringExt;
export Algebra.Eq;
export Algebra.Ord;
export Algebra.Show;
export Algebra.Bounded;
export Algebra.Enum;
export Algebra.Default;
export Algebra.Hashable;
export Algebra.Lattice;

// Core types
export Core.Bool;
export Core.Ordering;
export Core.Nat;
export Core.Maybe;
export Core.Either;
export Core.List;
export Core.Combinators;
export Core.Unit;
export Core.Void;
export Core.Pair;
export Core.NonEmpty;
export Core.Tree;
export Core.Reflect;
export Core.Instances;

// Numeric instances
export Numeric.Int;
export Numeric.Int.Fixed;
export Numeric.UInt;
export Numeric.Float;
export Numeric.Conversion;
export Numeric.String;
export Numeric.Char;

// Higher-kinded type algebras
export HKT.Functor;
export HKT.Applicative;
export HKT.Monad;
export HKT.Instances;
export HKT.Traverse;

// Morphisms
export Morphism.Convertible;
export Morphism.Iso;
export Morphism.Embed;

// Collections
export Collection.Array;

// SIMD
export SIMD.Vec;
export SIMD.Lane;

// Cross-cutting instances
export Instances.Show;
export Instances.Bounded;
export Instances.Enum;
export Instances.Misc;

// Effects
export Effect.Console;
export Effect.FileIO;
export Effect.State;
export Effect.Exception;

// Mutable state
export Mutable.Ref;
export Mutable.MutArray;

// Repr
export Repr.Nat;
// Repr.Char disabled: fromRepr keyed by ["Int"] collides with Repr.Nat.
// Char<->Int conversion available via fromEnum/toEnum.
// TODO: fix repr dispatch to support multiple fromRepr instances (type-directed)
// export Repr.Char;

// String library
export Algebra.StringLike;
export String.Utf8;
export String.Str;
export String.Algebras;
export String.Instances;
export String.Ops;
export String.FromString;
