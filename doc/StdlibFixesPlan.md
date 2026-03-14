# Standard Library Fixes Plan

## Overview

35 fixes across 14 files identified during comprehensive stdlib review. Covers bugs, missing instances, and operator aliases.

## Operator Aliases (4 new fixity groups)

| Algebra | Operator | Fixity | Definition |
|---------|----------|--------|------------|
| Semigroup | `(<>)` | `infixr 5` | `= combine(x, y)` |
| Functor | `(<$>)` | `infixl 4` | `= fmap(f, x)` |
| Applicative | `(<*>)` | `infixl 4` | `= ap(ff, fa)` |
| Monad | `(>>=)` | `infixl 1` | `= bind(x, f)` |
| Monad | `(>>)` | `infixl 1` | `= seq(x, y)` |
| Category | `(>>>)` | `infixr 1` | `= ccompose(g, f)` (left-to-right) |
| Category | `(<<<)` | `infixr 9` | `= ccompose(f, g)` (right-to-left) |

## Bug Fixes (10)

### Algebra.tl — Remove 3 laws referencing non-superclass operations

1. **Bounded law** uses `<=` (Ord) but doesn't extend Ord → remove law
2. **Hashable law** uses `==` (Eq) but doesn't extend Eq → remove law
3. **Absolute decomposition law** uses `*` (MultiplicativeSemigroup) but is standalone → remove law

### Categorical.tl

4. **Add `import Algebra;`** — List Monad uses `combine` from Semigroup
5. **Fix `mapM_` base case** — returns bare `Unit` instead of `pure(Unit)`
6. **Add `Applicative(Maybe)` and `Applicative(List)` instances** — Monad extends Applicative but no instances declared

### Collection.tl

7. **Remove Filterable `filterNone` law** — uses `isEmpty` from Sized but Filterable only extends Foldable

### Core.tl

8. **Fix `compose` signature**: `(f:b, g:a, x:a) : c` → `(f:b -> c, g:a -> b, x:a) : c`
9. **Fix `apply` signature**: `(f:a, x:b) : c` → `(f:a -> b, x:a) : b`
10. **Fix `flip` signature**: `(f:a, x:b, y:c)` → `(f:a -> b -> c, x:b, y:a) : c`

### Instance Files

11. **List.tl `partition`** — uses `fst`/`snd` without importing Pair → use pattern match instead
12. **Utility.tl `doubleNat`** — computes 4x not 2x → `plus(x, x)` not `plus(twice, twice)`
13. **NonEmpty.tl `singleton`** — name clash with `Buildable.singleton` → rename to `neSingleton`

## Missing Instances (21)

### Eq instances (5)
- `Eq(List(a))` — recursive element comparison
- `Eq(Maybe(a))` — Nothing==Nothing, Just(x)==Just(y) when x==y
- `Eq(Either(a,b))` — Left==Left, Right==Right when inner ==
- `Eq(NonEmpty(a))` — head + tail comparison
- `Eq(Tree(a))` — Leaf/Branch recursive

### Ord instances (5)
- `Ord(Bool)` — False < True
- `Ord(Maybe(a))` — Nothing < Just
- `Ord(Either(a,b))` — Left < Right
- `Ord(List(a))` — lexicographic
- `Ord(Pair(a,b))` — lexicographic

### Hashable instances (2)
- `Hashable(Maybe(a))` — 0 for Nothing, 31+hash(v) for Just(v)
- `Hashable(Pair(a,b))` — hash(x)*31 + hash(y)

### Semigroup/Monoid (2)
- `Semigroup(Maybe(a))` — Nothing is identity, Just combines inner
- `Monoid(Maybe(a))` — empty = Nothing

### Collection instances for List (4)
- `Searchable(List)` — find, any, all, elem, count
- `Sortable(List)` — merge sort via sortBy
- `Zippable(List)` — zipWith, zip, unzip
- `Sliceable(List)` — take, drop, splitAt, takeWhile, dropWhile, scanl

### HKT instances (3)
- `Functor(Tree)` — map over leaves
- `Foldable(Tree)` — fold over leaves
- `Functor(Either(e))` — map over Right branch

### Numeric completeness (4+)
- `Bounded(Int)` = intrinsic
- `Bounded(UInt)` = intrinsic
- `Show(UInt)` = intrinsic
- `Hashable(UInt)`, `Enum(UInt)` = intrinsic

## Implementation Order

1. Operator aliases (Algebra.tl, Categorical.tl)
2. Bug fixes (all files)
3. Missing Eq/Ord instances
4. Missing collection instances for List
5. Missing HKT instances
6. Missing numeric instances
7. Run full test suite, verify no regressions
