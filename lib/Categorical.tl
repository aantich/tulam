/// Category theory foundations: Category, Arrow, Functor, Applicative, Monad.
///
/// These are the abstract algebraic structures from category theory that
/// underpin the entire tulam type system. All other algebras (collections,
/// math, effects) build on these foundations.
///
/// Hierarchy:
///   Category(arr: Type -> Type -> Type) — composable morphisms
///   Arrow(arr: Type -> Type -> Type) extends Category — lifted computation
///   Functor(f: Type -> Type) — structure-preserving maps
///   Applicative(f: Type -> Type) extends Functor — independent effects
///   Monad(m: Type -> Type) extends Applicative — sequential effects
///
/// See doc/CategoricalDesign.md for the full theoretical motivation.
module Categorical;
import Core;

// ============================================================================
// Category
// ============================================================================

/// A category: objects connected by composable, associative morphisms with identity.
///
/// `arr` is the morphism type constructor: `arr(a, b)` is a morphism from a to b.
/// The plain function type `a -> b` is the default category (Type category).
///
/// @param arr A binary type constructor (Type -> Type -> Type)
algebra Category(arr: Type -> Type -> Type) = {
    /// Identity morphism: the "do nothing" arrow.
    function cid() : arr(a, a);
    /// Compose two morphisms: f after g.
    function ccompose(f:arr(b, c), g:arr(a, b)) : arr(a, c);

    law leftIdentity(f:arr(a, b)) = ccompose(cid(), f) === f;
    law rightIdentity(f:arr(a, b)) = ccompose(f, cid()) === f;
    law associativity(f:arr(c, d), g:arr(b, c), h:arr(a, b)) =
        ccompose(f, ccompose(g, h)) === ccompose(ccompose(f, g), h)
};

// ============================================================================
// Arrow
// ============================================================================

/// An Arrow generalizes functions to computations with structured input/output.
/// Every Arrow is a Category with the ability to lift pure functions and
/// route data through product types.
///
/// Arrows subsume both functions and monadic computations. They are the
/// foundation for dataflow programming, signal processing, and FRP.
///
/// @param arr A binary type constructor (Type -> Type -> Type)
algebra Arrow(arr: Type -> Type -> Type) extends Category(arr) = {
    /// Lift a pure function into the arrow.
    function arrlift(f: a -> b) : arr(a, b);
    /// Apply an arrow to the first component of a pair, passing the second through.
    function first(f:arr(a, b)) : arr(Pair(a, c), Pair(b, c));
    /// Apply an arrow to the second component of a pair.
    function second(f:arr(a, b)) : arr(Pair(c, a), Pair(c, b));
    /// Apply two arrows in parallel to a pair (***).
    function split(f:arr(a, b), g:arr(c, d)) : arr(Pair(a, c), Pair(b, d));
    /// Apply two arrows to the same input, collecting both outputs (&&&).
    function fanout(f:arr(a, b), g:arr(a, c)) : arr(a, Pair(b, c));

    // Defaults
    function second(f:arr(a, b)) : arr(Pair(c, a), Pair(c, b)) =
        let swap = arrlift(fn(p) = Pair(p.snd, p.fst)) in
        ccompose(swap, ccompose(first(f), swap));
    function split(f:arr(a, b), g:arr(c, d)) : arr(Pair(a, c), Pair(b, d)) =
        ccompose(second(g), first(f));
    function fanout(f:arr(a, b), g:arr(a, c)) : arr(a, Pair(b, c)) =
        ccompose(split(f, g), arrlift(fn(x) = Pair(x, x)));

    law arrliftId() = arrlift(id) === cid();
    law arrliftCompose(f:a -> b, g:b -> c) =
        arrlift(fn(x) = g(f(x))) === ccompose(arrlift(g), arrlift(f));
    law firstDistribute(f:arr(a, b), g:arr(b, c)) =
        first(ccompose(g, f)) === ccompose(first(g), first(f))
};

/// ArrowChoice extends Arrow with the ability to route through sum types.
/// Where Arrow handles product (Pair), ArrowChoice handles coproduct (Either).
algebra ArrowChoice(arr: Type -> Type -> Type) extends Arrow(arr) = {
    /// Route an arrow through the Left branch, passing Right through unchanged.
    function left(f:arr(a, b)) : arr(Either(a, c), Either(b, c));
    /// Route an arrow through the Right branch.
    function right(f:arr(a, b)) : arr(Either(c, a), Either(c, b));
    /// Choose between two arrows based on input (|||).
    function fanin(f:arr(a, c), g:arr(b, c)) : arr(Either(a, b), c);
    /// Route inputs to different arrows based on a predicate (+++ generalized).
    function choose(f:arr(a, b), g:arr(c, d)) : arr(Either(a, c), Either(b, d));

    // Defaults
    function right(f:arr(a, b)) : arr(Either(c, a), Either(c, b)) =
        let mirror = arrlift(fn(e) = match | Left(x) -> Right(x) | Right(x) -> Left(x)) in
        ccompose(mirror, ccompose(left(f), mirror));
    function choose(f:arr(a, b), g:arr(c, d)) : arr(Either(a, c), Either(b, d)) =
        ccompose(right(g), left(f))
};

/// ArrowApply: arrows that can apply themselves. Equivalent to Monad in power.
/// Not every Arrow is an ArrowApply — this is strictly more powerful.
algebra ArrowApply(arr: Type -> Type -> Type) extends Arrow(arr) = {
    /// Apply: given an arrow and its input, produce the output.
    function app() : arr(Pair(arr(a, b), a), b)
};

// ============================================================================
// Kleisli arrows
// ============================================================================

/// Kleisli arrow: a function `a -> m(b)` for a monad m.
/// Every monad gives rise to a Category of Kleisli arrows.
type Kleisli(m: Type -> Type, a:Type, b:Type) = KleisliArrow * run: a -> m(b);

// ============================================================================
// Functor
// ============================================================================

/// A Functor maps both objects (types) and morphisms (functions) while
/// preserving composition and identity.
///
/// If `f` is a Functor, then for any function `g: a -> b`,
/// `fmap(g) : f(a) -> f(b)` lifts it into the functor's context.
///
/// @param f A unary type constructor (Type -> Type)
algebra Functor(f: Type -> Type) = {
    /// Lift a function into the functor.
    function fmap(g: a -> b, x:f(a)) : f(b);

    law identity(x:f(a)) = fmap(id, x) === x;
    law composition(g:b -> c, h:a -> b, x:f(a)) =
        fmap(compose(g, h), x) === fmap(g, fmap(h, x))
};

// ============================================================================
// Applicative
// ============================================================================

/// Applicative functor: independent effectful computations with combining.
/// Sits between Functor (one effect) and Monad (sequenced effects).
///
/// Key distinction from Monad: Applicative effects are independent —
/// the second computation doesn't depend on the result of the first.
/// This independence enables parallel execution.
///
/// @param f A unary type constructor (Type -> Type)
algebra Applicative(f: Type -> Type) extends Functor(f) = {
    /// Lift a pure value into the applicative context.
    function pure(x:a) : f(a);
    /// Apply a wrapped function to a wrapped value.
    function ap(ff:f(a -> b), fa:f(a)) : f(b);

    law apIdentity(x:f(a)) = ap(pure(id), x) === x;
    law apHomomorphism(f:a -> b, x:a) = ap(pure(f), pure(x)) === pure(f(x));
    law apComposition(u:f(b -> c), v:f(a -> b), w:f(a)) =
        ap(ap(ap(pure(compose), u), v), w) === ap(u, ap(v, w))
};

// ============================================================================
// Monad
// ============================================================================

/// Monad: sequential computation where each step can depend on previous results.
///
/// The fundamental abstraction for sequencing effects. `bind` (>>=) threads
/// results through a chain of computations. Unlike Applicative, Monad allows
/// the choice of next computation to depend on the value produced so far.
///
/// @param m A unary type constructor (Type -> Type)
algebra Monad(m: Type -> Type) extends Applicative(m) = {
    /// Sequential bind: run x, feed its result to f.
    function bind(x:m(a), f: a -> m(b)) : m(b);
    /// Sequential discard: run x, discard its result, run y.
    function seq(x:m(a), y:m(b)) : m(b);

    // Default
    function seq(x:m(a), y:m(b)) : m(b) = bind(x, fn(_) = y);

    law leftIdentity(x:a, f: a -> m(b))  = bind(pure(x), f) === f(x);
    law rightIdentity(x:m(a)) = bind(x, pure) === x;
    law associativity(x:m(a), f: a -> m(b), g: b -> m(c)) =
        bind(bind(x, f), g) === bind(x, fn(y) = bind(f(y), g))
};

// ============================================================================
// Instances: Maybe
// ============================================================================

instance Functor(Maybe) = {
    function fmap(g, x) = match
        | g, Nothing -> Nothing
        | g, Just(v) -> Just(g(v))
};

instance Monad(Maybe) = {
    function pure(x) = Just(x);
    function bind(x, f) = match
        | Nothing, f -> Nothing
        | Just(v), f -> f(v);
    function ap(ff, fa) = match
        | Nothing, fa -> Nothing
        | Just(g), fa -> fmap(g, fa);
    function seq(x, y) = match
        | Nothing, y -> Nothing
        | Just(v), y -> y
};

// ============================================================================
// Instances: List
// ============================================================================

instance Functor(List) = {
    function fmap(g, xs) = match
        | g, Nil -> Nil
        | g, Cons(h, t) -> Cons(g(h), fmap(g, t))
};

instance Monad(List) = {
    function pure(x) = Cons(x, Nil);
    function bind(xs, f) = match
        | Nil, f -> Nil
        | Cons(h, t), f -> combine(f(h), bind(t, f));
    function ap(ff, fa) = match
        | Nil, fa -> Nil
        | Cons(g, gs), fa -> combine(fmap(g, fa), ap(gs, fa));
    function seq(xs, ys) = match
        | Nil, ys -> Nil
        | Cons(h, t), ys -> combine(ys, seq(t, ys))
};

// ============================================================================
// Kleisli Category instance
// ============================================================================

// instance Category(Kleisli(m)) requires Monad(m: Type -> Type) = {
//     function cid() = KleisliArrow(fn(x) = pure(x));
//     function ccompose(f, g) = KleisliArrow(fn(x) = bind(g.run(x), f.run))
// };

// ============================================================================
// Monadic traversal helpers (Maybe-specialized)
// ============================================================================

/// Map a Maybe-returning function over a list, collecting results.
/// Short-circuits on the first Nothing.
function mapMMaybe(f, xs:List(a)) = match
    | f, Nil -> Just(Nil)
    | f, Cons(h, t) -> bind(f(h), fn(bh) = bind(mapMMaybe(f, t), fn(bt) = Just(Cons(bh, bt))));

/// Fold a list with a Maybe-returning accumulator.
function foldMMaybe(f, acc, xs:List(a)) = match
    | f, acc, Nil -> Just(acc)
    | f, acc, Cons(h, t) -> bind(f(acc, h), fn(acc2) = foldMMaybe(f, acc2, t));

/// Sequence a list of Maybe values into a Maybe of list.
function sequenceMaybe(xs:List(Maybe(a))) = match
    | Nil -> Just(Nil)
    | Cons(h, t) -> bind(h, fn(v) = bind(sequenceMaybe(t), fn(vs) = Just(Cons(v, vs))));

/// mapMMaybe with arguments flipped.
function forMMaybe(xs:List(a), f) = mapMMaybe(f, xs);

/// Map a monadic function over a list, discarding results.
function mapM_(f, xs:List(a)) = match
    | f, Nil -> Unit
    | f, Cons(h, t) -> seq(f(h), mapM_(f, t));
