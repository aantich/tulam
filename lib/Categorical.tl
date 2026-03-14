/// Category theory foundations.
///
/// Core algebraic structures from category theory that underpin the tulam
/// type system. All other algebras (collections, math, effects) build on these.
///
/// Hierarchy:
///   Category(arr: Type -> Type -> Type) — composable morphisms
///   Arrow(arr) extends Category — lifted computation with products
///   ArrowChoice(arr) extends Arrow — lifted computation with coproducts
///   ArrowApply(arr) extends Arrow — self-applying arrows (Monad-equivalent)
///   Functor(f: Type -> Type) — structure-preserving maps
///   Bifunctor(p: Type -> Type -> Type) — maps over two type params
///   Contravariant(f: Type -> Type) — reverses the arrow
///   Profunctor(p: Type -> Type -> Type) — contra first, co second
///   Applicative(f) extends Functor — independent effects
///   Alternative(f) extends Applicative — monoid on applicatives
///   Monad(m) extends Applicative — sequential effects
///   MonadPlus(m) extends Monad, Alternative — filtering + failure
///
/// See doc/CategoricalDesign.md for the full theoretical motivation.
module Categorical;
import Core;
import Algebra;

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
/// @param arr A binary type constructor (Type -> Type -> Type)
algebra Arrow(arr: Type -> Type -> Type) extends Category(arr) = {
    /// Lift a pure function into the arrow.
    function arrlift(f: a -> b) : arr(a, b);
    /// Apply an arrow to the first component of a pair, passing the second through.
    function first(f:arr(a, b)) : arr(Pair(a, c), Pair(b, c));
    /// Apply an arrow to the second component of a pair.
    function second(f:arr(a, b)) : arr(Pair(c, a), Pair(c, b));
    /// Apply two arrows in parallel to a pair.
    function arrsplit(f:arr(a, b), g:arr(c, d)) : arr(Pair(a, c), Pair(b, d));
    /// Apply two arrows to the same input, collecting both outputs.
    function fanout(f:arr(a, b), g:arr(a, c)) : arr(a, Pair(b, c));

    // Defaults
    function second(f:arr(a, b)) : arr(Pair(c, a), Pair(c, b)) =
        let swap = arrlift(fn(p) = Pair(p.snd, p.fst)) in
        ccompose(swap, ccompose(first(f), swap));
    function arrsplit(f:arr(a, b), g:arr(c, d)) : arr(Pair(a, c), Pair(b, d)) =
        ccompose(second(g), first(f));
    function fanout(f:arr(a, b), g:arr(a, c)) : arr(a, Pair(b, c)) =
        ccompose(arrsplit(f, g), arrlift(fn(x) = Pair(x, x)));

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
    /// Choose between two arrows based on input.
    function fanin(f:arr(a, c), g:arr(b, c)) : arr(Either(a, b), c);
    /// Route inputs to different arrows based on a predicate.
    function choose(f:arr(a, b), g:arr(c, d)) : arr(Either(a, c), Either(b, d));

    // Defaults
    function right(f:arr(a, b)) : arr(Either(c, a), Either(c, b)) =
        let mirror = arrlift(fn(e) = match | Left(x) -> Right(x) | Right(x) -> Left(x)) in
        ccompose(mirror, ccompose(left(f), mirror));
    function choose(f:arr(a, b), g:arr(c, d)) : arr(Either(a, c), Either(b, d)) =
        ccompose(right(g), left(f))
};

/// ArrowApply: arrows that can apply themselves. Equivalent to Monad in power.
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
/// @param f A unary type constructor (Type -> Type)
algebra Functor(f: Type -> Type) = {
    /// Lift a function into the functor.
    function fmap(g: a -> b, x:f(a)) : f(b);

    law identity(x:f(a)) = fmap(id, x) === x;
    law composition(g:b -> c, h:a -> b, x:f(a)) =
        fmap(compose(g, h), x) === fmap(g, fmap(h, x))
};

// ============================================================================
// Bifunctor
// ============================================================================

/// A Bifunctor maps over both type parameters of a binary type constructor.
///
/// Instances: Pair, Either.
///
/// @param p A binary type constructor (Type -> Type -> Type)
algebra Bifunctor(p: Type -> Type -> Type) = {
    /// Map over both type parameters simultaneously.
    function bimap(f: a -> c, g: b -> d, x:p(a, b)) : p(c, d);
    /// Map over the first type parameter only.
    function mapFirst(f: a -> c, x:p(a, b)) : p(c, b);
    /// Map over the second type parameter only.
    function mapSecond(g: b -> d, x:p(a, b)) : p(a, d);

    // Defaults
    function mapFirst(f: a -> c, x:p(a, b)) : p(c, b) = bimap(f, id, x);
    function mapSecond(g: b -> d, x:p(a, b)) : p(a, d) = bimap(id, g, x);

    law bimapId(x:p(a, b)) = bimap(id, id, x) === x;
    law bimapCompose(f1:a -> b, f2:b -> c, g1:d -> e, g2:e -> f, x:p(a, d)) =
        bimap(compose(f2, f1), compose(g2, g1), x) === bimap(f2, g2, bimap(f1, g1, x))
};

// ============================================================================
// Contravariant
// ============================================================================

/// A Contravariant functor reverses the direction of morphisms.
///
/// Where Functor lifts `a -> b` to `f(a) -> f(b)` (covariant),
/// Contravariant lifts `b -> a` to `f(a) -> f(b)` (contravariant).
///
/// Instances: predicates (`a -> Bool`), comparators (`a -> a -> Ordering`).
///
/// @param f A unary type constructor (Type -> Type)
algebra Contravariant(f: Type -> Type) = {
    /// Lift a function contravariantly.
    function contramap(g: b -> a, x:f(a)) : f(b);

    law contramapId(x:f(a)) = contramap(id, x) === x;
    law contramapCompose(f:b -> a, g:c -> b, x:f(a)) =
        contramap(compose(f, g), x) === contramap(g, contramap(f, x))
};

// ============================================================================
// Profunctor
// ============================================================================

/// A Profunctor is contravariant in the first argument and covariant
/// in the second. The foundation for optics (lenses, prisms).
///
/// The function arrow `a -> b` is the canonical Profunctor.
///
/// @param p A binary type constructor (Type -> Type -> Type)
algebra Profunctor(p: Type -> Type -> Type) = {
    /// Map both sides: contravariant first, covariant second.
    function dimap(f: a2 -> a1, g: b1 -> b2, x:p(a1, b1)) : p(a2, b2);
    /// Map only the input (contravariantly).
    function lmap(f: a2 -> a1, x:p(a1, b)) : p(a2, b);
    /// Map only the output (covariantly).
    function rmap(g: b1 -> b2, x:p(a, b1)) : p(a, b2);

    // Defaults
    function lmap(f: a2 -> a1, x:p(a1, b)) : p(a2, b) = dimap(f, id, x);
    function rmap(g: b1 -> b2, x:p(a, b1)) : p(a, b2) = dimap(id, g, x);

    law dimapId(x:p(a, b)) = dimap(id, id, x) === x;
    law dimapCompose(f1:b -> a, f2:c -> b, g1:d -> e, g2:e -> f, x:p(a, d)) =
        dimap(compose(f1, f2), compose(g2, g1), x) === dimap(f2, g2, dimap(f1, g1, x))
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
// Alternative
// ============================================================================

/// Monoid on applicative functors: choice and failure.
///
/// `aempty` represents failure/no result. `alt` picks the first success.
/// The parsing combinator library's foundation.
///
/// Instances: Maybe (first Just wins), List (concatenation).
///
/// @param f A unary type constructor (Type -> Type)
algebra Alternative(f: Type -> Type) extends Applicative(f) = {
    /// The identity of `alt`: failure / empty.
    function aempty() : f(a);
    /// Choose between two alternatives: try x, if it fails try y.
    function alt(x:f(a), y:f(a)) : f(a);

    law altAssociativity(x:f(a), y:f(a), z:f(a)) =
        alt(alt(x, y), z) === alt(x, alt(y, z));
    law altLeftIdentity(x:f(a)) = alt(aempty(), x) === x;
    law altRightIdentity(x:f(a)) = alt(x, aempty()) === x
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
// MonadPlus
// ============================================================================

/// Monad with choice and failure. Combines Monad's sequencing with
/// Alternative's failure recovery.
///
/// Enables guard-based filtering in monadic computations.
///
/// @param m A unary type constructor (Type -> Type)
algebra MonadPlus(m: Type -> Type) extends Monad(m), Alternative(m) = {
    /// Conditionally fail: if b is False, produce aempty.
    function guard(b:Bool) : m(Unit);
    /// Filter a monadic value by a predicate.
    function mfilter(p: a -> Bool, x:m(a)) : m(a);

    // Defaults
    function guard(b:Bool) : m(Unit) = if b then pure(Unit) else aempty();
    function mfilter(p: a -> Bool, x:m(a)) : m(a) =
        bind(x, fn(a) = if p(a) then pure(a) else aempty());

    law mfilterTrue(x:m(a)) = mfilter(fn(a) = True, x) === x;
    law guardTrue() = guard(True) === pure(Unit)
};

// ============================================================================
// Operators
// ============================================================================

/// Functor map operator: `f <$> x` = `fmap(f, x)`
infixl 4 (<$>);
function (<$>)(f: a -> b, x:c(a)) : c(b) = fmap(f, x);

/// Applicative apply operator: `ff <*> fa` = `ap(ff, fa)`
infixl 4 (<*>);
function (<*>)(ff:c(a -> b), fa:c(a)) : c(b) = ap(ff, fa);

/// Monadic bind operator: `x >>= f` = `bind(x, f)`
infixl 1 (>>=);
function (>>=)(x:m(a), f: a -> m(b)) : m(b) = bind(x, f);

/// Monadic sequence operator: `x >> y` = `seq(x, y)`
infixl 1 (>>);
function (>>)(x:m(a), y:m(b)) : m(b) = seq(x, y);

/// Category left-to-right composition: `f >>> g` = `ccompose(g, f)`
infixr 1 (>>>);
function (>>>)(f:arr(a, b), g:arr(b, c)) : arr(a, c) = ccompose(g, f);

/// Category right-to-left composition: `f <<< g` = `ccompose(f, g)`
infixr 1 (<<<);
function (<<<)(f:arr(b, c), g:arr(a, b)) : arr(a, c) = ccompose(f, g);

/// Arrow fanout: `f &&& g` = `fanout(f, g)`
infixr 3 (&&&);
function (&&&)(f:arr(a, b), g:arr(a, c)) : arr(a, Pair(b, c)) = fanout(f, g);

/// Arrow split: `f *** g` = `split(f, g)`
infixr 3 (***);
function (***)(f:arr(a, b), g:arr(c, d)) : arr(Pair(a, c), Pair(b, d)) = arrsplit(f, g);

/// Alternative choice: `x <|> y` = `alt(x, y)`
infixl 3 (<|>);
function (<|>)(x:f(a), y:f(a)) : f(a) = alt(x, y);

// ============================================================================
// Unicode operator aliases
// ============================================================================

/// Unicode bind: `x ≫= f` = `bind(x, f)`
infixl 1 (≫=);
function (≫=)(x:m(a), f: a -> m(b)) : m(b) = bind(x, f);

/// Unicode sequence: `x ≫ y` = `seq(x, y)`
infixl 1 (≫);
function (≫)(x:m(a), y:m(b)) : m(b) = seq(x, y);

/// Unicode category left-to-right: `f ⋙ g` = `ccompose(g, f)`
infixr 1 (⋙);
function (⋙)(f:arr(a, b), g:arr(b, c)) : arr(a, c) = ccompose(g, f);

/// Unicode category right-to-left: `f ⋘ g` = `ccompose(f, g)`
infixr 1 (⋘);
function (⋘)(f:arr(b, c), g:arr(a, b)) : arr(a, c) = ccompose(f, g);
