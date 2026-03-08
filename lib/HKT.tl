/// Higher-kinded type algebras: Functor, Applicative, Monad.
///
/// Also includes instances for Maybe and List, and monadic traversal.
module HKT;
import Core;

// ============================================================================
// Functor
// ============================================================================

/// Functor algebra for type constructors (Type1).
algebra Functor(f:Type1) = {
    function fmap(g: a -> b, x:f(a)) : f(b);

    law identity(x:f(a)) = fmap(id, x) === x;
    law composition(f:b -> c, g:a -> b, x:f(a)) =
        fmap(compose(f, g), x) === fmap(f, fmap(g, x))
};

// ============================================================================
// Applicative
// ============================================================================

/// Applicative functor algebra.
algebra Applicative(f:Type1) extends Functor(f) = {
    function pure(x:a) : f(a);
    function ap(ff:f(a -> b), fa:f(a)) : f(b)
};

// ============================================================================
// Monad
// ============================================================================

/// Monad algebra for sequential computation.
algebra Monad(m:Type1) extends Applicative(m) = {
    function bind(x:m(a), f: a -> m(b)) : m(b);
    function seq(x:m(a), y:m(b)) : m(b);

    law left_identity(x:a, f: a -> m(b))  = bind(pure(x), f) === f(x);
    law right_identity(x:m(a)) = bind(x, pure) === x;
    law associativity(x:m(a), f: a -> m(b), g: b -> m(c)) =
        bind(bind(x, f), g) === bind(x, \y -> bind(f(y), g))
};

// ============================================================================
// Instances
// ============================================================================

instance Functor(Maybe) = {
    function fmap(g, x) = match
        | g, Nothing -> Nothing
        | g, Just(v) -> Just(g(v))
};

instance Functor(List) = {
    function fmap(g, xs) = match
        | g, Nil -> Nil
        | g, Cons(h, t) -> Cons(g(h), fmap(g, t))
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
// Traverse (Maybe-specialized)
// ============================================================================

/// Map a Maybe-returning function over a list, collecting results.
function mapMMaybe(f, xs:List(a)) = match
    | f, Nil -> Just(Nil)
    | f, Cons(h, t) -> bind(f(h), \bh -> bind(mapMMaybe(f, t), \bt -> Just(Cons(bh, bt))));

/// Fold a list with a Maybe-returning accumulator.
function foldMMaybe(f, acc, xs:List(a)) = match
    | f, acc, Nil -> Just(acc)
    | f, acc, Cons(h, t) -> bind(f(acc, h), \acc2 -> foldMMaybe(f, acc2, t));

/// Sequence a list of Maybe values into a Maybe of list.
function sequenceMaybe(xs:List(Maybe(a))) = match
    | Nil -> Just(Nil)
    | Cons(h, t) -> bind(h, \v -> bind(sequenceMaybe(t), \vs -> Just(Cons(v, vs))));

/// mapMMaybe with arguments flipped.
function forMMaybe(xs:List(a), f) = mapMMaybe(f, xs);

/// Map a monadic function over a list, discarding results.
function mapM_(f, xs:List(a)) : Unit = match
    | f, Nil -> Unit
    | f, Cons(h, t) -> seq(f(h), mapM_(f, t));
