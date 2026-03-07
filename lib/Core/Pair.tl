/// The canonical product type of two values.
///
/// Pair(a, b) holds exactly one value of type a and one value of type b.
/// It is the simplest product type and the type-theoretic equivalent of
/// logical conjunction (A AND B).
///
/// @see Core.Unit, HKT.Functor
module Core.Pair;

/// A pair of two values.
/// @param a Type of the first component
/// @param b Type of the second component
type Pair(a:Type, b:Type) = Pair(fst:a, snd:b);

/// First projection — extracts the left component.
/// @param p A pair
/// @returns The first component
/// @example fst(Pair(1, True)) => 1
function fst(p:Pair(a, b)) : a = match
    | Pair(x, y) -> x;

/// Second projection — extracts the right component.
/// @param p A pair
/// @returns The second component
/// @example snd(Pair(1, True)) => True
function snd(p:Pair(a, b)) : b = match
    | Pair(x, y) -> y;

/// Exchanges the two components.
/// @param p A pair
/// @returns The pair with components swapped
/// @example swap(Pair(1, True)) => Pair(True, 1)
function swap(p:Pair(a, b)) : Pair(b, a) = match
    | Pair(x, y) -> Pair(y, x);

/// Applies a function to the first component.
/// @param f The function to apply
/// @param p A pair
/// @returns Pair with f applied to first component
function mapFst(f:a, p:Pair(a, b)) : Pair(c, b) = match
    | f, Pair(x, y) -> Pair(f(x), y);

/// Applies a function to the second component.
/// @param f The function to apply
/// @param p A pair
/// @returns Pair with f applied to second component
function mapSnd(f:b, p:Pair(a, b)) : Pair(a, c) = match
    | f, Pair(x, y) -> Pair(x, f(y));

/// Applies the same function to both components of a homogeneous pair.
/// @param f The function to apply
/// @param p A homogeneous pair
/// @returns Pair with f applied to both components
function both(f:a, p:Pair(a, a)) : Pair(b, b) = match
    | f, Pair(x, y) -> Pair(f(x), f(y));
