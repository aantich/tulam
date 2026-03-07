/// Standard combinators from combinatory logic.
///
/// These are the fundamental higher-order functions that form the
/// basis of point-free programming.
///
/// @see HKT.Functor, HKT.Monad
module Core.Combinators;

/// Identity function — returns its argument unchanged.
/// @param x Any value
/// @returns x
/// @example id(42) => 42
function id(x:a) : a = x;

/// Constant function — ignores its second argument.
/// @param x The value to return
/// @param y Ignored
/// @returns x
/// @example const(1, 2) => 1
function const(x:a, y:b) : a = x;

/// Function application.
/// @param f The function to apply
/// @param x The argument
/// @returns f(x)
function apply(f:a, x:b) : c = f(x);

/// Function composition — (f . g)(x) = f(g(x)).
/// @param f The outer function
/// @param g The inner function
/// @param x The argument
/// @returns f(g(x))
function compose(f:b, g:a, x:a) : c = f(g(x));

/// Flips the first two arguments of a function.
/// @param f The function to flip
/// @param x First argument (becomes second)
/// @param y Second argument (becomes first)
/// @returns f(y, x)
function flip(f:a, x:b, y:c) = f(y, x);
