/// Peano natural numbers (unary representation).
///
/// Nat represents natural numbers (0, 1, 2, ...) using the Peano encoding:
/// Z for zero, Succ(n) for the successor of n. This gives a type-safe
/// representation where negative numbers are impossible by construction.
///
/// @note For efficient arithmetic, use Int with the Nat repr mapping.
/// @see Repr.Nat, Core.Instances
module Core.Nat;

/// Natural numbers in Peano representation.
/// @example Z
/// @example Succ(Z)
/// @example Succ(Succ(Z))
type Nat = Z | Succ(n:Nat);

/// Structural equality on natural numbers.
/// @param x First natural number
/// @param y Second natural number
/// @returns True if x and y are structurally equal
/// @example eq(Z, Z) => True
/// @example eq(Succ(Z), Z) => False
function eq(x:Nat,y:Nat):Bool = match
  | Z, Z -> True
  | Z, n -> False
  | n, Z -> False
  | Succ(m), Succ(n) -> eq(m,n);

/// Addition of natural numbers.
/// @param x First operand
/// @param y Second operand
/// @returns The sum x + y
/// @example plus(Succ(Z), Succ(Z)) => Succ(Succ(Z))
function plus (x:Nat,y:Nat):Nat = match
  | Z, n -> n
  | Succ(n), m -> Succ (plus(n, m));

/// Multiplication of natural numbers.
/// @param x First operand
/// @param y Second operand
/// @returns The product x * y
/// @example mult(Succ(Succ(Z)), Succ(Succ(Z))) => Succ(Succ(Succ(Succ(Z))))
function mult (x:Nat,y:Nat):Nat = match
  | Z, n -> Z
  | Succ(n), m -> plus(mult(n, m), m);
