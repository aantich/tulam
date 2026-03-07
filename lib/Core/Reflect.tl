/// Value and type reflection primitives.
///
/// Provides runtime introspection of algebraic data type values:
/// constructor tags, field access, and type-level constructor enumeration.
/// These primitives power the derive system for automatic instance generation.
///
/// @see Algebra.Eq, Algebra.Ord, Algebra.Show
/// @since 0.2.0
module Core.Reflect;

import Core.Bool;
import Core.Ordering;
import Algebra.Eq;
import Algebra.Ord;
import Algebra.Show;
import Algebra.Hashable;
import Algebra.StringOps;

// ---------------------------------------------------------------------------
// Value reflection intrinsics (operate on any value at runtime)
// ---------------------------------------------------------------------------

/// Constructor tag index (0-based). Primitives return 0.
/// For sum types, each constructor gets a sequential index starting from 0.
/// @param x Any value
/// @returns The constructor's tag index
/// @example tag(True) => 0
/// @example tag(False) => 1
/// @example tag(Just(42)) => 1
/// @example tag(42) => 0
function tag(x:a) : Int = intrinsic;

/// Constructor name as a string. Primitives return their type name.
/// @param x Any value
/// @returns The constructor name as a String
/// @example tagName(True) => "True"
/// @example tagName(Just(42)) => "Just"
/// @example tagName(42) => "Int"
function tagName(x:a) : String = intrinsic;

/// Number of fields in the constructor application. Primitives return 0.
/// @param x Any value
/// @returns The number of fields (constructor arity)
/// @example arity(True) => 0
/// @example arity(Just(42)) => 1
/// @example arity(Pair(1, 2)) => 2
function arity(x:a) : Int = intrinsic;

/// Get the i-th field of a constructor value (0-indexed).
/// Raises an error if i >= arity(x).
/// @param x A constructor value with fields
/// @param i The field index (0-based)
/// @returns The value of the i-th field
/// @example field(Just(42), 0) => 42
/// @example field(Pair(1, 2), 0) => 1
/// @example field(Pair(1, 2), 1) => 2
function field(x:a, i:Int) : b = intrinsic;

// ---------------------------------------------------------------------------
// Type reflection intrinsics (operate on type names as strings)
// ---------------------------------------------------------------------------

/// Number of constructors in a sum type.
/// Takes the type name as a string since types are erased at runtime.
/// @param typeName The name of the type as a String
/// @returns The number of constructors
/// @example numConstructors("Bool") => 2
/// @example numConstructors("Ordering") => 3
/// @example numConstructors("Maybe") => 2
function numConstructors(typeName:String) : Int = intrinsic;

/// Create a nullary constructor value by type name and tag index.
/// Raises an error if the constructor at that index has fields.
/// @param typeName The name of the type as a String
/// @param i The constructor tag index (0-based)
/// @returns The constructed value
/// @example constructorByIndex("Bool", 0) => True
/// @example constructorByIndex("Bool", 1) => False
/// @example constructorByIndex("Ordering", 2) => GreaterThan
function constructorByIndex(typeName:String, i:Int) : a = intrinsic;

// ---------------------------------------------------------------------------
// Structural helper functions (used by derive blocks in algebras)
// ---------------------------------------------------------------------------

/// Structural equality: compare constructor tags, then all fields recursively.
/// Fields are compared using their own (==) instances via CLMIAP dispatch.
/// @param x First value
/// @param y Second value
/// @returns True if structurally equal
/// @example structuralEq(True, True) => True
/// @example structuralEq(True, False) => False
/// @example structuralEq(Just(1), Just(1)) => True
/// @example structuralEq(Just(1), Just(2)) => False
function structuralEq(x:a, y:a) : Bool =
    if tag(x) != tag(y) then False
    else eqFields(x, y, 0, arity(x));

/// Compare fields of two values for equality, starting at index i.
/// @param x First value
/// @param y Second value
/// @param i Current field index
/// @param n Total number of fields
/// @returns True if all fields from i to n-1 are equal
function eqFields(x:a, y:a, i:Int, n:Int) : Bool =
    if i >= n then True
    else if field(x, i) == field(y, i) then eqFields(x, y, i + 1, n)
    else False;

/// Structural ordering: compare constructor tags first, then fields lexicographically.
/// @param x First value
/// @param y Second value
/// @returns Ordering of x relative to y
/// @example structuralCompare(True, False) => LessThan
/// @example structuralCompare(Just(1), Just(2)) => LessThan
function structuralCompare(x:a, y:a) : Ordering =
    if tag(x) < tag(y) then LessThan
    else if tag(x) > tag(y) then GreaterThan
    else compareFields(x, y, 0, arity(x));

/// Compare fields of two values lexicographically, starting at index i.
/// @param x First value
/// @param y Second value
/// @param i Current field index
/// @param n Total number of fields
/// @returns Ordering of fields from i to n-1
function compareFields(x:a, y:a, i:Int, n:Int) : Ordering =
    if i >= n then Equal
    else match compare(field(x, i), field(y, i))
         | Equal -> compareFields(x, y, i + 1, n)
         | result -> result;

/// Structural show: constructor name followed by fields in parentheses.
/// @param x Any value
/// @returns String representation
/// @example structuralShow(True) => "True"
/// @example structuralShow(Just(42)) => "Just(42)"
function structuralShow(x:a) : String =
    if arity(x) == 0 then tagName(x)
    else concat(tagName(x), concat("(", concat(showFields(x, 0, arity(x)), ")")));

/// Show fields of a value as comma-separated strings, starting at index i.
/// @param x A value with fields
/// @param i Current field index
/// @param n Total number of fields
/// @returns Comma-separated string of field representations
function showFields(x:a, i:Int, n:Int) : String =
    if i >= n - 1 then show(field(x, i))
    else concat(show(field(x, i)), concat(", ", showFields(x, i + 1, n)));

/// Structural hash: combine constructor tag with field hashes.
/// @param x Any value
/// @returns Hash value
function structuralHash(x:a) : Int =
    hashFields(x, tag(x) * 31, 0, arity(x));

/// Hash fields of a value, accumulating hash from index i.
/// @param x A value with fields
/// @param acc Accumulated hash value
/// @param i Current field index
/// @param n Total number of fields
/// @returns Combined hash of all fields
function hashFields(x:a, acc:Int, i:Int, n:Int) : Int =
    if i >= n then acc
    else hashFields(x, acc * 31 + hash(field(x, i)), i + 1, n);
