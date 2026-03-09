/// Reflection intrinsics and structural helpers.
///
/// Provides runtime access to constructor structure: tag indices, names,
/// field access, and constructor enumeration. Also provides structural
/// implementations of Eq, Ord, Show, and Hashable used by derive blocks.
module Reflection;
import Core;

// ============================================================================
// Reflection intrinsics
// ============================================================================

/// Constructor tag index (0-based). Primitives return 0.
function tag(x:a) : Int = intrinsic;

/// Constructor name as a string. Primitives return their type name.
function tagName(x:a) : String = intrinsic;

/// Number of fields in the constructor application. Primitives return 0.
function arity(x:a) : Int = intrinsic;

/// Get the i-th field of a constructor value (0-indexed).
function field(x:a, i:Int) : b = intrinsic;

/// Number of constructors in a sum type.
function numConstructors(typeName:String) : Int = intrinsic;

/// Create a nullary constructor value by type name and tag index.
function constructorByIndex(typeName:String, i:Int) : a = intrinsic;

// ============================================================================
// Structural helpers (used by derive blocks)
// ============================================================================

/// Structural equality: compare constructor tags, then all fields recursively.
function structuralEq(x:a, y:a) : Bool =
    if tag(x) != tag(y) then False
    else eqFields(x, y, 0, arity(x));

function eqFields(x:a, y:a, i:Int, n:Int) : Bool =
    if i >= n then True
    else if field(x, i) == field(y, i) then eqFields(x, y, i + 1, n)
    else False;

/// Structural ordering: compare constructor tags first, then fields lexicographically.
function structuralCompare(x:a, y:a) : Ordering =
    if tag(x) < tag(y) then LessThan
    else if tag(x) > tag(y) then GreaterThan
    else compareFields(x, y, 0, arity(x));

function compareFields(x:a, y:a, i:Int, n:Int) : Ordering =
    if i >= n then Equal
    else match compare(field(x, i), field(y, i))
         | Equal -> compareFields(x, y, i + 1, n)
         | result -> result;

/// Structural show: constructor name followed by fields in parentheses.
function structuralShow(x:a) : String =
    if arity(x) == 0 then tagName(x)
    else concat(tagName(x), concat("(", concat(showFields(x, 0, arity(x)), ")")));

function showFields(x:a, i:Int, n:Int) : String =
    if i >= n - 1 then show(field(x, i))
    else concat(show(field(x, i)), concat(", ", showFields(x, i + 1, n)));

/// Structural hash: combine constructor tag with field hashes.
function structuralHash(x:a) : Int =
    hashFields(x, tag(x) * 31, 0, arity(x));

function hashFields(x:a, acc:Int, i:Int, n:Int) : Int =
    if i >= n then acc
    else hashFields(x, acc * 31 + hash(field(x, i)), i + 1, n);
