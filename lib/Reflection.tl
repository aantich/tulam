/// Reflection intrinsics and structural helpers.
///
/// Provides runtime access to constructor structure: tag indices, names,
/// field access, and constructor enumeration. Also provides structural
/// implementations of Eq, Ord, Show, and Hashable used by derive blocks.
module Reflection;
import Core;
import Algebra;
import Repr;

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
    else eqFields(x, y, fromRepr(arity(x)));

/// Compare fields counting down from remaining to Z.
function eqFields(x:a, y:a, remaining:Nat) : Bool = match
    | x, y, Z -> True
    | x, y, Succ(r) ->
        let i = arity(x) - toRepr(remaining) in
        if field(x, i) == field(y, i) then eqFields(x, y, r)
        else False;

/// Structural ordering: compare constructor tags first, then fields lexicographically.
function structuralCompare(x:a, y:a) : Ordering =
    if tag(x) < tag(y) then LessThan
    else if tag(x) > tag(y) then GreaterThan
    else compareFields(x, y, fromRepr(arity(x)));

/// Compare fields lexicographically, counting down.
function compareFields(x:a, y:a, remaining:Nat) : Ordering = match
    | x, y, Z -> Equal
    | x, y, Succ(r) ->
        let i = arity(x) - toRepr(remaining) in
        match compare(field(x, i), field(y, i))
        | Equal -> compareFields(x, y, r)
        | result -> result;

/// Structural show: constructor name followed by fields in parentheses.
function structuralShow(x:a) : String =
    if arity(x) == 0 then tagName(x)
    else tagName(x) <> "(" <> showFields(x, fromRepr(arity(x))) <> ")";

/// Show fields comma-separated, counting down.
function showFields(x:a, remaining:Nat) : String = match
    | x, Succ(Z) ->
        let i = arity(x) - 1 in
        show(field(x, i))
    | x, Succ(r) ->
        let i = arity(x) - toRepr(remaining) in
        show(field(x, i)) <> ", " <> showFields(x, r);

/// Structural hash: combine constructor tag with field hashes.
function structuralHash(x:a) : Int =
    hashFields(x, tag(x) * 31, fromRepr(arity(x)));

/// Hash fields, counting down.
function hashFields(x:a, acc:Int, remaining:Nat) : Int = match
    | x, acc, Z -> acc
    | x, acc, Succ(r) ->
        let i = arity(x) - toRepr(remaining) in
        hashFields(x, acc * 31 + hash(field(x, i)), r);
