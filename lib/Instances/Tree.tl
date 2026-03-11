/// Tree instances: Show.
///
/// Also provides tree operations: treeSize, treeDepth, treeMap, treeFold.
module Instances.Tree;
import Core;
import Algebra;
import Instances.Nat;

// ============================================================================
// Tree-specific functions
// ============================================================================

/// Counts the number of leaves in the tree.
function treeSize(t:Tree(a)) : Nat = match
    | Leaf(x) -> Succ(Z)
    | Branch(l, r) -> plus(treeSize(l), treeSize(r));

/// Computes the depth (height) of the tree.
function treeDepth(t:Tree(a)) : Nat = match
    | Leaf(x) -> Z
    | Branch(l, r) -> Succ(natMax(treeDepth(l), treeDepth(r)));

/// Applies a function to every leaf value, preserving structure.
function treeMap(f:(a -> b), t:Tree(a)) : Tree(b) = match
    | f, Leaf(x) -> Leaf(f(x))
    | f, Branch(l, r) -> Branch(treeMap(f, l), treeMap(f, r));

/// Catamorphism — fold a tree by replacing Leaf and Branch with functions.
function treeFold(f:(a -> b), g:(b -> b -> b), t:Tree(a)) : b = match
    | f, g, Leaf(x) -> f(x)
    | f, g, Branch(l, r) -> g(treeFold(f, g, l), treeFold(f, g, r));

// ============================================================================
// Algebra instances
// ============================================================================

instance Show(Tree(a)) = {
    function show(x:Tree(a)) : String = match
        | Leaf(v) -> concat("Leaf(", concat(show(v), ")"))
        | Branch(l, r) -> concat("Branch(", concat(show(l), concat(", ", concat(show(r), ")"))))
};
