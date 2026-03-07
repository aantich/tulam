/// Binary tree with values at the leaves.
///
/// Tree(a) is a binary tree where each node is either a Leaf containing
/// a value, or a Branch splitting into left and right subtrees.
///
/// @see HKT.Functor
module Core.Tree;
import Core.Nat;

/// Binary tree — Leaf(value) or Branch(left, right).
/// @param a The type of values stored at leaves
/// @example Leaf(1)
/// @example Branch(Leaf(1), Leaf(2))
type Tree(a:Type) = Leaf(val:a) | Branch(left:Tree(a), right:Tree(a));

/// Counts the number of leaves in the tree.
/// @param t The tree
/// @returns Number of leaves as a Nat
/// @example treeSize(Leaf(1)) => Succ(Z)
function treeSize(t:Tree(a)) : Nat = match
    | Leaf(x) -> Succ(Z)
    | Branch(l, r) -> plus(treeSize(l), treeSize(r));

/// Computes the depth (height) of the tree.
/// @param t The tree
/// @returns Depth as a Nat (Leaf has depth 0)
/// @example treeDepth(Leaf(1)) => Z
function treeDepth(t:Tree(a)) : Nat = match
    | Leaf(x) -> Z
    | Branch(l, r) -> Succ(natMax(treeDepth(l), treeDepth(r)));

/// Returns the larger of two natural numbers.
/// @param x First natural number
/// @param y Second natural number
/// @returns The maximum of x and y
function natMax(x:Nat, y:Nat) : Nat = match
    | Z, n -> n
    | n, Z -> n
    | Succ(m), Succ(n) -> Succ(natMax(m, n));

/// Applies a function to every leaf value, preserving structure.
/// @param f The function to apply
/// @param t The tree
/// @returns Tree with f applied to all leaf values
function treeMap(f:a, t:Tree(a)) : Tree(b) = match
    | f, Leaf(x) -> Leaf(f(x))
    | f, Branch(l, r) -> Branch(treeMap(f, l), treeMap(f, r));

/// Catamorphism — fold a tree by replacing Leaf and Branch with functions.
/// @param f Function to apply to leaf values
/// @param g Function to combine branch results
/// @param t The tree to fold
/// @returns The folded result
function treeFold(f:a, g:b, t:Tree(a)) : b = match
    | f, g, Leaf(x) -> f(x)
    | f, g, Branch(l, r) -> g(treeFold(f, g, l), treeFold(f, g, r));
