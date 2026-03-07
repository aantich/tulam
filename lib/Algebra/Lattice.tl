/// Lattice hierarchy: join (least upper bound) and meet (greatest lower bound).
///
/// A lattice is a partially ordered set where every pair of elements has
/// a join (\/) and a meet (/\). Used for Bool, sets, types, and information ordering.
///
/// @see Algebra.Ord, Algebra.Bounded
module Algebra.Lattice;

/// Join (least upper bound) semi-lattice.
algebra JoinSemiLattice(a:Type) = {
    /// Join — least upper bound of two values.
    /// @param x Left operand
    /// @param y Right operand
    /// @returns The smallest value >= both x and y
    function (\/)(x:a, y:a) : a,

    law join_associativity(x:a, y:a, z:a) = (x \/ y) \/ z === x \/ (y \/ z),
    law join_commutativity(x:a, y:a) = (x \/ y) === (y \/ x),
    law join_idempotence(x:a) = (x \/ x) === x
};

/// Meet (greatest lower bound) semi-lattice.
algebra MeetSemiLattice(a:Type) = {
    /// Meet — greatest lower bound of two values.
    /// @param x Left operand
    /// @param y Right operand
    /// @returns The largest value <= both x and y
    function (/\)(x:a, y:a) : a,

    law meet_associativity(x:a, y:a, z:a) = (x /\ y) /\ z === x /\ (y /\ z),
    law meet_commutativity(x:a, y:a) = (x /\ y) === (y /\ x),
    law meet_idempotence(x:a) = (x /\ x) === x
};

/// Full lattice with both join and meet, connected by absorption.
algebra Lattice(a:Type) extends JoinSemiLattice(a), MeetSemiLattice(a) = {
    law join_absorption(x:a, y:a) = x \/ (x /\ y) === x,
    law meet_absorption(x:a, y:a) = x /\ (x \/ y) === x
};

/// Lattice with top and bottom elements.
algebra BoundedLattice(a:Type) extends Lattice(a) = {
    /// The smallest element (identity for join).
    value bottom : a,

    /// The largest element (identity for meet).
    value top : a,

    law bottom_identity(x:a) = (bottom \/ x) === x,
    law top_identity(x:a) = (top /\ x) === x
};
