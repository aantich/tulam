/// The empty type with no values.
///
/// Void has no constructors, meaning no value of type Void can ever exist.
/// It is the "impossible" type, the logical equivalent of False/absurdity.
///
/// Use cases:
///   - Marking impossible branches: Either(Void, a) is isomorphic to a
///   - Type-level proofs: a function Void -> a proves "from nothing, anything"
///   - The initial object in the category of types
module Core.Void;

/// The empty type — no constructors, no inhabitants.
type Void;
