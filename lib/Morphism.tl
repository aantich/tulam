/// Morphism definitions: Convertible, Embed, Iso.
///
/// Type conversion patterns from one-way lossy to fully reversible.
module Morphism;
import Core;

// ============================================================================
// Convertible
// ============================================================================

/// One-way type conversion morphism.
morphism Convertible(a:Type, b:Type) = {
    function convert(x:a) : b
};

// ============================================================================
// Embed
// ============================================================================

/// Injective embedding with partial inverse.
morphism Embed(a:Type, b:Type) = {
    function embed(x:a) : b;
    function project(y:b) : Maybe(a);

    law embed_project(x:a) = project(embed(x)) === Just(x)
};

// ============================================================================
// Iso
// ============================================================================

/// Lossless bidirectional type conversion.
morphism Iso(a:Type, b:Type) extends Convertible(a, b) = {
    function unconvert(x:b) : a;

    law roundtrip1(x:a) = unconvert(convert(x)) === x;
    law roundtrip2(y:b) = convert(unconvert(y)) === y
};
