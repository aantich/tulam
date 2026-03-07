/// Embed — injective embedding with partial inverse.
///
/// Embeds type a into type b losslessly. Going back from b to a may
/// fail (returns Maybe), since b may have values with no a equivalent.
///
/// @see Morphism.Convertible, Morphism.Iso
module Morphism.Embed;
import Core.Maybe;

/// Injective embedding with partial inverse.
morphism Embed(a:Type, b:Type) = {
    /// Inject a value of type a into type b (lossless).
    /// @param x The value to embed
    /// @returns The embedded value
    function embed(x:a) : b,

    /// Attempt to extract a value of type a from type b.
    /// @param y The value to project
    /// @returns Just(x) if y is in the image of embed, Nothing otherwise
    function project(y:b) : Maybe(a),

    law embed_project(x:a) = project(embed(x)) === Just(x)
};
