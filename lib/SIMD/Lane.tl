/// SIMD lane operations: splat, extract, insert, and reductions.
///
/// @see SIMD.Vec
module SIMD.Lane;

/// Lane-wise SIMD operations.
algebra Lane(v:Type1) = {
    /// Broadcast a scalar to all lanes.
    /// @param x The scalar value
    /// @returns Vector with x in every lane
    function splat(x:a) : v(a),

    /// Extract the value at a lane index.
    /// @param xs The vector
    /// @param i Lane index
    /// @returns The value at lane i
    function extract(xs:v(a), i:Int) : a,

    /// Insert a value at a lane index.
    /// @param xs The vector
    /// @param i Lane index
    /// @param x Value to insert
    /// @returns Updated vector
    function insert(xs:v(a), i:Int, x:a) : v(a),

    /// Horizontal sum — add all lanes.
    /// @param xs The vector
    /// @returns Sum of all lane values
    function hsum(xs:v(a)) : a,

    /// Horizontal minimum — smallest lane value.
    /// @param xs The vector
    /// @returns Minimum of all lane values
    function hmin(xs:v(a)) : a,

    /// Horizontal maximum — largest lane value.
    /// @param xs The vector
    /// @returns Maximum of all lane values
    function hmax(xs:v(a)) : a,

    /// Number of lanes in the vector type.
    /// @param xs The vector
    /// @returns Number of lanes
    function lanes(xs:v(a)) : Int
};

/// Lane operations for Vec2.
instance Lane(Vec2) = intrinsic;
/// Lane operations for Vec4.
instance Lane(Vec4) = intrinsic;
/// Lane operations for Vec8.
instance Lane(Vec8) = intrinsic;
/// Lane operations for Vec16.
instance Lane(Vec16) = intrinsic;
