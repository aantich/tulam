/// SIMD vector types and lane operations.
module SIMD;

// ============================================================================
// Vector primitives
// ============================================================================

primitive Vec2(a:Type);
primitive Vec4(a:Type);
primitive Vec8(a:Type);
primitive Vec16(a:Type);

// ============================================================================
// Lane algebra
// ============================================================================

algebra Lane(v:Type1) = {
    function splat(x:a) : v(a);
    function extract(xs:v(a), i:Int) : a;
    function insert(xs:v(a), i:Int, x:a) : v(a);
    function hsum(xs:v(a)) : a;
    function hmin(xs:v(a)) : a;
    function hmax(xs:v(a)) : a;
    function lanes(xs:v(a)) : Int
};

instance Lane(Vec2) = intrinsic;
instance Lane(Vec4) = intrinsic;
instance Lane(Vec8) = intrinsic;
instance Lane(Vec16) = intrinsic;
