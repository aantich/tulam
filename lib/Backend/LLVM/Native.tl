/// Native backend target declarations for LLVM code generation.
///
/// This file is the canonical source of truth for how tulam algebras and effects
/// map to native machine operations. The compiler reads these declarations and
/// uses inline expansion at call sites — zero overhead, no wrapper functions.
///
/// Architecture (three layers):
///   Layer 1: Primitive ops (__add_i64 etc.) — compiler-internal, map to LIR instructions
///   Layer 2: This file — composes primitive ops into algebra instances and effect handlers
///   Layer 3: User code — pure tulam, completely backend-agnostic
///
/// Primitive op naming convention:
///   __<op>_<type>    e.g. __add_i64, __mul_f64, __print_string
///   These are not callable by user code. They exist only inside target blocks.
///
/// Instance inheritance: each instance only defines methods NEW to that algebra level.
/// Upward propagation automatically registers inherited methods with parent algebras.
/// E.g. EuclideanDomain(Int) with div/mod propagates (+) from AdditiveSemigroup(Int).
module Backend.LLVM.Native;

// ============================================================================
// Int — 64-bit signed integer
// ============================================================================

target native {

    instance AdditiveSemigroup(Int) = {
        function (+)(x, y) = __add_i64(x, y)
    };

    instance AdditiveMonoid(Int) = {
        value zero = 0
    };

    instance AdditiveGroup(Int) = {
        function negate(x) = __neg_i64(x);
        function (-)(x, y) = __sub_i64(x, y)
    };

    instance MultiplicativeSemigroup(Int) = {
        function (*)(x, y) = __mul_i64(x, y)
    };

    instance MultiplicativeMonoid(Int) = {
        value one = 1
    };

    instance EuclideanDomain(Int) = {
        function div(x, y) = __div_i64(x, y);
        function mod(x, y) = __rem_i64(x, y)
    };

    instance Absolute(Int) = {
        function abs(x) = if __lt_i64(x, 0) then __neg_i64(x) else x;
        function signum(x) = if __lt_i64(x, 0) then __neg_i64(1)
                              else if __eq_i64(x, 0) then 0 else 1
    };

    instance Bits(Int) = {
        function (.&.)(x, y) = __band_i64(x, y);
        function (.|.)(x, y) = __bor_i64(x, y);
        function xor(x, y) = __bxor_i64(x, y);
        function complement(x) = __bxor_i64(x, __neg_i64(1));
        function shiftL(x, y) = __shl_i64(x, y);
        function shiftR(x, y) = __shr_i64(x, y);
        value bitSize = 64
    };

    instance Eq(Int) = {
        function (==)(x, y) = __eq_i64(x, y);
        function (!=)(x, y) = __neq_i64(x, y)
    };

    instance Ord(Int) = {
        function (<)(x, y) = __lt_i64(x, y);
        function (>)(x, y) = __gt_i64(x, y);
        function (<=)(x, y) = __le_i64(x, y);
        function (>=)(x, y) = __ge_i64(x, y);
        function compare(x, y) =
            if __lt_i64(x, y) then LessThan
            else if __gt_i64(x, y) then GreaterThan
            else EqualTo
    };

    instance Show(Int) = {
        function show(x) = __show_i64(x)
    };

    instance Bounded(Int) = {
        value minBound = __int_min_i64();
        value maxBound = __int_max_i64()
    };

    instance Enum(Int) = {
        function succ(x) = __add_i64(x, 1);
        function pred(x) = __sub_i64(x, 1);
        function toEnum(i) = i;
        function fromEnum(x) = x
    };

    instance Hashable(Int) = {
        function hash(x) = x
    };

    instance Group(Int) = {
        function combine(x, y) = __add_i64(x, y);
        value empty = 0;
        function inverse(x) = __neg_i64(x)
    };

};

// ============================================================================
// Float64 — 64-bit IEEE 754 double
// ============================================================================

target native {

    instance AdditiveSemigroup(Float64) = {
        function (+)(x, y) = __add_f64(x, y)
    };

    instance AdditiveMonoid(Float64) = {
        value zero = 0.0
    };

    instance AdditiveGroup(Float64) = {
        function negate(x) = __neg_f64(x);
        function (-)(x, y) = __sub_f64(x, y)
    };

    instance MultiplicativeSemigroup(Float64) = {
        function (*)(x, y) = __mul_f64(x, y)
    };

    instance MultiplicativeMonoid(Float64) = {
        value one = 1.0
    };

    instance MultiplicativeGroup(Float64) = {
        function recip(x) = __div_f64(1.0, x);
        function (/)(x, y) = __div_f64(x, y)
    };

    /// Floating adds transcendental ops on top of Field.
    instance Floating(Float64) = {
        function sqrt(x) = __sqrt_f64(x);
        function exp(x) = __exp_f64(x);
        function log(x) = __log_f64(x);
        function sin(x) = __sin_f64(x);
        function cos(x) = __cos_f64(x);
        function tan(x) = __tan_f64(x);
        function asin(x) = __asin_f64(x);
        function acos(x) = __acos_f64(x);
        function atan(x) = __atan_f64(x);
        function pow(x, y) = __pow_f64(x, y);
        value pi = 3.141592653589793
    };

    instance Absolute(Float64) = {
        function abs(x) = __fabs_f64(x);
        function signum(x) = if __flt_f64(x, 0.0) then __neg_f64(1.0)
                              else if __feq_f64(x, 0.0) then 0.0 else 1.0
    };

    instance Eq(Float64) = {
        function (==)(x, y) = __feq_f64(x, y);
        function (!=)(x, y) = __fneq_f64(x, y)
    };

    instance Ord(Float64) = {
        function (<)(x, y) = __flt_f64(x, y);
        function (>)(x, y) = __fgt_f64(x, y);
        function (<=)(x, y) = __fle_f64(x, y);
        function (>=)(x, y) = __fge_f64(x, y);
        function compare(x, y) =
            if __flt_f64(x, y) then LessThan
            else if __fgt_f64(x, y) then GreaterThan
            else EqualTo
    };

    instance Show(Float64) = {
        function show(x) = __show_f64(x)
    };

    instance AbelianGroup(Float64) = {
        function combine(x, y) = __add_f64(x, y);
        value empty = 0.0;
        function inverse(x) = __neg_f64(x)
    };

};

// ============================================================================
// Float32 — 32-bit IEEE 754 float
// ============================================================================

target native {

    instance AdditiveSemigroup(Float32) = {
        function (+)(x, y) = __add_f32(x, y)
    };

    instance AdditiveMonoid(Float32) = {
        value zero : Float32 = 0.0
    };

    instance AdditiveGroup(Float32) = {
        function negate(x) = __neg_f32(x);
        function (-)(x, y) = __sub_f32(x, y)
    };

    instance MultiplicativeSemigroup(Float32) = {
        function (*)(x, y) = __mul_f32(x, y)
    };

    instance MultiplicativeMonoid(Float32) = {
        value one : Float32 = 1.0
    };

    instance MultiplicativeGroup(Float32) = {
        function recip(x) = __div_f32(1.0, x);
        function (/)(x, y) = __div_f32(x, y)
    };

    instance Floating(Float32) = {
        function sqrt(x) = __sqrt_f32(x);
        function exp(x) = __exp_f32(x);
        function log(x) = __log_f32(x);
        function sin(x) = __sin_f32(x);
        function cos(x) = __cos_f32(x);
        function tan(x) = __tan_f32(x);
        function asin(x) = __asin_f32(x);
        function acos(x) = __acos_f32(x);
        function atan(x) = __atan_f32(x);
        function pow(x, y) = __pow_f32(x, y);
        value pi : Float32 = 3.14159265
    };

    instance Absolute(Float32) = {
        function abs(x) = __fabs_f32(x);
        function signum(x) = if __flt_f32(x, 0.0) then __neg_f32(1.0)
                              else if __feq_f32(x, 0.0) then 0.0 else 1.0
    };

    instance Eq(Float32) = {
        function (==)(x, y) = __feq_f32(x, y);
        function (!=)(x, y) = __fneq_f32(x, y)
    };

    instance Ord(Float32) = {
        function (<)(x, y) = __flt_f32(x, y);
        function (>)(x, y) = __fgt_f32(x, y);
        function (<=)(x, y) = __fle_f32(x, y);
        function (>=)(x, y) = __fge_f32(x, y);
        function compare(x, y) =
            if __flt_f32(x, y) then LessThan
            else if __fgt_f32(x, y) then GreaterThan
            else EqualTo
    };

    instance Show(Float32) = {
        function show(x) = __show_f32(x)
    };

};

// ============================================================================
// String
// ============================================================================

target native {

    instance StringOps(String) = {
        function concat(x, y) = __string_concat(x, y);
        function (++)(x, y) = __string_concat(x, y);
        function length(s) = __string_length(s)
    };

    instance Eq(String) = {
        function (==)(x, y) = __string_eq(x, y);
        function (!=)(x, y) = __string_neq(x, y)
    };

    instance Ord(String) = {
        function compare(x, y) = __string_compare(x, y);
        function (<)(x, y) = match __string_compare(x, y) | LessThan -> True | _ -> False;
        function (>)(x, y) = match __string_compare(x, y) | GreaterThan -> True | _ -> False;
        function (<=)(x, y) = match __string_compare(x, y) | GreaterThan -> False | _ -> True;
        function (>=)(x, y) = match __string_compare(x, y) | LessThan -> False | _ -> True
    };

    instance Show(String) = {
        function show(s) = __string_show(s)
    };

    instance Semigroup(String) = {
        function combine(x, y) = __string_concat(x, y)
    };

    instance Monoid(String) = {
        value empty = ""
    };

    instance Hashable(String) = {
        function hash(s) = __string_hash(s)
    };

    instance StringExt(String) = {
        function charAt(s, i) = __string_charAt(s, i);
        function substring(s, start, end) = __string_substring(s, start, end);
        function indexOf(haystack, needle) = __string_indexOf(haystack, needle);
        function trim(s) = __string_trim(s);
        function toUpper(s) = __string_toUpper(s);
        function toLower(s) = __string_toLower(s);
        function startsWith(s, prefix) = __string_startsWith(s, prefix);
        function endsWith(s, suffix) = __string_endsWith(s, suffix);
        function replace(s, old, new) = __string_replace(s, old, new);
        function parseInt(s) = __string_parseInt(s);
        function parseFloat(s) = __string_parseFloat(s);
        function split(s, delim) = __string_split(s, delim);
        function join(xs, delim) = __string_join(xs, delim);
        function fromChar(c) = __string_fromChar(c)
    };

};

// ============================================================================
// Char
// ============================================================================

target native {

    instance Eq(Char) = {
        function (==)(x, y) = __eq_i32(x, y);
        function (!=)(x, y) = __neq_i32(x, y)
    };

    instance Ord(Char) = {
        function (<)(x, y) = __lt_i32(x, y);
        function (>)(x, y) = __gt_i32(x, y);
        function (<=)(x, y) = __le_i32(x, y);
        function (>=)(x, y) = __ge_i32(x, y);
        function compare(x, y) =
            if __lt_i32(x, y) then LessThan
            else if __gt_i32(x, y) then GreaterThan
            else EqualTo
    };

    instance Show(Char) = {
        function show(c) = __show_char(c)
    };

    instance Enum(Char) = {
        function succ(c) = __char_succ(c);
        function pred(c) = __char_pred(c);
        function toEnum(i) = __int_to_char(i);
        function fromEnum(c) = __char_to_int(c)
    };

    instance Bounded(Char) = {
        value minBound = '\0';
        value maxBound = __char_max()
    };

    instance Hashable(Char) = {
        function hash(c) = __char_to_int(c)
    };

    // Bool instances — no externs needed, pure pattern matching
    instance Show(Bool) = {
        function show(x) = match x | True -> "True" | False -> "False"
    };

    instance Eq(Bool) = {
        function (==)(x, y) = match x
            | True -> match y | True -> True | False -> False
            | False -> match y | True -> False | False -> True;
        function (!=)(x, y) = match x
            | True -> match y | True -> False | False -> True
            | False -> match y | True -> True | False -> False
    };

};

// ============================================================================
// Sequence effect handler — strict sequential execution
// ============================================================================

target native {

    /// In strict native code, seq/bind just apply the continuation to the value.
    /// No thunk forcing needed — Lazy(a) is handled via explicit force().
    handler NativeSequence : Sequence = {
        function effectSeq(a, f) = f(a);
        function effectBind(a, f) = f(a)
    };

};

// ============================================================================
// Console effect handler
// ============================================================================

target native {

    handler StdConsole : Console = {
        function putStrLn(s) = action {
            __print_string(s);
            __print_newline()
        };
        function putStr(s) = __print_string(s);
        function readLine() = __read_line()
    };

};

// ============================================================================
// FileIO effect handler
// ============================================================================

target native {

    handler StdFileIO : FileIO = {
        function readFile(path) = __file_read(path);
        function writeFile(path, content) = __file_write(path, content);
        function appendFile(path, content) = __file_append(path, content);
        function fileExists(path) = __file_exists(path)
    };

};

// ============================================================================
// Exception effect handler
// ============================================================================

target native {

    handler DefaultException : Exception = {
        function throw(msg) = __error(msg);
        function catch(body, h) = __try_catch(body, h)
    };

};

// ============================================================================
// Primitive op declarations — Int (i64)
// ============================================================================

target native {
    /// Arithmetic
    extern function __add_i64(x:Int, y:Int) : Int = inline "add";
    extern function __sub_i64(x:Int, y:Int) : Int = inline "sub";
    extern function __mul_i64(x:Int, y:Int) : Int = inline "mul";
    extern function __div_i64(x:Int, y:Int) : Int = inline "sdiv";
    extern function __rem_i64(x:Int, y:Int) : Int = inline "srem";
    extern function __neg_i64(x:Int) : Int = inline "neg";

    /// Comparison
    extern function __eq_i64(x:Int, y:Int) : Bool = inline "icmp eq";
    extern function __neq_i64(x:Int, y:Int) : Bool = inline "icmp ne";
    extern function __lt_i64(x:Int, y:Int) : Bool = inline "icmp slt";
    extern function __le_i64(x:Int, y:Int) : Bool = inline "icmp sle";
    extern function __gt_i64(x:Int, y:Int) : Bool = inline "icmp sgt";
    extern function __ge_i64(x:Int, y:Int) : Bool = inline "icmp sge";

    /// Bitwise
    extern function __band_i64(x:Int, y:Int) : Int = inline "and";
    extern function __bor_i64(x:Int, y:Int) : Int = inline "or";
    extern function __bxor_i64(x:Int, y:Int) : Int = inline "xor";
    extern function __shl_i64(x:Int, y:Int) : Int = inline "shl";
    extern function __shr_i64(x:Int, y:Int) : Int = inline "ashr";

    /// Conversion / display
    extern function __show_i64(x:Int) : String;
    extern function __int_min_i64() : Int;
    extern function __int_max_i64() : Int;
};

// ============================================================================
// Primitive op declarations — Float64 (f64)
// ============================================================================

target native {
    /// Arithmetic
    extern function __add_f64(x:Float64, y:Float64) : Float64 = inline "fadd";
    extern function __sub_f64(x:Float64, y:Float64) : Float64 = inline "fsub";
    extern function __mul_f64(x:Float64, y:Float64) : Float64 = inline "fmul";
    extern function __div_f64(x:Float64, y:Float64) : Float64 = inline "fdiv";
    extern function __neg_f64(x:Float64) : Float64 = inline "fneg";

    /// Comparison
    extern function __feq_f64(x:Float64, y:Float64) : Bool = inline "fcmp oeq";
    extern function __fneq_f64(x:Float64, y:Float64) : Bool = inline "fcmp one";
    extern function __flt_f64(x:Float64, y:Float64) : Bool = inline "fcmp olt";
    extern function __fle_f64(x:Float64, y:Float64) : Bool = inline "fcmp ole";
    extern function __fgt_f64(x:Float64, y:Float64) : Bool = inline "fcmp ogt";
    extern function __fge_f64(x:Float64, y:Float64) : Bool = inline "fcmp oge";

    /// Transcendentals (LLVM intrinsics)
    extern function __sqrt_f64(x:Float64) : Float64 = llvm "llvm.sqrt.f64";
    extern function __exp_f64(x:Float64) : Float64 = llvm "llvm.exp.f64";
    extern function __log_f64(x:Float64) : Float64 = llvm "llvm.log.f64";
    extern function __sin_f64(x:Float64) : Float64 = llvm "llvm.sin.f64";
    extern function __cos_f64(x:Float64) : Float64 = llvm "llvm.cos.f64";
    extern function __pow_f64(x:Float64, y:Float64) : Float64 = llvm "llvm.pow.f64";
    extern function __fabs_f64(x:Float64) : Float64 = llvm "llvm.fabs.f64";

    /// C math library (no LLVM intrinsic for these)
    extern function __tan_f64(x:Float64) : Float64;
    extern function __asin_f64(x:Float64) : Float64;
    extern function __acos_f64(x:Float64) : Float64;
    extern function __atan_f64(x:Float64) : Float64;

    /// Display
    extern function __show_f64(x:Float64) : String;
};

// ============================================================================
// Primitive op declarations — Float32 (f32)
// ============================================================================

target native {
    /// Arithmetic
    extern function __add_f32(x:Float32, y:Float32) : Float32 = inline "fadd";
    extern function __sub_f32(x:Float32, y:Float32) : Float32 = inline "fsub";
    extern function __mul_f32(x:Float32, y:Float32) : Float32 = inline "fmul";
    extern function __div_f32(x:Float32, y:Float32) : Float32 = inline "fdiv";
    extern function __neg_f32(x:Float32) : Float32 = inline "fneg";

    /// Comparison
    extern function __feq_f32(x:Float32, y:Float32) : Bool = inline "fcmp oeq";
    extern function __fneq_f32(x:Float32, y:Float32) : Bool = inline "fcmp one";
    extern function __flt_f32(x:Float32, y:Float32) : Bool = inline "fcmp olt";
    extern function __fle_f32(x:Float32, y:Float32) : Bool = inline "fcmp ole";
    extern function __fgt_f32(x:Float32, y:Float32) : Bool = inline "fcmp ogt";
    extern function __fge_f32(x:Float32, y:Float32) : Bool = inline "fcmp oge";

    /// Transcendentals (LLVM intrinsics)
    extern function __sqrt_f32(x:Float32) : Float32 = llvm "llvm.sqrt.f32";
    extern function __exp_f32(x:Float32) : Float32 = llvm "llvm.exp.f32";
    extern function __log_f32(x:Float32) : Float32 = llvm "llvm.log.f32";
    extern function __sin_f32(x:Float32) : Float32 = llvm "llvm.sin.f32";
    extern function __cos_f32(x:Float32) : Float32 = llvm "llvm.cos.f32";
    extern function __pow_f32(x:Float32, y:Float32) : Float32 = llvm "llvm.pow.f32";
    extern function __fabs_f32(x:Float32) : Float32 = llvm "llvm.fabs.f32";

    /// C math library
    extern function __tan_f32(x:Float32) : Float32;
    extern function __asin_f32(x:Float32) : Float32;
    extern function __acos_f32(x:Float32) : Float32;
    extern function __atan_f32(x:Float32) : Float32;

    /// Display
    extern function __show_f32(x:Float32) : String;
};

// ============================================================================
// Primitive op declarations — String
// ============================================================================

target native {
    /// Core string ops (all are C runtime calls)
    extern function __string_concat(x:String, y:String) : String;
    extern function __string_length(s:String) : Int;
    extern function __string_eq(x:String, y:String) : Bool;
    extern function __string_neq(x:String, y:String) : Bool;
    extern function __string_compare(x:String, y:String) : Ordering;
    extern function __string_show(s:String) : String;
    extern function __string_hash(s:String) : Int;

    /// StringExt ops
    extern function __string_charAt(s:String, i:Int) : Char;
    extern function __string_substring(s:String, start:Int, end:Int) : String;
    extern function __string_indexOf(haystack:String, needle:String) : Int;
    extern function __string_trim(s:String) : String;
    extern function __string_toUpper(s:String) : String;
    extern function __string_toLower(s:String) : String;
    extern function __string_startsWith(s:String, prefix:String) : Bool;
    extern function __string_endsWith(s:String, suffix:String) : Bool;
    extern function __string_replace(s:String, old:String, new:String) : String;
    extern function __string_parseInt(s:String) : Int;
    extern function __string_parseFloat(s:String) : Float64;
    extern function __string_split(s:String, delim:String) : List(String);
    extern function __string_join(xs:List(String), delim:String) : String;
    extern function __string_fromChar(c:Char) : String;
};

// ============================================================================
// Primitive op declarations — Char
// ============================================================================

target native {
    extern function __char_succ(c:Char) : Char;
    extern function __char_pred(c:Char) : Char;
    extern function __int_to_char(i:Int) : Char;
    extern function __char_to_int(c:Char) : Int;
    extern function __show_char(c:Char) : String;
    extern function __char_max() : Char;

    /// Char comparisons reuse i32 instructions (char is 32-bit Unicode codepoint)
    extern function __eq_i32(x:Char, y:Char) : Bool = inline "icmp eq";
    extern function __neq_i32(x:Char, y:Char) : Bool = inline "icmp ne";
    extern function __lt_i32(x:Char, y:Char) : Bool = inline "icmp ult";
    extern function __le_i32(x:Char, y:Char) : Bool = inline "icmp ule";
    extern function __gt_i32(x:Char, y:Char) : Bool = inline "icmp ugt";
    extern function __ge_i32(x:Char, y:Char) : Bool = inline "icmp uge";
};

// ============================================================================
// MutableRef effect handler
// ============================================================================

target native {

    handler NativeRef : MutableRef = {
        function newRef(val) = __newref(val);
        function readRef(r) = __readref(r);
        function writeRef(r, val) = __writeref(r, val);
        function modifyRef(r, f) = __modifyref(r, f)
    };

};

// ============================================================================
// MutableArray effect handler
// ============================================================================

target native {

    handler NativeMutArray : MutableArray = {
        function newMutArray(n, val) = __newmutarray(n, val);
        function mutRead(arr, i) = __mutread(arr, i);
        function mutWrite(arr, i, val) = __mutwrite(arr, i, val);
        function mutLength(arr) = __mutlength(arr);
        function freeze(arr) = __freeze(arr);
        function thaw(arr) = __thaw(arr);
        function mutPush(arr, val) = __mutpush(arr, val)
    };

};

// ============================================================================
// Primitive op declarations — IO / Effects
// ============================================================================

target native {
    extern function __print_string(s:String) : Unit;
    extern function __print_newline() : Unit;
    extern function __read_line() : String;
    extern function __file_read(path:String) : String;
    extern function __file_write(path:String, content:String) : Unit;
    extern function __file_append(path:String, content:String) : Unit;
    extern function __file_exists(path:String) : Bool;
    extern function __error(msg:String) : Unit;
    extern function __try_catch(body:Unit, h:Unit) : Unit;
};

// ============================================================================
// Primitive op declarations — Mutable Ref
// ============================================================================

target native {
    extern function __newref(val:Int) : Ref(Int);
    extern function __readref(r:Ref(Int)) : Int;
    extern function __writeref(r:Ref(Int), val:Int) : Unit;
    extern function __modifyref(r:Ref(Int), f:Int) : Unit;
};

// ============================================================================
// Primitive op declarations — Mutable Array
// ============================================================================

target native {
    extern function __newmutarray(n:Int, val:Int) : MutArray(Int);
    extern function __mutread(arr:MutArray(Int), i:Int) : Int;
    extern function __mutwrite(arr:MutArray(Int), i:Int, val:Int) : Unit;
    extern function __mutlength(arr:MutArray(Int)) : Int;
    extern function __freeze(arr:MutArray(Int)) : Array(Int);
    extern function __thaw(arr:Array(Int)) : MutArray(Int);
    extern function __mutpush(arr:MutArray(Int), val:Int) : Unit;
};

// ============================================================================
// Type conversions
// ============================================================================

target native {
    extern function __sitofp_i64(n:Int) : Float64 = inline "sitofp";
    extern function __fptosi_f64(x:Float64) : Int = inline "fptosi";
    extern function __fpext_f32(x:Float32) : Float64 = inline "fpext";
    extern function __fptrunc_f64(x:Float64) : Float32 = inline "fptrunc";
};

// ============================================================================
// Clock
// ============================================================================

target native {
    /// Monotonic clock returning nanoseconds since an arbitrary epoch.
    extern function __clock_nanos() : Int;

    handler NativeClock : Clock = {
        function clockNanos() = __clock_nanos()
    };
};
