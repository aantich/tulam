/// Comprehensive bytecode backend test suite — Core language features.
/// Run with: :load tests/programs/BCTest_Core.tl
/// Then:     :bc run run_all
///
/// Failing tests guide bytecode VM development.

/// ============================================================================
/// Test Harness Helpers
/// ============================================================================

/// Manual bool equality (avoids Eq(Bool) algebra dispatch)
function bool_eq(a:Bool, b:Bool) : Bool = match
    | True, True -> True
    | False, False -> True
    | _, _ -> False;

function assert_int(name:String, actual:Int, expected:Int) : Unit =
    action {
        if actual == expected
        then putStrLn(concat(concat("  PASS: ", name), concat(" = ", show(actual))))
        else putStrLn(concat(concat(concat(concat("  FAIL: ", name), " expected "), show(expected)), concat(" got ", show(actual))));
    };

function assert_bool(name:String, actual:Bool, expected:Bool) : Unit =
    action {
        if bool_eq(actual, expected)
        then putStrLn(concat(concat("  PASS: ", name), concat(" = ", show(actual))))
        else putStrLn(concat(concat(concat(concat("  FAIL: ", name), " expected "), show(expected)), concat(" got ", show(actual))));
    };

function assert_str(name:String, actual:String, expected:String) : Unit =
    action {
        if actual == expected
        then putStrLn(concat(concat("  PASS: ", name), concat(" = ", actual)))
        else putStrLn(concat(concat(concat(concat("  FAIL: ", name), " expected "), expected), concat(" got ", actual)));
    };

function assert_float(name:String, actual:Float64, expected:Float64) : Unit =
    action {
        if actual == expected
        then putStrLn(concat(concat("  PASS: ", name), concat(" = ", show(actual))))
        else putStrLn(concat(concat(concat(concat("  FAIL: ", name), " expected "), show(expected)), concat(" got ", show(actual))));
    };

function assert_char(name:String, actual:Char, expected:Char) : Unit =
    action {
        if actual == expected
        then putStrLn(concat(concat("  PASS: ", name), concat(" = ", show(actual))))
        else putStrLn(concat(concat(concat(concat("  FAIL: ", name), " expected "), show(expected)), concat(" got ", show(actual))));
    };

/// ============================================================================
/// Section 1: Integer Arithmetic (+, -, *, div, mod via EuclideanDomain)
/// ============================================================================

function test_add() : Int = 1 + 2;
function test_sub() : Int = 10 - 3;
function test_mul() : Int = 6 * 7;
function test_div() : Int = div(15, 4);
function test_mod() : Int = mod(17, 5);
function test_neg() : Int = negate(42);
function test_arith_complex() : Int = (3 + 4) * 2 - 1;
function test_arith_precedence() : Int = 2 + 3 * 4;

/// ============================================================================
/// Section 2: Float64 Arithmetic (+, -, *, / via Field)
/// ============================================================================

function test_fadd() : Float64 = 1.5 + 2.5;
function test_fmul() : Float64 = 3.0 * 2.0;
function test_fdiv() : Float64 = 10.0 / 4.0;
function test_fsub() : Float64 = 10.0 - 3.5;
function test_fneg() : Float64 = negate(1.0);

/// ============================================================================
/// Section 3: Comparison & Boolean Logic (Eq, Ord on Int)
/// ============================================================================

function test_eq_true() : Bool = 42 == 42;
function test_eq_false() : Bool = 42 == 43;
function test_neq() : Bool = 1 != 2;
function test_lt() : Bool = 3 < 5;
function test_gt() : Bool = 5 > 3;
function test_le() : Bool = 3 <= 3;
function test_ge() : Bool = 5 >= 5;
function test_lt_false() : Bool = 5 < 3;
function test_gt_false() : Bool = 3 > 5;
function test_not_true() : Bool = not(True);
function test_not_false() : Bool = not(False);

/// ============================================================================
/// Section 4: Conditionals
/// ============================================================================

function test_if_true() : Int = if True then 1 else 0;
function test_if_false() : Int = if False then 1 else 0;
function test_if_nested() : Int = if 3 > 2 then (if 1 == 1 then 42 else 0) else 99;
function test_if_chain() : Int = if 1 > 2 then 10 else if 2 > 3 then 20 else 30;

/// ============================================================================
/// Section 5: Let Expressions
/// ============================================================================

function test_let_simple() : Int = let x = 10 in x;
function test_let_multi() : Int = let { x = 10; y = 20 } in x + y;
function test_let_seq() : Int = let { x = 5; y = x * 2 } in y;
function test_let_func() : Int = let { double(n:Int) : Int = n * 2 } in double(21);
function test_let_nested() : Int = let x = 1 in let y = x + 1 in y + 1;
function test_let_shadow() : Int = let x = 10 in let x = 20 in x;

/// ============================================================================
/// Section 6: Pattern Matching — Sum Types
/// ============================================================================

type Color3 = Red3 + Green3 + Blue3;

function color_to_int(c:Color3) : Int = match
    | Red3   -> 1
    | Green3 -> 2
    | Blue3  -> 3;

function test_match_red() : Int = color_to_int(Red3);
function test_match_green() : Int = color_to_int(Green3);
function test_match_blue() : Int = color_to_int(Blue3);

/// Wildcard patterns
function is_red(c:Color3) : Bool = match
    | Red3 -> True
    | _    -> False;

function test_wildcard_match() : Bool = is_red(Red3);
function test_wildcard_miss() : Bool = is_red(Blue3);

/// ============================================================================
/// Section 7: Maybe (constructors: Nothing, Just; functions: isNothing, fromMaybe)
/// ============================================================================

function safe_div(a:Int, b:Int) : Maybe(Int) =
    if b == 0 then Nothing else Just(div(a, b));

function test_safe_div_ok() : Int = fromMaybe(0, safe_div(10, 3));
function test_safe_div_zero() : Int = fromMaybe(-1, safe_div(10, 0));
function test_just_extract() : Int = fromMaybe(0, Just(99));
function test_nothing_default() : Int = fromMaybe(-1, Nothing);
function test_isNothing_true() : Bool = isNothing(Nothing);
function test_isNothing_false() : Bool = isNothing(Just(1));

/// ============================================================================
/// Section 8: Recursion
/// ============================================================================

function factorial(n:Int) : Int = if n <= 1 then 1 else n * factorial(n - 1);
function test_factorial() : Int = factorial(10);
function test_factorial_base() : Int = factorial(0);
function test_factorial_one() : Int = factorial(1);

function fib(n:Int) : Int = if n <= 1 then n else fib(n - 1) + fib(n - 2);
function test_fib() : Int = fib(10);
function test_fib_zero() : Int = fib(0);
function test_fib_one() : Int = fib(1);

/// Tail-recursive accumulator style
function sum_to(n:Int, acc:Int) : Int = if n <= 0 then acc else sum_to(n - 1, acc + n);
function test_sum_to() : Int = sum_to(100, 0);

/// ============================================================================
/// Section 9: List Operations (constructors: Nil, Cons; functions: head, tail)
/// ============================================================================

function list_len(xs:List(Int)) : Int = match
    | Nil -> 0
    | Cons(h, t) -> 1 + list_len(t);

function list_sum(xs:List(Int)) : Int = match
    | Nil -> 0
    | Cons(h, t) -> h + list_sum(t);

function list_map_int(f:Int -> Int, xs:List(Int)) : List(Int) = match
    | g, Nil -> Nil
    | g, Cons(h, t) -> Cons(g(h), list_map_int(g, t));

function list_reverse_acc(xs:List(Int), acc:List(Int)) : List(Int) = match
    | Nil, a -> a
    | Cons(h, t), a -> list_reverse_acc(t, Cons(h, a));

function test_list_len() : Int = list_len(Cons(1, Cons(2, Cons(3, Nil))));
function test_list_sum() : Int = list_sum(Cons(10, Cons(20, Cons(30, Nil))));
function test_list_empty() : Int = list_len(Nil);
function test_list_map() : Int = list_sum(list_map_int(fn(x) = x * 2, Cons(1, Cons(2, Cons(3, Nil)))));
function test_list_reverse() : Int = list_sum(list_reverse_acc(Cons(1, Cons(2, Cons(3, Nil))), Nil));

/// head/tail from stdlib
function test_head_some() : Int = fromMaybe(0, head(Cons(42, Nil)));
function test_head_none() : Int = fromMaybe(-1, head(Nil));

/// ============================================================================
/// Section 10: Higher-Order Functions & Closures
/// ============================================================================

function apply_twice(f:Int -> Int, x:Int) : Int = f(f(x));
function test_apply_twice() : Int = apply_twice(fn(x) = x + 1, 10);

function make_adder(n:Int) : Int -> Int = fn(x) = x + n;
function test_closure() : Int = let add5 = make_adder(5) in add5(37);

function compose_int(f:Int -> Int, g:Int -> Int, x:Int) : Int = f(g(x));
function test_compose() : Int = compose_int(fn(x) = x * 2, fn(x) = x + 1, 20);

/// ============================================================================
/// Section 11: Show (intrinsic for Int, Float64, Bool, String, Char)
/// ============================================================================

function test_show_int() : String = show(42);
function test_show_neg() : String = show(negate(5));
function test_show_float() : String = show(3.14);
function test_show_bool_t() : String = show(True);
function test_show_bool_f() : String = show(False);
function test_show_char() : String = show('A');

/// ============================================================================
/// Section 12: String Operations (StringOps: concat, length; Eq/Ord on String)
/// ============================================================================

function test_str_len() : Int = length("hello");
function test_str_concat() : String = concat("hello", " world");
function test_str_eq() : Bool = "abc" == "abc";
function test_str_neq() : Bool = "abc" != "xyz";
function test_str_empty_len() : Int = length("");
function test_str_concat_empty() : String = concat("", "hello");
function test_str_lt() : Bool = "abc" < "abd";

/// ============================================================================
/// Section 13: Char Operations (Eq, Ord, Show, Enum on Char)
/// ============================================================================

function test_char_eq() : Bool = 'A' == 'A';
function test_char_neq() : Bool = 'A' != 'B';
function test_from_enum_char() : Int = fromEnum('A');
function test_to_enum_char() : Char = toEnum(65);

/// ============================================================================
/// Section 14: Bitwise Operations (Bits algebra on Int)
/// ============================================================================

function test_band() : Int = 12 .&. 10;
function test_bor() : Int = 12 .|. 3;
function test_bxor() : Int = xor(12, 10);
function test_bshl() : Int = shiftL(1, 4);
function test_bshr() : Int = shiftR(32, 2);

/// ============================================================================
/// Section 15: Absolute value & signum (Absolute algebra on Int/Float64)
/// ============================================================================

function test_abs_pos() : Int = abs(42);
function test_abs_neg() : Int = abs(negate(42));
function test_signum_pos() : Int = signum(99);
function test_signum_neg() : Int = signum(negate(99));
function test_signum_zero() : Int = signum(0);
function test_fabs() : Float64 = abs(negate(3.14));

/// ============================================================================
/// Section 16: Bounded & Enum (on Int and Char)
/// ============================================================================

function test_succ_int() : Int = succ(41);
function test_pred_int() : Int = pred(43);
function test_succ_char() : Char = succ('A');
function test_pred_char() : Char = pred('C');

/// ============================================================================
/// Section 17: Named Fields / Product Types (implicit constructors)
/// ============================================================================

type Point2 = x:Int * y:Int;

function point_sum(p:Point2) : Int = match
    | Point2(a, b) -> a + b;

function test_point_sum() : Int = point_sum(Point2(10, 32));

/// ============================================================================
/// Section 18: Nested Pattern Matching
/// ============================================================================

function nested_match(m:Maybe(Maybe(Int))) : Int = match
    | Nothing -> 0
    | Just(Nothing) -> 1
    | Just(Just(v)) -> v;

function test_nested_nothing() : Int = nested_match(Nothing);
function test_nested_just_nothing() : Int = nested_match(Just(Nothing));
function test_nested_just_just() : Int = nested_match(Just(Just(42)));

/// ============================================================================
/// Section 19: Either Type
/// ============================================================================

function either_val(e:Either(String, Int)) : Int = match
    | Left(s) -> negate(1)
    | Right(n) -> n;

function test_either_right() : Int = either_val(Right(42));
function test_either_left() : Int = either_val(Left("error"));

/// ============================================================================
/// Section 20: Pair (functions: fst, snd, swap from stdlib)
/// ============================================================================

function test_pair_fst() : Int = fst(Pair(42, 99));
function test_pair_snd() : Int = snd(Pair(42, 99));
function test_pair_swap_fst() : Int = fst(swap(Pair(1, 2)));
function test_pair_swap_snd() : Int = snd(swap(Pair(1, 2)));

/// ============================================================================
/// Section 21: Nat Operations (plus, mult from stdlib)
/// ============================================================================

function nat_to_int(n:Nat) : Int = match
    | Z -> 0
    | Succ(k) -> 1 + nat_to_int(k);

function test_nat_plus() : Int = nat_to_int(plus(Succ(Succ(Z)), Succ(Z)));
function test_nat_mult() : Int = nat_to_int(mult(Succ(Succ(Z)), Succ(Succ(Succ(Z)))));

/// ============================================================================
/// Section 22: Ordering Type
/// ============================================================================

function ord_to_int(o:Ordering) : Int = match
    | LessThan -> negate(1)
    | Equal -> 0
    | GreaterThan -> 1;

function test_compare_lt() : Int = ord_to_int(compare(1, 2));
function test_compare_eq() : Int = ord_to_int(compare(5, 5));
function test_compare_gt() : Int = ord_to_int(compare(9, 3));

/// ============================================================================
/// Section 23: Core Combinators (id, const, apply, compose, flip)
/// ============================================================================

function test_id() : Int = id(42);
function test_const() : Int = const(42, 99);
function test_apply() : Int = apply(fn(x) = x + 1, 41);
function test_compose_stdlib() : Int = compose(fn(x) = x * 2, fn(x) = x + 1, 20);
function test_flip() : Int = flip(fn(x, y) = x - y, 10, 42);

/// ============================================================================
/// Section 24: Morphism Convertible (convert Nat/Bool/Int)
/// ============================================================================

function test_convert_nat_to_int() : Int = convert(Succ(Succ(Z)));
function test_convert_bool_to_nat() : Int = nat_to_int(convert(True));

/// ============================================================================
/// Section 25: Float64 Comparison & Math
/// ============================================================================

function test_feq() : Bool = 1.0 == 1.0;
function test_flt() : Bool = 1.0 < 2.0;
function test_fgt() : Bool = 2.0 > 1.0;

/// ============================================================================
/// Section 26: Tree Type
/// ============================================================================

function tree_leaf_sum(t:Tree(Int)) : Int = match
    | Leaf(v) -> v
    | Branch(l, r) -> tree_leaf_sum(l) + tree_leaf_sum(r);

function test_tree_sum() : Int =
    tree_leaf_sum(Branch(Branch(Leaf(10), Leaf(20)), Leaf(12)));

/// ============================================================================
/// Section 27: NonEmpty List
/// ============================================================================

function test_ne_head() : Int = neHead(NonEmpty(42, Nil));

/// ============================================================================
/// Test Runner
/// ============================================================================

function run_all() : Unit = action {
    putStrLn("=== BCTest_Core: Comprehensive Bytecode Tests ===");
    putStrLn("");

    putStrLn("-- Section 1: Integer Arithmetic --");
    assert_int("1+2", test_add(), 3);
    assert_int("10-3", test_sub(), 7);
    assert_int("6*7", test_mul(), 42);
    assert_int("div(15,4)", test_div(), 3);
    assert_int("mod(17,5)", test_mod(), 2);
    assert_int("negate(42)", test_neg(), negate(42));
    assert_int("(3+4)*2-1", test_arith_complex(), 13);
    assert_int("2+3*4", test_arith_precedence(), 14);

    putStrLn("-- Section 2: Float64 Arithmetic --");
    assert_float("1.5+2.5", test_fadd(), 4.0);
    assert_float("3.0*2.0", test_fmul(), 6.0);
    assert_float("10.0/4.0", test_fdiv(), 2.5);
    assert_float("10.0-3.5", test_fsub(), 6.5);
    assert_float("negate(1.0)", test_fneg(), negate(1.0));

    putStrLn("-- Section 3: Comparison & Boolean --");
    assert_bool("42==42", test_eq_true(), True);
    assert_bool("42==43", test_eq_false(), False);
    assert_bool("1!=2", test_neq(), True);
    assert_bool("3<5", test_lt(), True);
    assert_bool("5>3", test_gt(), True);
    assert_bool("3<=3", test_le(), True);
    assert_bool("5>=5", test_ge(), True);
    assert_bool("5<3", test_lt_false(), False);
    assert_bool("3>5", test_gt_false(), False);
    assert_bool("not(True)", test_not_true(), False);
    assert_bool("not(False)", test_not_false(), True);

    putStrLn("-- Section 4: Conditionals --");
    assert_int("if True", test_if_true(), 1);
    assert_int("if False", test_if_false(), 0);
    assert_int("if nested", test_if_nested(), 42);
    assert_int("if chain", test_if_chain(), 30);

    putStrLn("-- Section 5: Let Expressions --");
    assert_int("let simple", test_let_simple(), 10);
    assert_int("let multi", test_let_multi(), 30);
    assert_int("let seq", test_let_seq(), 10);
    assert_int("let func", test_let_func(), 42);
    assert_int("let nested", test_let_nested(), 3);
    assert_int("let shadow", test_let_shadow(), 20);

    putStrLn("-- Section 6: Pattern Matching Sum Types --");
    assert_int("match Red3", test_match_red(), 1);
    assert_int("match Green3", test_match_green(), 2);
    assert_int("match Blue3", test_match_blue(), 3);
    assert_bool("wildcard match", test_wildcard_match(), True);
    assert_bool("wildcard miss", test_wildcard_miss(), False);

    putStrLn("-- Section 7: Maybe --");
    assert_int("safe_div ok", test_safe_div_ok(), 3);
    assert_int("safe_div zero", test_safe_div_zero(), negate(1));
    assert_int("Just extract", test_just_extract(), 99);
    assert_int("Nothing default", test_nothing_default(), negate(1));
    assert_bool("isNothing(Nothing)", test_isNothing_true(), True);
    assert_bool("isNothing(Just 1)", test_isNothing_false(), False);

    putStrLn("-- Section 8: Recursion --");
    assert_int("factorial(10)", test_factorial(), 3628800);
    assert_int("factorial(0)", test_factorial_base(), 1);
    assert_int("factorial(1)", test_factorial_one(), 1);
    assert_int("fib(10)", test_fib(), 55);
    assert_int("fib(0)", test_fib_zero(), 0);
    assert_int("fib(1)", test_fib_one(), 1);
    assert_int("sum_to(100)", test_sum_to(), 5050);

    putStrLn("-- Section 9: Lists --");
    assert_int("list_len", test_list_len(), 3);
    assert_int("list_sum", test_list_sum(), 60);
    assert_int("list_empty", test_list_empty(), 0);
    assert_int("list_map", test_list_map(), 12);
    assert_int("list_reverse", test_list_reverse(), 6);
    assert_int("head(Cons(42,Nil))", test_head_some(), 42);
    assert_int("head(Nil)", test_head_none(), negate(1));

    putStrLn("-- Section 10: HOF & Closures --");
    assert_int("apply_twice", test_apply_twice(), 12);
    assert_int("closure", test_closure(), 42);
    assert_int("compose", test_compose(), 42);

    putStrLn("-- Section 11: Show --");
    assert_str("show(42)", test_show_int(), "42");
    assert_str("show(-5)", test_show_neg(), "-5");
    assert_str("show(True)", test_show_bool_t(), "True");
    assert_str("show(False)", test_show_bool_f(), "False");
    assert_str("show('A')", test_show_char(), "'A'");

    putStrLn("-- Section 12: Strings --");
    assert_int("length(hello)", test_str_len(), 5);
    assert_str("concat", test_str_concat(), "hello world");
    assert_bool("str ==", test_str_eq(), True);
    assert_bool("str !=", test_str_neq(), True);
    assert_int("length empty", test_str_empty_len(), 0);
    assert_str("concat empty", test_str_concat_empty(), "hello");
    assert_bool("str <", test_str_lt(), True);

    putStrLn("-- Section 13: Chars --");
    assert_bool("char ==", test_char_eq(), True);
    assert_bool("char !=", test_char_neq(), True);
    assert_int("fromEnum A", test_from_enum_char(), 65);
    assert_char("toEnum 65", test_to_enum_char(), 'A');

    putStrLn("-- Section 14: Bitwise --");
    assert_int("12 .&. 10", test_band(), 8);
    assert_int("12 .|. 3", test_bor(), 15);
    assert_int("xor(12,10)", test_bxor(), 6);
    assert_int("shiftL(1,4)", test_bshl(), 16);
    assert_int("shiftR(32,2)", test_bshr(), 8);

    putStrLn("-- Section 15: Absolute & Signum --");
    assert_int("abs(42)", test_abs_pos(), 42);
    assert_int("abs(-42)", test_abs_neg(), 42);
    assert_int("signum(99)", test_signum_pos(), 1);
    assert_int("signum(-99)", test_signum_neg(), negate(1));
    assert_int("signum(0)", test_signum_zero(), 0);
    assert_float("abs(-3.14)", test_fabs(), 3.14);

    putStrLn("-- Section 16: Enum --");
    assert_int("succ(41)", test_succ_int(), 42);
    assert_int("pred(43)", test_pred_int(), 42);
    assert_char("succ A", test_succ_char(), 'B');
    assert_char("pred C", test_pred_char(), 'B');

    putStrLn("-- Section 17: Named Fields --");
    assert_int("point sum", test_point_sum(), 42);

    putStrLn("-- Section 18: Nested Pattern Matching --");
    assert_int("nested Nothing", test_nested_nothing(), 0);
    assert_int("nested Just(Nothing)", test_nested_just_nothing(), 1);
    assert_int("nested Just(Just 42)", test_nested_just_just(), 42);

    putStrLn("-- Section 19: Either --");
    assert_int("Right(42)", test_either_right(), 42);
    assert_int("Left(err)", test_either_left(), negate(1));

    putStrLn("-- Section 20: Pair --");
    assert_int("fst(42,99)", test_pair_fst(), 42);
    assert_int("snd(42,99)", test_pair_snd(), 99);
    assert_int("fst(swap(1,2))", test_pair_swap_fst(), 2);
    assert_int("snd(swap(1,2))", test_pair_swap_snd(), 1);

    putStrLn("-- Section 21: Nat --");
    assert_int("plus(2,1)", test_nat_plus(), 3);
    assert_int("mult(2,3)", test_nat_mult(), 6);

    putStrLn("-- Section 22: Ordering --");
    assert_int("compare(1,2)", test_compare_lt(), negate(1));
    assert_int("compare(5,5)", test_compare_eq(), 0);
    assert_int("compare(9,3)", test_compare_gt(), 1);

    putStrLn("-- Section 23: Combinators --");
    assert_int("id(42)", test_id(), 42);
    assert_int("const(42,99)", test_const(), 42);
    assert_int("apply(+1,41)", test_apply(), 42);
    assert_int("compose(*2,+1,20)", test_compose_stdlib(), 42);
    assert_int("flip(sub,10,42)", test_flip(), 32);

    putStrLn("-- Section 24: Convertible --");
    assert_int("convert Nat->Int", test_convert_nat_to_int(), 2);
    assert_int("convert Bool->Nat", test_convert_bool_to_nat(), 1);

    putStrLn("-- Section 25: Float64 Comparison --");
    assert_bool("1.0==1.0", test_feq(), True);
    assert_bool("1.0<2.0", test_flt(), True);
    assert_bool("2.0>1.0", test_fgt(), True);

    putStrLn("-- Section 26: Tree --");
    assert_int("tree sum", test_tree_sum(), 42);

    putStrLn("-- Section 27: NonEmpty --");
    assert_int("neHead", test_ne_head(), 42);

    putStrLn("");
    putStrLn("=== Done ===");
};
