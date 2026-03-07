// P33: Operator Fixity and Precedence Tests

// ===== Group A: Basic precedence (prec 7, 6, 4) =====

// t1: multiplication binds tighter than addition
function t1() : Int = 2 + 3 * 4;

// t2: multiplication binds tighter than addition (reversed)
function t2() : Int = 3 * 4 + 2;

// t3: == binds looser than +
function t3() : Bool = 1 + 2 == 3;

// t4: == binds looser than *
function t4() : Bool = 2 * 3 == 6;

// t5: full chain: + and * and ==
function t5() : Bool = 1 + 2 * 3 == 7;

// t6: left-associativity of -
function t6() : Int = 1 - 2 - 3;

// t7: left-associativity of *
function t7() : Int = 2 * 3 * 4;

// t8: unary minus with binary op
function t8() : Int = 3 * -2 + 10;

// t9: parentheses override precedence
function t9() : Int = (2 + 3) * 4;

// t10: nested arithmetic
function t10() : Bool = 10 - 2 * 3 == 4;

// ===== Group B: More prec 7 vs 6 =====

// t11: + on left, * on right: 3 + 2 * 5 = 3 + 10 = 13
function t11() : Int = 3 + 2 * 5;

// t12: - on left, * on right, - on right: 20 - 3 * 4 - 1 = 20 - 12 - 1 = 7
function t12() : Int = 20 - 3 * 4 - 1;

// ===== Group C: Comparison operators (all prec 4) =====

// t13: < binds looser than +: (1 + 2) < 5
function t13() : Bool = 1 + 2 < 5;

// t14: > binds looser than *: (2 * 3) > 5
function t14() : Bool = 2 * 3 > 5;

// t15: <= binds looser than +: (3 + 2) <= 5
function t15() : Bool = 3 + 2 <= 5;

// t16: >= binds looser than *: (3 * 2) >= 6
function t16() : Bool = 3 * 2 >= 6;

// t17: != binds looser than +: (1 + 1) != 3
function t17() : Bool = 1 + 1 != 3;

// t18: < with * on both sides: (2 * 3) < (2 * 4)
function t18() : Bool = 2 * 3 < 2 * 4;

// ===== Group D: Bitwise operators (prec 3, 2) =====

// t19: .&. basic: 7 .&. 3 = 3
function t19() : Int = 7 .&. 3;

// t20: .|. basic: 4 .|. 2 = 6
function t20() : Int = 4 .|. 2;

// t21: .&. (prec 3) binds tighter than .|. (prec 2): 5 .|. 3 .&. 6 = 5 .|. (3 .&. 6) = 5 .|. 2 = 7
function t21() : Int = 5 .|. 3 .&. 6;

// t22: .|. (prec 2) binds looser than .&. (prec 3): 3 .&. 6 .|. 5 = (3 .&. 6) .|. 5 = 2 .|. 5 = 7
function t22() : Int = 3 .&. 6 .|. 5;

// t23: .&. right-assoc: 15 .&. 7 .&. 3 = 15 .&. (7 .&. 3) = 15 .&. 3 = 3
function t23() : Int = 15 .&. 7 .&. 3;

// t24: .|. right-assoc: 1 .|. 2 .|. 4 = 1 .|. (2 .|. 4) = 1 .|. 6 = 7
function t24() : Int = 1 .|. 2 .|. 4;

// ===== Group E: Cross-level precedence (bitwise vs comparison vs arithmetic) =====

// t25: == (prec 4) binds tighter than .&. (prec 3): can't test directly since == returns Bool
// Instead: arithmetic (6,7) > comparison (4) > bitwise AND (3) > bitwise OR (2)
// t25: + binds tighter than ==, which binds tighter than .&.
// This means: 1 + 2 == 3 .&. 1 should parse as ((1+2) == 3) .&. 1 ... but .&. on Bool isn't defined
// So test numeric: .&. binds looser than + and *
// 5 .&. 2 + 1 = 5 .&. (2 + 1) = 5 .&. 3 = 1
function t25() : Int = 5 .&. 2 + 1;

// t26: .|. binds looser than +: 4 .|. 1 + 2 = 4 .|. (1 + 2) = 4 .|. 3 = 7
function t26() : Int = 4 .|. 1 + 2;

// t27: .&. binds looser than *: 7 .&. 2 * 3 = 7 .&. (2 * 3) = 7 .&. 6 = 6
function t27() : Int = 7 .&. 2 * 3;

// t28: .|. binds looser than *: 4 .|. 1 * 2 = 4 .|. (1 * 2) = 4 .|. 2 = 6
function t28() : Int = 4 .|. 1 * 2;

// ===== Group F: Full precedence chain (7, 6, 3, 2 in one expression) =====

// t29: 1 .|. 2 .&. 3 + 1 * 2 = 1 .|. (2 .&. (3 + (1 * 2))) = 1 .|. (2 .&. (3 + 2)) = 1 .|. (2 .&. 5) = 1 .|. 0 = 1
function t29() : Int = 1 .|. 2 .&. 3 + 1 * 2;

// ===== Group G: Unary minus edge cases =====

// t30: unary minus tighter than *: -3 * 2 = (-3) * 2 = -6
function t30() : Int = -3 * 2;

// t31: a + -b * c = a + ((-b) * c): 10 + -2 * 3 = 10 + ((-2) * 3) = 10 + (-6) = 4
function t31() : Int = 10 + -2 * 3;

// t32: double unary minus: - -5 = 5
function t32() : Int = - -5;

// ===== Group H: Custom fixity declarations =====

// Define a custom right-associative operator at precedence 5
infixr 5 (<>);
function (<>)(x:Int, y:Int) : Int = x * 10 + y;

// t33: custom op at prec 5 binds looser than * (prec 7): a <> b * c = a <> (b * c)
// 2 <> 3 * 4 = 2 <> 12 = 2 * 10 + 12 = 32
function t33() : Int = 2 <> 3 * 4;

// t34: custom op at prec 5 binds tighter than == (prec 4): a <> b == c means (a <> b) == c
// 1 <> 2 == 12 -> (1 <> 2) == 12 -> 12 == 12 -> True
function t34() : Bool = 1 <> 2 == 12;

// t35: custom op right-assoc: a <> b <> c = a <> (b <> c)
// 1 <> 2 <> 3 = 1 <> (2 <> 3) = 1 <> (2 * 10 + 3) = 1 <> 23 = 1 * 10 + 23 = 33
function t35() : Int = 1 <> 2 <> 3;

// t36: if left-assoc, would be (1 <> 2) <> 3 = 12 <> 3 = 123
// Since right-assoc, result is 33 (tested in t35), verify NOT 123
function t36() : Bool = 1 <> 2 <> 3 != 123;

// Define a very low-precedence left-assoc operator
infixl 1 (>>>);
function (>>>)(x:Int, y:Int) : Int = x + y;

// t37: >>> at prec 1 binds loosest: a >>> b * c = a >>> (b * c)
// 1 >>> 2 * 3 = 1 >>> 6 = 7
function t37() : Int = 1 >>> 2 * 3;

// t38: >>> binds looser than .&. (prec 3): a >>> b .&. c = a >>> (b .&. c)
// 1 >>> 7 .&. 3 = 1 >>> 3 = 4
function t38() : Int = 1 >>> 7 .&. 3;

// t39: >>> left-assoc: a >>> b >>> c = (a >>> b) >>> c
// 1 >>> 2 >>> 3 = (1 >>> 2) >>> 3 = 3 >>> 3 = 6
function t39() : Int = 1 >>> 2 >>> 3;
