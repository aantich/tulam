module P09.Main;
import P09.Token;
import P09.Calc;

function test_add() : CalcResult = evalSimple(3, TPlus, 4);
function test_mul() : CalcResult = evalSimple(5, TStar, 6);
function test_err() : CalcResult = evalSimple(1, TEnd, 2);
function test_monadic() : Maybe(Int) = calcAndDouble(10, TPlus, 5);
function test_err_monadic() : Maybe(Int) = calcAndDouble(1, TEnd, 2);
