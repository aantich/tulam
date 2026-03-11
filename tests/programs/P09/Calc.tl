module P09.Calc;
import P09.Token;

type CalcResult = CalcOk * val:Int + CalcErr;

function applyOp(op:Token, a:Int, b:Int) : CalcResult = match
    | TPlus, a, b  -> CalcOk(a + b)
    | TMinus, a, b -> CalcOk(a - b)
    | TStar, a, b  -> CalcOk(a * b)
    | op, a, b     -> CalcErr;

function evalSimple(a:Int, op:Token, b:Int) : CalcResult =
    applyOp(op, a, b);

function calcToMaybe(r:CalcResult) : Maybe(Int) = match
    | CalcOk(v) -> Just(v)
    | CalcErr   -> Nothing;

function safeCalc(a:Int, op:Token, b:Int) : Maybe(Int) =
    calcToMaybe(evalSimple(a, op, b));

function calcAndDouble(a:Int, op:Token, b:Int) : Maybe(Int) =
    bind(safeCalc(a, op, b), fn(r) = Just(r + r));
