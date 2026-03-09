module P09.Token;
type Token = TNum * val:Int + TPlus + TMinus + TStar + TEnd;

function isOperator(t:Token) : Bool = match
    | TNum(n) -> False
    | TPlus   -> True
    | TMinus  -> True
    | TStar   -> True
    | TEnd    -> False;
