type Digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9;

repr Digit as Int where {
    function toRepr(d:Digit) : Int = match
        | D0 -> 0 | D1 -> 1 | D2 -> 2 | D3 -> 3 | D4 -> 4
        | D5 -> 5 | D6 -> 6 | D7 -> 7 | D8 -> 8 | D9 -> 9;
    function fromRepr(i:Int) : Digit = match
        | 0 -> D0 | 1 -> D1 | 2 -> D2 | 3 -> D3 | 4 -> D4
        | 5 -> D5 | 6 -> D6 | 7 -> D7 | 8 -> D8 | n -> D9
};

function addDigits(a:Digit, b:Digit) : Int = (a as Int) + (b as Int);

function isEvenDigit(d:Digit) : Bool =
    let r = d as Int in
    mod(r, 2) == 0;

function nextDigit(d:Digit) : Digit =
    let r = d as Int in
    let r2 = if r >= 9 then 0 else r + 1 in
    r2 as Digit;
