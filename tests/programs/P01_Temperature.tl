type TempScale = Celsius + Fahrenheit + Kelvin;

function scaleId(s:TempScale) : Nat = match
    | Celsius    -> Z
    | Fahrenheit -> Succ(Z)
    | Kelvin     -> Succ(Succ(Z));

function isFreezing(temp:Int, scale:TempScale) : Bool = match
    | t, Celsius    -> t <= 0
    | t, Fahrenheit -> t <= 32
    | t, Kelvin     -> t <= 273;

function celsiusToFahr(c:Int) : Int = div(c * 9, 5) + 32;
function fahrToCelsius(f:Int) : Int = div((f - 32) * 5, 9);

function clampTemp(t:Int, lo:Int, hi:Int) : Int =
    if t < lo then lo
    else if t > hi then hi
    else t;
