/// Multi-function native compilation test
function myDouble(x:Int) : Int = x + x;
function myQuad(x:Int) : Int = myDouble(myDouble(x));
function myMain() : Int = myQuad(3);
