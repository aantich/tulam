// P21: Sealed Class Tests

// Sealed class with 2 subclasses (same file = allowed)
sealed class Expr(exprTag:Int) = {
    function eval(self:Expr) : Int = self.exprTag
};

class NumExpr(numVal:Int) extends Expr = {
    override function eval(self:NumExpr) : Int = self.numVal
};

class AddExpr(addLeft:Int, addRight:Int) extends Expr = {
    override function eval(self:AddExpr) : Int = (self.addLeft) + (self.addRight)
};

// Sealed with field access and methods
sealed class Token(tokenKind:String) = {
    function describe(self:Token) : String = self.tokenKind
};

class WordTok(wordText:String) extends Token = {
    override function describe(self:WordTok) : String = self.wordText
};

class PunctTok(punctSym:String) extends Token = {
    override function describe(self:PunctTok) : String = self.punctSym
};

// --- Test values ---

// T1: Construct sealed subclass NumExpr and eval
function t1() : Int = NumExpr.new(0, 42).eval();

// T2: Construct sealed subclass AddExpr and eval
function t2() : Int = AddExpr.new(0, 3, 4).eval();

// T3: Field access on sealed subclass
function t3() : Int = NumExpr.new(0, 99).numVal;

// T4: WordTok token describe
function t4() : String = WordTok.new("word", "hello").describe();

// T5: PunctTok token describe
function t5() : String = PunctTok.new("punct", "!").describe();
