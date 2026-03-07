// P23: Extern Class Plumbing Tests

// T1: Target import parses without error
import System.Windows.Forms target dotnet;

// T2: Normal tulam class still works fine after target import
class Widget(label:String) = {
    function getLabel(self:Widget) : String = self.label
};

// T3: Subclassing tulam class works normally
class Button(onClick:String) extends Widget = {
    function click(self:Button) : String = self.onClick
};

// --- Test values ---
function t1() : String = "parsed";
function t2() : String = Widget.new("hello").getLabel();
function t3() : String = Button.new("hello", "clicked").click();
