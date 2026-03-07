// P20: Class System Tests

// Basic class with fields and methods
class Animal(name:String, age:Int) = {
    function speak(self:Animal) : String = "...",
    function info(self:Animal) : String = self.name
};

// Subclass with override
class Dog(breed:String) extends Animal = {
    override function speak(self:Dog) : String = "Woof!",
    function fetch(self:Dog, item:String) : String = self.name
};

// Another subclass
class Cat() extends Animal = {
    override function speak(self:Cat) : String = "Meow!"
};

// Deeper hierarchy
class Puppy(toy:String) extends Dog = {
    override function speak(self:Puppy) : String = "Yip!"
};

// Abstract class
abstract class Shape(color:String) = {
    function describe(self:Shape) : String = self.color
};

class Circle(radius:Int) extends Shape = {
    function area(self:Circle) : Int = self.radius
};

// --- Test values ---

// T1: Basic construction and field access
function t1() : String = Animal.new("Rex", 5).name;

// T2: Field access - age
function t2() : Int = Animal.new("Rex", 5).age;

// T3: Method call on base class
function t3() : String = Animal.new("Rex", 5).speak();

// T4: Subclass construction with inherited field
function t4() : String = Dog.new("Buddy", 3, "Labrador").name;

// T5: Subclass own field
function t5() : String = Dog.new("Buddy", 3, "Labrador").breed;

// T6: Overridden method (dynamic dispatch)
function t6() : String = Dog.new("Buddy", 3, "Labrador").speak();

// T7: Inherited method
function t7() : String = Dog.new("Buddy", 3, "Labrador").info();

// T8: Cat construction and method
function t8() : String = Cat.new("Whiskers", 2).speak();

// T9: Deep hierarchy - Puppy
function t9() : String = Puppy.new("Max", 1, "Poodle", "ball").speak();

// T10: Puppy inherits Dog field
function t10() : String = Puppy.new("Max", 1, "Poodle", "ball").breed;

// T11: Puppy own field
function t11() : String = Puppy.new("Max", 1, "Poodle", "ball").toy;

// T12: Puppy inherits Animal field
function t12() : String = Puppy.new("Max", 1, "Poodle", "ball").name;

// T13: Method with extra args
function t13() : String = Dog.new("Buddy", 3, "Lab").fetch("stick");

// T14: Circle (extends abstract Shape)
function t14() : String = Circle.new("red", 5).describe();

// T15: Circle own method
function t15() : Int = Circle.new("red", 5).area();
