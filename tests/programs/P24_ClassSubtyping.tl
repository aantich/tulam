// P24: Class Subtyping and Safe Downcast Tests

// Class hierarchy (self-contained — no dependency on P20)
class Animal(name:String, age:Int) = {
    function speak(self:Animal) : String = "...";
};
class Dog(breed:String) extends Animal = {
    function speak(self:Dog) : String = "Woof!";
};
class Cat() extends Animal = {
    function speak(self:Cat) : String = "Meow!";
};
class Puppy(toy:String) extends Dog = {
    function speak(self:Puppy) : String = "Yap!";
};

// T1: Pass subclass to function expecting superclass (value-level polymorphism)
function animalName(a:Animal) : String = a.name;
function t1() : String = animalName(Dog.new("Rex", 5, "Lab"));

// T2: Nested subtyping — grandchild passed where grandparent expected
function t2() : String = animalName(Puppy.new("Tiny", 1, "Poodle", "bone"));

// T3: Pattern match on subclass constructors (function-level match)
function classifyAnimal(a:Animal) : String = match
    | Dog(n, age, breed) -> breed
    | Cat(n, age) -> "cat"
    | _ -> "other";
function t3() : String = classifyAnimal(Dog.new("Rex", 5, "Lab"));

// T4: Pattern match Cat branch
function t4() : String = classifyAnimal(Cat.new("Whiskers", 2));

// T5: Pattern match wildcard (base Animal)
function t5() : String = classifyAnimal(Animal.new("Generic", 3));

// T6-T9: Safe downcast — helper for matching Maybe(Dog)
function matchDogBreed(m:Maybe(Dog)) : String = match
    | Just(d) -> d.breed
    | Nothing -> "not a dog";

// T6: Safe downcast succeeds — Dog as Dog → Just(Dog)
function t6() : String = matchDogBreed(Dog.new("Rex", 5, "Lab") as Dog);

// T7: Safe downcast fails — Cat as Dog → Nothing
function t7() : String = matchDogBreed(Cat.new("Whiskers", 2) as Dog);

// T8: Downcast Puppy as Dog succeeds (Puppy <: Dog)
function t8() : String = matchDogBreed(Puppy.new("Max", 1, "Poodle", "ball") as Dog);

// T9: Downcast Animal as Dog fails (Animal is NOT a subclass of Dog)
function t9() : String = matchDogBreed(Animal.new("Generic", 3) as Dog);
