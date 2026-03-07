// P14: Real Programs — Array HOFs, Universal Show, Effectful Traversal

// === Array HOFs ===

// Test 1: fmap over array
function testFmapIncr() : Array = fmap(\x -> x + 1, [1, 2, 3]);

// Test 2: fmap square
function testFmapSquare() : Array = fmap(\x -> x * x, [2, 3, 4]);

// Test 3: filter
function testFilter() : Array = filter(\x -> x > 2, [1, 2, 3, 4]);

// Test 4: foldl sum
function testFoldlSum() : Int = foldl(\acc, x -> acc + x, 0, [1, 2, 3]);

// Test 5: foldr sum
function testFoldrSum() : Int = foldr(\x, acc -> acc + x, 0, [1, 2, 3]);

// Test 6: generate
function testGenerate() : Array = generate(5, \i -> i * 2);

// Test 7: filter + length composition
function testFilterLength() : Int = length(filter(\x -> x > 0, [1, 0 - 2, 3]));

// Test 8: fmap on empty array
function testFmapEmpty() : Array = fmap(\x -> x, []);

// === Universal Show ===

// Test 9: show user-defined nullary constructor
type Color = Red | Green | Blue;
function testShowColor() : String = show(Red);

// Test 10: show user-defined type with fields
type Pair2 = MkPair(fst:Int, snd:Int);
function testShowPair() : String = show(MkPair(1, 2));

// Test 11: show array
function testShowArray() : String = show([1, 2, 3]);

// === Monadic Traversal (List + Maybe) ===

// Test 12: mapMMaybe with Maybe
function testMapMMaybe() = mapMMaybe(\x -> Just(x + 1), Cons(1, Cons(2, Nil)));

// Test 13: foldMMaybe with Maybe
function testFoldMMaybe() = foldMMaybe(\acc, x -> Just(acc + x), 0, Cons(1, Cons(2, Nil)));

// Test 14: sequenceMaybe with Maybe (all Just)
function testSequenceJust() = sequenceMaybe(Cons(Just(1), Cons(Just(2), Nil)));

// Test 15: sequenceMaybe with Maybe (short-circuits on Nothing)
function testSequenceNothing() = sequenceMaybe(Cons(Just(1), Cons(Nothing, Nil)));
