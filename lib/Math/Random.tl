/// Random number generation: pure algebras and effect layer.
///
/// Pure layer (no effects):
///   RandomGen: PRNG state algebra (next, split, fromSeed).
///   SampleDist: sample from distributions with explicit generator state.
///   RandomChoice: shuffle, choice, choiceN with explicit generator state.
///
/// Effect layer (convenience):
///   Random effect: implicit generator state via effect system.
///   SampleDistEff: sample from distributions using Random effect.
///   RandomChoiceEff: shuffle/choice using Random effect.
///
/// The pure layer enables GPU-accelerated bulk sampling (no effect overhead
/// in hot loops). The effect layer provides ergonomic imperative-style usage.
module Math.Random;
import Algebra;
import Math.Stats;
import Categorical;
import Collection;

// ============================================================================
// RandomGen (pure algebra)
// ============================================================================

/// Pseudorandom number generator state.
/// Pure functional interface: caller threads generator state explicitly.
/// Implementations: Xoshiro256**, PCG, SplitMix64, Mersenne Twister.
algebra RandomGen(g:Type) = {
    /// Generate a random Int and advance the generator state.
    function next(gen:g) : {val:Int, gen:g};
    /// Generate a random Float64 in [0, 1) and advance the generator state.
    function nextFloat(gen:g) : {val:Float64, gen:g};
    /// Generate a random Int in [lo, hi] (inclusive) and advance the generator.
    function nextIntRange(gen:g, lo:Int, hi:Int) : {val:Int, gen:g};
    /// Split the generator into two independent generators.
    /// Essential for parallel random number generation.
    function split(gen:g) : {left:g, right:g};
    /// Create a generator from a seed value.
    function fromSeed(seed:Int) : g
};

// ============================================================================
// SampleDist (pure morphism)
// ============================================================================

/// Sample from probability distributions with explicit generator state.
/// `d` is the distribution type, `s` is the sample type, `g` is the generator type.
morphism SampleDist(d:Type, s:Type, g:Type)
    requires Distribution(d, s), RandomGen(g) = {
    /// Draw a single sample from the distribution.
    function sample(dist:d, gen:g) : {val:s, gen:g};
    /// Draw n independent samples from the distribution.
    function sampleN(dist:d, n:Int, gen:g) : {values:Array(s), gen:g};

    // Default: inverse CDF method (works for any distribution with icdf).
    function sample(dist:d, gen:g) : {val:s, gen:g} =
        let r = nextFloat(gen) in
        {val = icdf(dist, r.val), gen = r.gen}
};

// ============================================================================
// RandomChoice (pure morphism)
// ============================================================================

/// Random selection and shuffling with explicit generator state.
/// `c` is a container type constructor, `g` is the generator type.
morphism RandomChoice(c: Type -> Type, g:Type)
    requires Functor(c), RandomGen(g) = {
    /// Fisher-Yates shuffle: uniformly random permutation.
    function shuffle(xs:c(a), gen:g) : {val:c(a), gen:g};
    /// Choose a single element uniformly at random.
    function choice(xs:c(a), gen:g) : {val:a, gen:g};
    /// Choose n elements. With replacement if replace=True, without otherwise.
    function choiceN(xs:c(a), n:Int, gen:g, replace:Bool) : {val:Array(a), gen:g}
};

// ============================================================================
// Random effect
// ============================================================================

/// Random number generation as an effect.
/// Provides implicit generator state — no need to thread it manually.
/// Use `with SeededRandom(42) { ... }` or `with SystemRandom { ... }`.
effect Random = {
    /// Generate a random Int.
    function randInt() : Int;
    /// Generate a random Float64 in [0, 1).
    function randFloat() : Float64;
    /// Generate a random Int in [lo, hi] (inclusive).
    function randIntRange(lo:Int, hi:Int) : Int
};

// ============================================================================
// SampleDistEff (effectful morphism)
// ============================================================================

/// Sample from distributions using the Random effect.
/// Convenience layer over SampleDist for imperative-style code.
morphism SampleDistEff(d:Type, s:Type)
    requires Distribution(d, s) = {
    /// Draw a single sample (effectful).
    function sampleE(dist:d) : Eff { random: Random | r } s;
    /// Draw n samples (effectful).
    function sampleNE(dist:d, n:Int) : Eff { random: Random | r } Array(s)
};

// ============================================================================
// RandomChoiceEff (effectful morphism)
// ============================================================================

/// Random selection using the Random effect.
algebra RandomChoiceEff(c: Type -> Type)
    requires Functor(c) = {
    /// Shuffle (effectful).
    function shuffleE(xs:c(a)) : Eff { random: Random | r } c(a);
    /// Choose one element (effectful).
    function choiceE(xs:c(a)) : Eff { random: Random | r } a;
    /// Choose n elements (effectful).
    function choiceNE(xs:c(a), n:Int, replace:Bool) : Eff { random: Random | r } Array(a)
};
