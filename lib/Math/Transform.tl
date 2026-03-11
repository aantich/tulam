/// Signal transforms and convolution algebras.
///
/// Fourier: FFT/IFFT, real FFT, 2D FFT (maps to FFTW / cuFFT).
/// DiscreteTrigTransform: DCT/DST and inverses.
/// Convolve: convolution and cross-correlation.
///
/// All morphisms are parameterized over container (Type -> Type) and scalar
/// (Type), so the same contract works for Array, DenseVector, GPU arrays, etc.
module Math.Transform;
import Algebra;
import Math.Numeric;
import Categorical;
import Collection;

// ============================================================================
// Fourier
// ============================================================================

/// Fourier transforms.
/// Maps to FFTW (CPU) or cuFFT (GPU) via target-qualified intrinsic instances.
///
/// Convention: complex values are represented as interleaved arrays
/// [re0, im0, re1, im1, ...] for the complex variants. Real variants
/// (rfft/irfft) take real input and produce half-complex output.
morphism Fourier(c: Type -> Type, s:Type)
    requires Functor(c), Floating(s) = {
    /// Complex forward FFT. Input/output: interleaved [re,im,...].
    function fft(xs:c(s)) : c(s);
    /// Complex inverse FFT.
    function ifft(xs:c(s)) : c(s);
    /// Real-input forward FFT. Input: N real values. Output: N/2+1 complex values (interleaved).
    function rfft(xs:c(s)) : c(s);
    /// Real-output inverse FFT. Input: N/2+1 complex values. Output: N real values.
    function irfft(xs:c(s)) : c(s);
    /// 2D forward FFT (row-major, interleaved complex).
    function fft2(xs:Array(c(s))) : Array(c(s));
    /// 2D inverse FFT.
    function ifft2(xs:Array(c(s))) : Array(c(s));

    law roundtrip(xs:c(s)) = ifft(fft(xs)) ~= xs;
    /// Parseval's theorem: energy is preserved by the transform.
    law parseval(xs:c(s)) =
        dsum(fmap(fn(x) = x * x, xs)) ~= dsum(fmap(fn(x) = x * x, fft(xs)))
};

// ============================================================================
// DiscreteTrigTransform
// ============================================================================

/// Discrete cosine and sine transforms.
/// DCT-II (the "DCT") is used in JPEG, MP3, etc.
/// DCT-III (the "inverse DCT") inverts DCT-II.
morphism DiscreteTrigTransform(c: Type -> Type, s:Type)
    requires Functor(c), Floating(s) = {
    /// Discrete cosine transform (type II).
    function dct(xs:c(s)) : c(s);
    /// Inverse discrete cosine transform (type III).
    function idct(xs:c(s)) : c(s);
    /// Discrete sine transform (type II).
    function dst(xs:c(s)) : c(s);
    /// Inverse discrete sine transform (type III).
    function idst(xs:c(s)) : c(s);

    law dctRoundtrip(xs:c(s)) = idct(dct(xs)) ~= xs;
    law dstRoundtrip(xs:c(s)) = idst(dst(xs)) ~= xs
};

// ============================================================================
// Convolve
// ============================================================================

/// Convolution and cross-correlation.
/// Convolution is the fundamental operation of signal processing and
/// convolutional neural networks. For large inputs, implementations
/// should use FFT-based convolution (O(n log n) vs O(n^2)).
morphism Convolve(c: Type -> Type, s:Type)
    requires Functor(c), Ring(s) = {
    /// Linear convolution: (f * g)[n] = sum_k f[k] * g[n-k].
    function convolve(xs:c(s), kernel:c(s)) : c(s);
    /// Cross-correlation: (f ⋆ g)[n] = sum_k f[k] * g[n+k].
    function correlate(xs:c(s), kernel:c(s)) : c(s);

    law convolveCommutative(xs:c(s), ys:c(s)) =
        convolve(xs, ys) ~= convolve(ys, xs);
    law convolveAssociative(xs:c(s), ys:c(s), zs:c(s)) =
        convolve(convolve(xs, ys), zs) ~= convolve(xs, convolve(ys, zs))
};
