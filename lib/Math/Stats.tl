/// Statistics algebras and morphisms.
///
/// Descriptive: mean, variance, median, quantiles, skewness, kurtosis.
/// Correlation: pearson, spearman, kendall, covariance.
/// CovarianceOps: covariance/correlation matrices, Mahalanobis distance.
/// Distribution: pdf, cdf, icdf for probability distributions.
/// DiscreteDistribution: pmf and finite support.
/// TestResult: hypothesis test output (statistic, p-value).
/// ParametricTest: t-test, chi-squared, ANOVA, F-test, z-test.
/// NonParametricTest: Mann-Whitney, Wilcoxon, Kruskal-Wallis, KS test.
/// Regression: OLS, ridge, lasso, polynomial.
/// RegressionResult: coefficients, R-squared, residuals, predict.
/// CurveFit: nonlinear least-squares (Levenberg-Marquardt).
///
/// Container parameters use `Type -> Type` to work with Array, DenseVector, etc.
module Math.Stats;
import Algebra;
import Math.Numeric;
import Math.LinAlg;
import Categorical;
import Collection;

// ============================================================================
// Descriptive statistics
// ============================================================================

/// Descriptive statistics on bulk containers of ordered field elements.
/// `c` is a container type constructor (Array, etc.), `s` is the scalar type.
morphism Descriptive(c: Type -> Type, s:Type)
    requires Functor(c), Ord(s), Field(s) = {
    /// Arithmetic mean.
    function mean(xs:c(s)) : s;
    /// Population variance (divided by N).
    function variance(xs:c(s)) : s;
    /// Sample variance (divided by N-1).
    function varianceSample(xs:c(s)) : s;
    /// Population standard deviation.
    function stddev(xs:c(s)) : s;
    /// Sample standard deviation.
    function stddevSample(xs:c(s)) : s;
    /// Median (middle value).
    function median(xs:c(s)) : s;
    /// Quantile: q in [0,1]. quantile(xs, 0.5) === median(xs).
    function quantile(xs:c(s), q:Float64) : s;
    /// Minimum value.
    function dmin(xs:c(s)) : s;
    /// Maximum value.
    function dmax(xs:c(s)) : s;
    /// Sum of all elements.
    function dsum(xs:c(s)) : s;
    /// Product of all elements.
    function dprod(xs:c(s)) : s;
    /// Skewness (third standardized moment). Measures asymmetry.
    function skewness(xs:c(s)) : s;
    /// Kurtosis (fourth standardized moment). Measures tail heaviness.
    function kurtosis(xs:c(s)) : s;

    // Defaults
    function stddev(xs:c(s)) : s = sqrt(variance(xs));
    function stddevSample(xs:c(s)) : s = sqrt(varianceSample(xs))
};

// ============================================================================
// Correlation
// ============================================================================

/// Correlation and covariance measures between paired samples.
morphism Correlation(c: Type -> Type, s:Type)
    requires Descriptive(c, s) = {
    /// Sample covariance between two equal-length samples.
    function covariance(xs:c(s), ys:c(s)) : s;
    /// Pearson correlation coefficient (linear correlation, [-1,1]).
    function pearson(xs:c(s), ys:c(s)) : s;
    /// Spearman rank correlation coefficient.
    function spearman(xs:c(s), ys:c(s)) : s;
    /// Kendall tau rank correlation coefficient.
    function kendall(xs:c(s), ys:c(s)) : s;
    /// Autocorrelation at a given lag.
    function autocorrelation(xs:c(s), lag:Int) : s;

    // Defaults
    function pearson(xs:c(s), ys:c(s)) : s =
        covariance(xs, ys) / (stddev(xs) * stddev(ys));

    law pearsonBounds(xs:c(s), ys:c(s)) =
        pearson(xs, ys) >= negate(one) ;
    law pearsonSelf(xs:c(s)) =
        pearson(xs, xs) ~= one
};

// ============================================================================
// CovarianceOps (matrix-level statistics)
// ============================================================================

/// Covariance and correlation matrices for multivariate data.
/// Bridges statistics and linear algebra.
morphism CovarianceOps(m: Type -> Type, c: Type -> Type, s:Type)
    requires MatrixOps(m, s), Descriptive(c, s) = {
    /// Covariance matrix: N observations x P variables -> P x P matrix.
    function covMatrix(data:Array(c(s))) : m(s);
    /// Correlation matrix (standardized covariance).
    function corMatrix(data:Array(c(s))) : m(s);
    /// Mahalanobis distance: distance from a point to a distribution.
    function mahalanobis(x:c(s), mu:c(s), covInv:m(s)) : s
};

// ============================================================================
// Distribution
// ============================================================================

/// Probability distribution interface.
/// `d` is the distribution type (e.g. Normal, Poisson),
/// `s` is the sample space type (e.g. Float64, Int).
/// Pure functions only — no random sampling here (see Math.Random for that).
morphism Distribution(d:Type, s:Type) = {
    /// Probability density function (continuous) or mass function (discrete).
    function pdf(dist:d, x:s) : Float64;
    /// Log-density. Numerically stable for extreme values.
    function logpdf(dist:d, x:s) : Float64;
    /// Cumulative distribution function: P(X <= x).
    function cdf(dist:d, x:s) : Float64;
    /// Inverse CDF (quantile function): given p, find x such that P(X <= x) = p.
    function icdf(dist:d, p:Float64) : s;
    /// Expected value (mean) of the distribution.
    function distMean(dist:d) : s;
    /// Variance of the distribution.
    function distVariance(dist:d) : s;

    // Defaults
    function logpdf(dist:d, x:s) : Float64 = log(pdf(dist, x))
};

/// Discrete distribution with enumerable support.
morphism DiscreteDistribution(d:Type, s:Type)
    requires Distribution(d, s) = {
    /// Probability mass function (synonym for pdf on discrete distributions).
    function pmf(dist:d, x:s) : Float64;
    /// Enumerable support of the distribution.
    function support(dist:d) : Array(s)
};

// ============================================================================
// Hypothesis testing
// ============================================================================

/// Result of a hypothesis test.
/// Parameterized so different tests can carry different metadata.
algebra TestResult(a:Type) = {
    /// The test statistic value (t, chi-squared, U, etc.).
    function statistic(r:a) : Float64;
    /// The p-value: probability of observing a result at least as extreme.
    function pvalue(r:a) : Float64;
    /// Is the result significant at the given alpha level?
    function significant(r:a, alpha:Float64) : Bool;

    // Default
    function significant(r:a, alpha:Float64) : Bool =
        pvalue(r) < alpha
};

/// Parametric hypothesis tests (assume distributional form).
morphism ParametricTest(c: Type -> Type, s:Type, r:Type)
    requires Descriptive(c, s), TestResult(r) = {
    /// One-sample t-test: is the mean of xs equal to mu0?
    function tTest1(xs:c(s), mu0:s) : r;
    /// Two-sample t-test: are the means of xs and ys equal?
    function tTest2(xs:c(s), ys:c(s)) : r;
    /// Paired t-test: are the paired differences zero-mean?
    function tTestPaired(xs:c(s), ys:c(s)) : r;
    /// Chi-squared goodness-of-fit: do observed frequencies match expected?
    function chiSquaredTest(observed:c(s), expected:c(s)) : r;
    /// One-way ANOVA: are the means of multiple groups equal?
    function anovaOneWay(groups:Array(c(s))) : r;
    /// F-test: are the variances of two samples equal?
    function fTest(xs:c(s), ys:c(s)) : r;
    /// Z-test: is the mean of xs equal to mu0? (known population variance sigma).
    function zTest(xs:c(s), mu0:s, sigma:s) : r
};

/// Non-parametric hypothesis tests (distribution-free).
morphism NonParametricTest(c: Type -> Type, s:Type, r:Type)
    requires Descriptive(c, s), TestResult(r) = {
    /// Mann-Whitney U test: are two independent samples from the same distribution?
    function mannWhitneyU(xs:c(s), ys:c(s)) : r;
    /// Wilcoxon signed-rank test: are paired differences symmetric around zero?
    function wilcoxonSigned(xs:c(s), ys:c(s)) : r;
    /// Kruskal-Wallis test: non-parametric alternative to one-way ANOVA.
    function kruskalWallis(groups:Array(c(s))) : r;
    /// Two-sample Kolmogorov-Smirnov test.
    function ksTest2(xs:c(s), ys:c(s)) : r
};

// ============================================================================
// Regression
// ============================================================================

/// Result of a regression fit.
morphism RegressionResult(r:Type, s:Type) = {
    /// Fitted coefficients (one per predictor variable).
    function coefficients(result:r) : Array(s);
    /// Intercept term (bias).
    function intercept(result:r) : s;
    /// R-squared: proportion of variance explained.
    function rSquared(result:r) : s;
    /// Residuals: observed - predicted for each data point.
    function residuals(result:r) : Array(s);
    /// Predict y for a new observation x.
    function predict(result:r, x:Array(s)) : s
};

/// Regression methods.
morphism Regression(c: Type -> Type, s:Type, r:Type)
    requires Descriptive(c, s), RegressionResult(r, s) = {
    /// Ordinary least squares: min ||X*beta - y||^2.
    function ols(x:Array(c(s)), y:c(s)) : r;
    /// Ridge regression: OLS + L2 penalty (lambda * ||beta||^2).
    function ridge(x:Array(c(s)), y:c(s), lambda:s) : r;
    /// Lasso regression: OLS + L1 penalty (lambda * ||beta||_1).
    function lasso(x:Array(c(s)), y:c(s), lambda:s) : r;
    /// Polynomial fit: fit y = a0 + a1*x + a2*x^2 + ... + aN*x^N.
    function polyFit(xs:c(s), ys:c(s), degree:Int) : r
};

// ============================================================================
// CurveFit
// ============================================================================

/// Nonlinear least-squares curve fitting (Levenberg-Marquardt).
algebra CurveFit(s:Type) requires Floating(s) = {
    /// Fit a parameterized model to data points.
    /// `model`: params -> x -> predicted_y.
    /// `xs`, `ys`: observed data.
    /// `initial`: starting parameter guess.
    /// Returns optimized parameters and residual norm.
    function curveFit(
        model: Array(s) -> s -> s,
        xs:Array(s),
        ys:Array(s),
        initial:Array(s)
    ) : {params:Array(s), residual:s}
};
