/// Optimization, root finding, interpolation, quadrature, differentiation,
/// and ODE solving algebras.
///
/// RootFinder: bisection, Brent, Newton, secant methods.
/// Optimizer: unconstrained multivariate minimization.
/// OptMethod: named optimization algorithms (Nelder-Mead, BFGS, L-BFGS, CG).
/// ConstrainedOpt: linear and quadratic programming.
/// Interpolate: linear, cubic spline, polynomial interpolation.
/// Quadrature: numerical integration (adaptive, Gauss).
/// Differentiate: numerical derivatives, gradients, Jacobians.
/// ODESolver: ordinary differential equation solvers (RK4, adaptive).
module Math.Optimize;
import Algebra;
import Math.Numeric;
import Math.LinAlg;

// ============================================================================
// RootFinder
// ============================================================================

/// Root finding for scalar functions: find x such that f(x) = 0.
algebra RootFinder(s:Type) requires Floating(s) = {
    /// Bisection method. Requires f(lo) and f(hi) have opposite signs.
    /// Guaranteed convergence, linear rate.
    function bisect(f:s -> s, lo:s, hi:s, tol:s) : s;
    /// Brent's method. Combines bisection, secant, and inverse quadratic
    /// interpolation. Guaranteed convergence, superlinear rate.
    function brent(f:s -> s, lo:s, hi:s, tol:s) : s;
    /// Newton's method. Requires derivative df. Quadratic convergence
    /// near root, but may diverge far from root.
    function newton(f:s -> s, df:s -> s, x0:s, tol:s) : s;
    /// Secant method. Like Newton but approximates derivative from two points.
    /// Superlinear convergence (order ~1.618).
    function secant(f:s -> s, x0:s, x1:s, tol:s) : s
};

// ============================================================================
// Optimizer
// ============================================================================

/// Unconstrained multivariate optimization.
/// Finds local minima of f: R^n -> R.
algebra Optimizer(s:Type) requires Floating(s) = {
    /// Minimize f starting from x0. Derivative-free.
    function minimize(
        f:Array(s) -> s,
        x0:Array(s),
        tol:s
    ) : {point:Array(s), value:s, converged:Bool};

    /// Minimize f with known gradient. Faster convergence.
    function minimizeGrad(
        f:Array(s) -> s,
        grad:Array(s) -> Array(s),
        x0:Array(s),
        tol:s
    ) : {point:Array(s), value:s, converged:Bool}
};

// ============================================================================
// OptMethod
// ============================================================================

/// Named optimization algorithms for when the user wants to select a strategy.
algebra OptMethod(s:Type) requires Optimizer(s) = {
    /// Nelder-Mead simplex method. Derivative-free. Good for small dimensions.
    function nelderMead(
        f:Array(s) -> s,
        x0:Array(s),
        tol:s
    ) : {point:Array(s), value:s, converged:Bool};

    /// BFGS quasi-Newton method. Requires gradient. Superlinear convergence.
    function bfgs(
        f:Array(s) -> s,
        grad:Array(s) -> Array(s),
        x0:Array(s),
        tol:s
    ) : {point:Array(s), value:s, converged:Bool};

    /// Limited-memory BFGS. For large-scale problems (many variables).
    /// `m` controls memory (number of stored correction pairs, typically 5-20).
    function lbfgs(
        f:Array(s) -> s,
        grad:Array(s) -> Array(s),
        x0:Array(s),
        tol:s,
        m:Int
    ) : {point:Array(s), value:s, converged:Bool};

    /// Conjugate gradient method. Requires gradient. Low memory.
    function conjugateGradient(
        f:Array(s) -> s,
        grad:Array(s) -> Array(s),
        x0:Array(s),
        tol:s
    ) : {point:Array(s), value:s, converged:Bool}
};

// ============================================================================
// ConstrainedOpt
// ============================================================================

/// Constrained optimization: linear and quadratic programming.
morphism ConstrainedOpt(m: Type -> Type, s:Type)
    requires MatrixOps(m, s), Optimizer(s) = {
    /// Linear programming: minimize c^T * x subject to A_ineq * x <= b_ineq
    /// and A_eq * x = b_eq. Uses simplex or interior-point method.
    function linprog(
        c:Array(s),
        aIneq:m(s), bIneq:Array(s),
        aEq:m(s), bEq:Array(s)
    ) : {point:Array(s), value:s};

    /// Quadratic programming: minimize 0.5 * x^T * Q * x + c^T * x
    /// subject to A_ineq * x <= b_ineq and A_eq * x = b_eq.
    function quadprog(
        q:m(s), c:Array(s),
        aIneq:m(s), bIneq:Array(s),
        aEq:m(s), bEq:Array(s)
    ) : {point:Array(s), value:s}
};

// ============================================================================
// Interpolate
// ============================================================================

/// Interpolation: estimate values between known data points.
algebra Interpolate(s:Type) requires Floating(s) = {
    /// Linear interpolation between data points.
    function interpLinear(xs:Array(s), ys:Array(s), x:s) : s;
    /// Natural cubic spline interpolation. Smooth (C^2 continuous).
    function interpCubicSpline(xs:Array(s), ys:Array(s), x:s) : s;
    /// Polynomial interpolation (Lagrange/Neville). Exact through all points
    /// but may oscillate (Runge's phenomenon) for many points.
    function interpPolynomial(xs:Array(s), ys:Array(s), x:s) : s
};

// ============================================================================
// Quadrature
// ============================================================================

/// Numerical integration (quadrature): approximate definite integrals.
algebra Quadrature(s:Type) requires Floating(s) = {
    /// General-purpose adaptive integration of f over [lo, hi].
    function integrate(f:s -> s, lo:s, hi:s, tol:s) : s;
    /// Gauss-Legendre quadrature with n nodes. Exact for polynomials of
    /// degree <= 2n-1.
    function gaussQuad(f:s -> s, lo:s, hi:s, n:Int) : s
};

// ============================================================================
// Differentiate
// ============================================================================

/// Numerical differentiation: approximate derivatives from function values.
algebra Differentiate(s:Type) requires Floating(s) = {
    /// First derivative via central difference: (f(x+h) - f(x-h)) / 2h.
    function diff(f:s -> s, x:s) : s;
    /// Second derivative via central difference: (f(x+h) - 2f(x) + f(x-h)) / h^2.
    function diff2(f:s -> s, x:s) : s;
    /// Gradient of a multivariate function: partial derivatives vector.
    function gradient(f:Array(s) -> s, x:Array(s)) : Array(s);
    /// Jacobian matrix of a vector-valued function: J[i,j] = df_i/dx_j.
    function jacobian(f:Array(s) -> Array(s), x:Array(s)) : Array(Array(s))
};

// ============================================================================
// ODESolver
// ============================================================================

/// Ordinary differential equation solvers.
/// Solves initial value problems: dy/dt = f(t, y), y(t0) = y0.
algebra ODESolver(s:Type) requires Floating(s) = {
    /// General-purpose ODE solver (implementation chooses method).
    /// `f`: t -> y -> dy/dt (the right-hand side).
    /// `y0`: initial state vector.
    /// `tStart`, `tEnd`: time interval.
    /// `dt`: step size.
    /// Returns arrays of time points and corresponding state vectors.
    function odesolve(
        f: s -> Array(s) -> Array(s),
        y0:Array(s),
        tStart:s,
        tEnd:s,
        dt:s
    ) : {ts:Array(s), ys:Array(Array(s))};

    /// Classical 4th-order Runge-Kutta (RK4). Fixed step size.
    /// The workhorse of scientific computing.
    function rk4(
        f: s -> Array(s) -> Array(s),
        y0:Array(s),
        tStart:s,
        tEnd:s,
        dt:s
    ) : {ts:Array(s), ys:Array(Array(s))};

    /// Adaptive Runge-Kutta (Dormand-Prince / RK45). Adjusts step size
    /// to maintain error within tolerance. More efficient for smooth problems.
    function adaptiveRK(
        f: s -> Array(s) -> Array(s),
        y0:Array(s),
        tStart:s,
        tEnd:s,
        tol:s
    ) : {ts:Array(s), ys:Array(Array(s))}
};
