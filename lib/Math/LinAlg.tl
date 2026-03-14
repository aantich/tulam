/// Linear algebra algebras and morphisms.
///
/// VectorSpace: scalar-vector operations (BLAS Level 1).
/// MatVecOps: matrix-vector operations (BLAS Level 2).
/// MatrixOps: matrix-matrix operations (BLAS Level 3).
/// TriangularOps: triangular/symmetric specializations.
/// Decompose: matrix factorizations (LU, QR, Cholesky, SVD, Schur).
/// Solve: linear system solvers, inverse, determinant.
/// Eigen: eigenvalue/eigenvector problems.
///
/// All morphisms are parameterized over the matrix/vector type constructor
/// (Type -> Type) and the scalar type (Type), so the same contract works
/// for DenseMatrix, SparseMatrix, BandMatrix, etc.
module Math.LinAlg;
import Algebra;
import Math.Numeric;

// ============================================================================
// VectorSpace (BLAS Level 1)
// ============================================================================

/// Scalar-vector operations.
/// `v` is a vector type constructor (e.g. DenseVector), `s` is the scalar type.
/// Maps to BLAS Level 1 routines: dscal, daxpy, ddot, dnrm2, dasum, idamax.
morphism VectorSpace(v: Type -> Type, s:Type)
    requires Group(v(s)) as Additive, Field(s) = {
    /// Scale a vector by a scalar (BLAS: xSCAL).
    function vscale(alpha:s, x:v(s)) : v(s);
    /// alpha*x + y (BLAS: xAXPY). Fused multiply-add on vectors.
    function axpy(alpha:s, x:v(s), y:v(s)) : v(s);
    /// Inner product / dot product (BLAS: xDOT).
    function dot(x:v(s), y:v(s)) : s;
    /// L1 norm: sum of absolute values (BLAS: xASUM).
    function norm1(x:v(s)) : s;
    /// L2 norm: Euclidean length (BLAS: xNRM2).
    function norm2(x:v(s)) : s;
    /// L-infinity norm: maximum absolute value (BLAS: IxAMAX).
    function normInf(x:v(s)) : s;
    /// Dimension (number of elements).
    function vdim(x:v(s)) : Int;

    // Defaults
    function axpy(alpha:s, x:v(s), y:v(s)) : v(s) = vscale(alpha, x) + y;

    law linearScale(a:s, b:s, x:v(s)) =
        vscale(a + b, x) === vscale(a, x) + vscale(b, x);
    law scaleAssoc(a:s, b:s, x:v(s)) =
        vscale(a * b, x) === vscale(a, vscale(b, x));
    law dotCommutative(x:v(s), y:v(s)) =
        dot(x, y) ~= dot(y, x);
    law dotLinear(a:s, x:v(s), y:v(s)) =
        dot(vscale(a, x), y) ~= a * dot(x, y)
};

// ============================================================================
// MatVecOps (BLAS Level 2)
// ============================================================================

/// Matrix-vector operations.
/// `m` is a matrix type constructor, `v` is a vector type constructor,
/// `s` is the scalar type.
/// Maps to BLAS Level 2 routines: dgemv, dger.
morphism MatVecOps(m: Type -> Type, v: Type -> Type, s:Type)
    requires VectorSpace(v, s) = {
    /// Matrix-vector product: A * x (BLAS: xGEMV with no transpose).
    function matvec(a:m(s), x:v(s)) : v(s);
    /// Transpose matrix-vector product: A^T * x (BLAS: xGEMV with transpose).
    function matvecTrans(a:m(s), x:v(s)) : v(s);
    /// Outer product: x * y^T, producing a matrix (BLAS: xGER).
    function outerProduct(x:v(s), y:v(s)) : m(s);
    /// Rank-1 update: A + alpha * x * y^T (BLAS: xGER applied to existing matrix).
    function rank1Update(alpha:s, x:v(s), y:v(s), a:m(s)) : m(s);

    law matvecLinear(a:m(s), x:v(s), y:v(s)) =
        matvec(a, x + y) ~= matvec(a, x) + matvec(a, y)
};

// ============================================================================
// MatrixOps (BLAS Level 3)
// ============================================================================

/// Matrix-matrix operations and basic accessors.
/// `m` is a matrix type constructor, `s` is the scalar type.
/// Maps to BLAS Level 3 (xGEMM) and basic matrix manipulation.
morphism MatrixOps(m: Type -> Type, s:Type)
    requires Field(s) = {
    /// Matrix multiplication: A * B (BLAS: xGEMM).
    function matMul(a:m(s), b:m(s)) : m(s);
    /// Transpose.
    function transpose(a:m(s)) : m(s);
    /// Number of rows.
    function mrows(a:m(s)) : Int;
    /// Number of columns.
    function mcols(a:m(s)) : Int;
    /// Element access: A[i,j].
    function mindex(a:m(s), i:Int, j:Int) : s;
    /// Construct from row-major nested arrays.
    function fromRows(xss:Array(Array(s))) : m(s);
    /// Extract as row-major nested arrays.
    function toRows(a:m(s)) : Array(Array(s));
    /// Identity matrix of size n.
    function identity(n:Int) : m(s);
    /// Diagonal matrix from an array of values.
    function diag(xs:Array(s)) : m(s);
    /// Trace: sum of diagonal elements.
    function trace(a:m(s)) : s;
    /// Scalar-matrix multiplication.
    function mscale(alpha:s, a:m(s)) : m(s);

    law mulAssoc(a:m(s), b:m(s), c:m(s)) =
        matMul(matMul(a, b), c) ~= matMul(a, matMul(b, c));
    law mulIdentity(a:m(s)) =
        matMul(a, identity(mcols(a))) ~= a;
    law transposeInvolution(a:m(s)) =
        transpose(transpose(a)) ~= a;
    law transposeMul(a:m(s), b:m(s)) =
        transpose(matMul(a, b)) ~= matMul(transpose(b), transpose(a))
};

// ============================================================================
// TriangularOps
// ============================================================================

/// Specialized operations for triangular and symmetric matrices.
/// Maps to BLAS: xTRSM, xTRMM, xSYMM.
morphism TriangularOps(m: Type -> Type, s:Type)
    requires MatrixOps(m, s) = {
    /// Solve triangular system: A*X = B where A is upper or lower triangular.
    /// (BLAS: xTRSM).
    function triSolve(a:m(s), b:m(s), upper:Bool) : m(s);
    /// Triangular matrix-matrix multiply (BLAS: xTRMM).
    function triMul(a:m(s), b:m(s), upper:Bool) : m(s);
    /// Symmetric matrix-matrix multiply: A*B where A is symmetric (BLAS: xSYMM).
    function symMul(a:m(s), b:m(s)) : m(s)
};

// ============================================================================
// Decompose (LAPACK factorizations)
// ============================================================================

/// Matrix factorizations.
/// Maps to LAPACK computational routines: xGETRF, xGEQRF, xPOTRF, xGESVD,
/// xGEHRD, xGEES.
morphism Decompose(m: Type -> Type, s:Type)
    requires MatrixOps(m, s) = {
    /// LU factorization with partial pivoting: P*A = L*U (LAPACK: xGETRF).
    function lu(a:m(s)) : {l:m(s), u:m(s), perm:Array(Int)};
    /// QR factorization: A = Q*R (LAPACK: xGEQRF + xORGQR).
    function qr(a:m(s)) : {q:m(s), r:m(s)};
    /// Cholesky factorization: A = L*L^T for positive-definite A (LAPACK: xPOTRF).
    function cholesky(a:m(s)) : m(s);
    /// Singular value decomposition: A = U * diag(sigma) * V^T (LAPACK: xGESVD).
    function svd(a:m(s)) : {u:m(s), sigma:Array(s), vt:m(s)};
    /// Schur decomposition: A = Z*T*Z^T (LAPACK: xGEES).
    function schur(a:m(s)) : {t:m(s), z:m(s)};
    /// Hessenberg reduction: A = Q*H*Q^T (LAPACK: xGEHRD).
    function hessenberg(a:m(s)) : {h:m(s), q:m(s)};
    /// Matrix rank (via SVD).
    function mrank(a:m(s)) : Int;
    /// Condition number (ratio of largest to smallest singular value).
    function cond(a:m(s)) : s
};

// ============================================================================
// Solve (LAPACK driver routines)
// ============================================================================

/// Linear system solvers, inverse, and determinant.
/// Maps to LAPACK driver routines: xGESV, xPOSV, xSYSV, xGELS.
morphism Solve(m: Type -> Type, s:Type)
    requires Decompose(m, s) = {
    /// General linear solve: A*X = B (LAPACK: xGESV).
    function solve(a:m(s), b:m(s)) : m(s);
    /// Symmetric solve: A*X = B where A is symmetric (LAPACK: xSYSV).
    function solveSymmetric(a:m(s), b:m(s)) : m(s);
    /// Positive-definite solve: A*X = B where A is pos-def (LAPACK: xPOSV).
    function solvePosdef(a:m(s), b:m(s)) : m(s);
    /// Triangular solve: A*X = B where A is triangular.
    function solveTriangular(a:m(s), b:m(s), upper:Bool) : m(s);
    /// Least-squares solve: min ||A*X - B||_2 (LAPACK: xGELS).
    function solveLeastSquares(a:m(s), b:m(s)) : m(s);
    /// Moore-Penrose pseudoinverse (via SVD).
    function pinv(a:m(s)) : m(s);
    /// General inverse (via LU).
    function inv(a:m(s)) : m(s);
    /// Determinant (via LU).
    function det(a:m(s)) : s;
    /// Log of absolute determinant. Numerically stable for large matrices.
    function logDet(a:m(s)) : s;

    // Defaults via factorizations
    function inv(a:m(s)) : m(s) =
        solve(a, identity(mrows(a)));

    law solveCorrect(a:m(s), b:m(s)) =
        matMul(a, solve(a, b)) ~= b;
    law invCorrect(a:m(s)) =
        matMul(a, inv(a)) ~= identity(mrows(a))
};

// ============================================================================
// Eigen (LAPACK eigenvalue routines)
// ============================================================================

/// Eigenvalue and eigenvector problems.
/// Maps to LAPACK: xSYEV/xSYEVD (symmetric), xGEEV (general),
/// xSYGV (generalized).
morphism Eigen(m: Type -> Type, s:Type)
    requires MatrixOps(m, s) = {
    /// Symmetric eigenvalue problem: A*v = lambda*v.
    /// Returns real eigenvalues (guaranteed) and orthogonal eigenvectors.
    /// (LAPACK: xSYEVD).
    function eigSymmetric(a:m(s)) : {values:Array(s), vectors:m(s)};
    /// Symmetric eigenvalues only (faster, no eigenvectors). (LAPACK: xSYEV).
    function eigvalSymmetric(a:m(s)) : Array(s);
    /// General eigenvalue problem. Eigenvalues may be complex, returned as
    /// separate real and imaginary parts. (LAPACK: xGEEV).
    function eig(a:m(s)) : {realParts:Array(s), imagParts:Array(s), vectors:m(s)};
    /// General eigenvalues only (no eigenvectors).
    function eigval(a:m(s)) : {realParts:Array(s), imagParts:Array(s)};
    /// Generalized eigenvalue problem: A*v = lambda*B*v. (LAPACK: xSYGV).
    function eigGeneralized(a:m(s), b:m(s)) : {values:Array(s), vectors:m(s)};

    // Defaults
    function eigvalSymmetric(a:m(s)) : Array(s) =
        let result = eigSymmetric(a) in result.values;
    function eigval(a:m(s)) : {realParts:Array(s), imagParts:Array(s)} =
        let result = eig(a) in {realParts = result.realParts, imagParts = result.imagParts}
};
