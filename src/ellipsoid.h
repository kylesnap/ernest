// File: /Users/ksnap/Projects/ernest/src/ellipsoid.h
// Created Date: Monday, October 20th 2025
// Author: Kyle Dewsnap
//
// Copyright (c) 2025 Kyle Dewsnap
// GNU General Public License v3.0 or later
// https://www.gnu.org/licenses/gpl-3.0-standalone.html
//
// Implements ellipsoid fitting and manipulation routines for nested sampling.
#pragma once

#include "KMeansRexCore.h"
#include "Rmath.h"
#include "utils.h"

namespace ern {
namespace vol {

// Numerical precision threshold for eigenvalue comparisons.
const double kLnPi = 1.14472988584940017414342735135;  // ln(pi)

// Status codes for ellipsoid computation operations.
enum Status {
  kOk = 0,               // Operation completed successfully.
  kNilpotent = 1,        // Input matrix cannot be inverted.
  kDegenerate = 2,       // Eigenvalue of the shape matrix is zero.
  kUnderdetermined = 3,  // Number of fitted points < n_dim + 1.
  kFatal = 99            // Nonspecific numerical error
};

// Status codes for FORTRAN subroutines.
enum Info {
  RelativeAccuracy = 1,  // Reached `rtol`
  AbsoluteAccuracy = 2,  // Reached `atol`
  RoundingErrors = 3,    // Rounding errors prevent further progress
  MaxIterations = 4,     // Failure to converge after itmax iterations
  DimensionalError = -1  // Input matrices were of incorrect shape.
};

// Define an eigendecomposition of a matrix
struct Eigendecomposition {
  Vector values;
  Matrix vectors;
};

extern "C" {
void dgqt_(int* n, double* a, int* lda, double* b, double* delta, double* rtol,
           double* atol, int* itmax, double* par, double* f, double* x, int* info,
           int* iter, double* z, double* wa1, double* wa2);
};

// Workspace for a solver for the constrained minimization problem
// f(x) = (1/2)*x'*A*x + b'*x performed by dgqt
class Solver {
 public:
  // Constructor
  inline Solver(int n) : n_(n), z_(n), wa1_(n), wa2_(n) {};

  // Minimize (1/2) x'Ax + b'x, subject to ||x|| <= delta.
  inline int Solve(Ref<Matrix> A, Ref<Vector> b, Ref<Vector> x, double delta) {
    if (A.rows() != n_ || A.cols() != n_ || b.size() != n_ || x.size() != n_) {
      info_ = DimensionalError;
      return info_;
    }
    int info_int = static_cast<int>(info_);
    dgqt_(&n_, A.data(), &n_, b.data(), &delta, &rtol_, &atol_, &it_max_, &par_, &f_,
          x.data(), &info_int, &iter_, z_.data(), wa1_.data(), wa2_.data());
    info_ = static_cast<Info>(info_int);
    return info_;
  }

  // Getters
  inline double f() const { return f_; };
  inline double iter() const { return iter_; };
  inline int info() const { return info_; };

 private:
  int n_;                              // Rank of A, size of b and x.
  double rtol_ = 1e-8;                 // Relative accuracy
  double atol_ = 1e-8;                 // Absolute accuracy
  double par_ = 0.0;                   // Est. lagrange multiplier of ||x|| <= delta
  double f_ = 0.0;                     // Function value at solution.
  Info info_;                          // Status code.
  int iter_ = 0;                       // Number of iterations taken.
  int it_max_ = 100;                   // Maximum number of iterations.
  std::vector<double> z_, wa1_, wa2_;  // Workspace variables.
};

// A hyper-ellipsoid defined by a center and shape matrix.
class Ellipsoid {
 public:
  // Build an ellipsoid spanning the `n_dim` unit-hypercube.
  explicit Ellipsoid(const int n_dim);
  // Build an ellipsoid that binds the provided data points `X`.
  explicit Ellipsoid(const ConstRef<Matrix> X);
  // Build an ellipsoid with the provided center and shape matrix.
  Ellipsoid(const ConstRef<Vector> center, const ConstRef<Matrix> shape);

  // Bound the ellipsoid to the provided data points `X`.
  void Fit(const ConstRef<Matrix> X);

  // Compute the distance of the ellipsoid's center to
  // its surface, relative to a ray drawn between the center and `point`.
  double Distance(const ConstRef<Vector> point) const;

  // Checks whether this ellipsoid intersects with `other`.
  bool Intersects(const Ellipsoid& other);

  // Find the closest point on the ellipsoid to `point`.
  Vector Closest(const ConstRef<Vector> point);

  // Determine whether `point` is bound by the ellipsoid.
  inline bool Covered(const ConstRef<Vector> point) const {
    double dist = Distance(point);
    if (!R_FINITE(dist)) {
      if (R_IsNaN(dist)) return true;
      cpp11::stop("Numerical issue encountered.");
      return false;
    }
    return dist >= 1.0;
  };

  // Converts the ellipsoid to an R list representation.
  inline cpp11::list as_list() const {
    using namespace cpp11::literals;
    return cpp11::writable::list(
        {"center"_nm = as_row_doubles(center_),
         "shape"_nm = as_doubles_matrix(L_ * L_.transpose()),
         "inv_sqrt_shape"_nm = as_doubles_matrix(inv_sqrt_shape_),
         "log_vol"_nm = log_volume_, "error"_nm = static_cast<int>(error_)});
  }

  // Getters for private variables
  inline RowVector center() const { return center_.transpose(); }
  inline Matrix shape() const { return L_ * L_.transpose(); }
  inline const Matrix& inv_sqrt_shape() const { return inv_sqrt_shape_; }
  inline const Matrix& matrixL() const { return L_; };
  inline int n_dim() const { return n_dim_; }
  inline Status error() const { return error_; }
  inline double log_volume() const { return log_volume_; }
  Matrix major_axes() const;

  // Fit multiple ellipsoids to the dataset `X` using recursive splitting,
  // retaining only those that meet the `min_reduction` criterion.
  // If `allow_contact` is false, ellipsoids that touch are also avoided.
  static std::vector<Ellipsoid> FitMultiEllipsoids(const ConstRef<Matrix> X,
                                                   const double min_reduction,
                                                   const bool allow_contact);

 private:
  Vector center_;                   // Center of the ellipsoid (changed from RowVector).
  Matrix L_;                        // Lower triangular of shape matrix.
  Matrix inv_sqrt_shape_;           // Inverse square root of shape matrix.
  int n_dim_;                       // Number of dimensions.
  Status error_ = kOk;              // Status of most recent operation.
  double log_volume_ = R_NegInf;    // Log volume of the ellipsoid.
  Solver solve_;                    // A constrained minimization solver.
  Eigendecomposition eigensystem_;  // Eigendecomposition of the shape matrix L_'L_

  // Computes the eigendecomposition of the covariance matrix of `X`.
  void FindAxes(const ConstRef<Matrix> X);

  // Scales the axes defined by `eig` to fit the data points `X`.
  void ScaleAxes(ConstRef<Matrix> X);

  // Sets the shape matrix of the ellipsoid based on the eigendecomposition
  // `eig`.
  void SetShape();

  // Compute the constant term in the log volume formula.
  const inline double log_volume_sphere() {
    return (0.5 * n_dim_ * kLnPi) - lgamma1p(n_dim_ / 2.0);
  }
};

// Define an ellipsoid and the observations in a dataset it binds.
struct SubEllipsoid {
  Ellipsoid ell;
  std::vector<int> rows;
};

}  // namespace vol
}  // namespace ern