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

#include <deque>
#include <vector>

#include "KMeansRexCore.h"
#include "Rmath.h"
#include "utils.h"

namespace vol {

// Imported typedefs
using ern::Matrix, ern::Vector, ern::RowVector, ern::Ref, ern::ConstRef;

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

// A hyper-ellipsoid defined by a center and shape matrix.
class Ellipsoid {
 public:
  // Build an ellipsoid with the provided center and shape matrix.
  Ellipsoid(const ConstRef<Vector> center, const ConstRef<Matrix> shape);

  // Build a unit sphere in `n_dim` dimensions with radius sqrt(n_dim)/2.
  explicit Ellipsoid(const int n_dim) {
    center_ = Vector::Constant(n_dim, 0.5);
    shape_ = Matrix::Identity(n_dim, n_dim) * (4.0 / n_dim);
    *this = Ellipsoid(center_, shape_);
  };

  // Bound the ellipsoid to the provided data points `X`.
  void Fit(const ConstRef<Matrix> X, double point_log_volume = 1);

  // Compute the distance of the ellipsoid's center to
  // its surface, relative to a ray drawn between the center and `point`.
  double Distance(const ConstRef<Vector> point) const;

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
        {"center"_nm = as_doubles(center_),
         "shape"_nm = as_doubles_matrix(shape_ * shape_.transpose()),
         "inv_sqrt_shape"_nm = as_doubles_matrix(inv_sqrt_shape_),
         "log_vol"_nm = log_volume_, "error"_nm = static_cast<int>(error_)});
  }

  // Getters for private variables
  inline RowVector center() const { return center_.transpose(); }
  inline Matrix shape() const { return shape_; }
  inline const Matrix& inv_sqrt_shape() const { return inv_sqrt_shape_; }
  inline int n_dim() const { return n_dim_; }
  inline Status error() const { return error_; }
  inline double log_volume() const { return log_volume_; }
  Matrix major_axes() const;

 private:
  Vector center_;          // Center of the ellipsoid.
  Matrix shape_;           // Shape matrix (i.e., the precision of bound points)
  Vector axial_lengths_;   // Lengths of the ellipsoid's axes.
  Matrix inv_sqrt_shape_;  // Inverse square root of shape matrix.

  int n_dim_;                     // Number of dimensions.
  Status error_ = kOk;            // Status of most recent operation.
  double log_volume_ = R_NegInf;  // Log volume of the ellipsoid.

  // Mutates `cov` into a precision matrix with eigenvalues adjusted to meet `log_target`.
  void FindAxes(Ref<Matrix> cov, const double log_target);

  // Scales the axes of the ellipsoid to ensure it fits the centered data `diff`.
  void ScaleAxes(ConstRef<Matrix> X);

  // Mutate the matrix held in shape_L_ to its proper components.
  void SetShape();

  // Compute the constant term in the log volume formula.
  const inline double log_volume_sphere() {
    return (0.5 * n_dim_ * kLnPi) - lgamma1p(n_dim_ / 2.0);
  }
};

// // Structure binding an ellipsoid to fitted data.
// struct EllipsoidAndData {
//   Ellipsoid ell;
//   Matrix data;
// };

// // Fit multiple ellipsoids to the dataset `X` using recursive splitting,
// // retaining only those that meet the `min_reduction` criterion.
// // If `allow_contact` is false, ellipsoids that touch are also avoided.
// std::vector<Ellipsoid> FitMultiEllipsoids(const ConstRef<Matrix> X,
//                                           const double min_reduction,
//                                           const bool allow_contact,
//                                           const double expected_volume);

}  // namespace vol