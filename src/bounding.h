#pragma once

#include "KMeansRexCore.h"
#include "Rmath.h"
#include "utils.h"

namespace bounding {

// Numerical precision threshold for eigenvalue comparisons.
constexpr double kPrecision = Eigen::NumTraits<double>::dummy_precision();

// Status codes for ellipsoid computation operations.
enum Status {
  kOk = 0,               // Operation completed successfully.
  kFatal = 1,            // Numerical error from estimating the shape matrix.
  kDegenerate = 2,       // Eigenvalue of the shape matrix is zero.
  kUnderdetermined = 3,  // Number of fitted points < n_dim + 1.
  kEmpty = -1            // Uninitialized ellipsoid
};

// Represents a hyper-ellipsoid defined by center and shape matrix.
//
// An ellipsoid satisfying the equation:
//   (x - center) * inverse_shape * (x - center)' <= 1
//
// The class provides methods to fit ellipsoids to point sets and handles
// degenerate cases such as collinear points or underdetermined systems.
class Ellipsoid {
 public:
  Ellipsoid(const ConstRef<Vector> center, const ConstRef<Matrix> shape);
  explicit Ellipsoid(const int n_dim);
  explicit Ellipsoid(const ConstRef<Matrix> X);
  void Fit(const ConstRef<Matrix> X);
  bool Overlaps(const Ellipsoid& other, double tau = 1.0) const;

  // Converts the ellipsoid to an R list representation.
  //
  // Returns:
  // An R list containing
  // - log_vol: Log volume of the ellipsoid (or -Inf on error).
  // - error: Status code.
  // - center: Center vector (only if not kFatal).
  // - inverse_shape: Inverse shape matrix (only if not kFatal).
  // - sqrt_shape: Square root of inverse shape (only if not kFatal).
  inline const cpp11::list as_list() {
    using namespace cpp11::literals;
    if (error_ == kFatal) {
      return cpp11::writable::list(
          {"log_vol"_nm = R_NegInf, "error"_nm = static_cast<int>(error_)});
    } else {
      return cpp11::writable::list(
          {"center"_nm = as_row_doubles(center_),
           "inverse_shape"_nm = as_doubles_matrix(inverse_shape_),
           "sqrt_shape"_nm = as_doubles_matrix(sqrt_shape_),
           "log_vol"_nm = log_volume_, "error"_nm = static_cast<int>(error_)});
    }
  };

  // Getters for private variables
  inline const RowVector& center() const { return center_; }
  inline const Matrix& shape() const { return shape_; }
  inline const Matrix& inverse_shape() const { return inverse_shape_; }
  inline const Matrix& sqrt_shape() const { return sqrt_shape_; }
  inline int n_dim() const { return n_dim_; }
  inline Status error() const { return error_; }
  inline double log_volume() const { return log_volume_; }

 private:
  RowVector center_;       // Center of the ellipsoid.
  Matrix shape_;           // Shape matrix (covariance-like).
  Matrix inverse_shape_;   // Inverse of shape matrix (precision).
  Matrix sqrt_shape_;      // Matrix square root of inverse shape.
  int n_dim_;              // Number of dimensions.
  Status error_ = kEmpty;  // Status of most recent operation.
  Eigen::SelfAdjointEigenSolver<Matrix> work_;  // Reusable workspace.
  double log_volume_ = R_NegInf;                // Log volume of the ellipsoid.

  void FindAxes(const ConstRef<Matrix> X);
  void ScaleAxes(const ConstRef<Matrix> X);

  // Compute the constant term in the log volume formula.
  //
  // Returns:
  // log(pi^(n_dim/2) / gamma(n_dim/2 - 1))
  const inline double log_volume_const() {
    return (n_dim_ * M_LN_SQRT_PI) - lgamma1p(n_dim_ / 2.0);
  }
};

// Struct containing an ellipsoid and a integer vector of row indicies.
struct SubEllipsoid {
  Ellipsoid ell;
  std::vector<int> rows;
};

std::vector<Ellipsoid> FitMultiEllipsoids(const ConstRef<Matrix> X,
                                          const double min_reduction,
                                          const double max_overlap);

}  // namespace bounding