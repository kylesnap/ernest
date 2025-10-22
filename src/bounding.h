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
  kNilpotent = 1,        // Input matrix cannot be inverted.
  kDegenerate = 2,       // Eigenvalue of the shape matrix is zero.
  kUnderdetermined = 3,  // Number of fitted points < n_dim + 1.
  kEmpty = -1,           // Uninitialized ellipsoid
  kFatal = 99            // Numerical error
};

// Simple struct for an eigensystem
struct Eigensystem {
  Vector values;
  Matrix vectors;
};

// DPGT wrapping class
class Solver {
 public:
  inline Solver(int n) : n_(n), z_(n), wa1_(n), wa2_(n) {};
  int Solve(Ref<Matrix> A, Ref<Vector> b, Ref<Vector> x, double delta);

  inline double f() const { return f_; };
  inline double iter() const { return iter_; };
  inline int info() const { return info_; };

 private:
  int n_;
  double rtol_ = 1e-8;
  double atol_ = 1e-8;
  double par_ = 0.0, f_ = 0.0;
  int info_ = 0, iter_ = 0, it_max_ = 100;
  std::vector<double> z_, wa1_, wa2_;
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
  double Distance(const ConstRef<Vector> point) const;
  Vector Closest(const ConstRef<Vector> point) const;
  double Distance(const Ellipsoid& other) const;

  // Determines whether a given point is covered by the bounding region.
  //
  // `point`: A constant reference to a Vector representing the point to check.
  //
  // Returns:
  // TRUE if the point is covered by the bounding region,
  // FALSE if the point is not covered.
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
           "shape"_nm = as_doubles_matrix(L_ * L_.transpose()),
           "sqrt_shape"_nm = as_doubles_matrix(inv_sqrt_shape_),
           "log_vol"_nm = log_volume_, "error"_nm = static_cast<int>(error_)});
    }
  };

  // Getters for private variables
  inline RowVector center() const { return center_.transpose(); }
  inline Matrix shape() const { return L_ * L_.transpose(); }
  inline const Matrix& inv_sqrt_shape() const { return inv_sqrt_shape_; }
  inline const Matrix& matrixL() const { return L_; };
  inline int n_dim() const { return n_dim_; }
  inline Status error() const { return error_; }
  inline double log_volume() const { return log_volume_; }

 private:
  Vector center_;          // Center of the ellipsoid (changed from RowVector).
  Matrix L_;               // Lower triangular of shape matrix.
  Matrix inv_sqrt_shape_;  // Inverse square root of shape matrix.
  int n_dim_;              // Number of dimensions.
  Status error_ = kEmpty;  // Status of most recent operation.
  double log_volume_ = R_NegInf;  // Log volume of the ellipsoid.

  Vector eigenvalues;
  Matrix eigenvectors;

  Eigensystem FindAxes(const ConstRef<Matrix> X);
  void ScaleAxes(ConstRef<Matrix> X, Eigensystem& eig);
  void SetShape(Eigensystem& eig);

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
                                          const bool allow_contact);

}  // namespace bounding