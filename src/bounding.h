/**
 * @file bounding.h
 * @brief Ellipsoidal bounding algorithms for nested sampling.
 *
 * This module provides functions for computing minimum volume ellipsoids
 * that bound point sets, primarily used in nested sampling algorithms.
 */

#pragma once

#include <deque>
#include <iostream>
#include <limits>
#include <numeric>
#include <vector>

#include "KMeansRexCore.h"
#include "Rmath.h"
#include "utils.h"

namespace bounding {

constexpr double kPrecision = Eigen::NumTraits<double>::dummy_precision();
inline const char* kMethod = "plusplus";

/**
 * @brief Status codes for ellipsoid computation operations.
 */
enum Status {
  kOk = 0,               // Operation completed successfully
  kNonInvertible = 1,    // Covariance matrix is non-invertible
  kUnderdetermined = 2,  // Insufficient points for full rank covariance
  kFatal = 3             // Fatal error preventing computation
};

class Ellipsoid {
 public:
  Ellipsoid(int n_dim)
      : _n_dim(n_dim),
        _loc(RowVector(n_dim)),
        _A(Matrix(n_dim, n_dim)),
        _scaled_inv_sqrt_A(Matrix(n_dim, n_dim)),
        _e_values(n_dim),
        _e_vectors(Matrix(n_dim, n_dim)),
        _work(n_dim) {};
  void Fit(const ConstRef<Matrix> X);

  // Getters for private members
  const RowVector& loc() const { return _loc; }
  const Matrix& A() const { return _A; }
  const Matrix& scaled_inv_sqrt_A() const { return _scaled_inv_sqrt_A; }
  double scale() const { return _scale; }
  double log_volume() const {
    if (_code == kFatal) {
      return R_NegInf;
    }
    return _log_volume;
  };
  Status code() const { return _code; }

 private:
  int _n_dim;
  RowVector _loc;
  Matrix _A;
  Matrix _scaled_inv_sqrt_A;
  double _scale = 0;
  double _log_volume = R_NegInf;
  Vector _e_values;
  Matrix _e_vectors;
  Eigen::SelfAdjointEigenSolver<Matrix> _work;
  Status _code = kOk;

  inline void CalcLogVolume() {
    _log_volume = log(_e_values.prod());
    _log_volume = 0.5 * (_log_volume + _n_dim * log(_scale));
    _log_volume += (_n_dim / 2.0) * log(M_PI) - lgamma(_n_dim / 2.0 + 1.0);
  }
  void ScaleFactor(const ConstRef<Matrix> X);
  void FindAxes(const ConstRef<Matrix> X);
};

using EllipsoidWithIdx = std::pair<Ellipsoid, std::vector<int>>;

void FitMultiEllipsoids(const ConstRef<Matrix> X,
                        std::vector<Ellipsoid>& ellipsoids,
                        const double log_volume_reduction);
}  // namespace bounding