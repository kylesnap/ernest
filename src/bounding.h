/**
 * @file Bounding.h
 * @brief Ellipsoidal bounding algorithms for nested sampling.
 *
 * This module provides functions for computing minimum volume ellipsoids
 * that bound point sets, primarily used in nested sampling algorithms.
 */

#pragma once

#include <iostream>
#include <limits>

#include "Rmath.h"
#include "utils.h"

namespace bounding {

constexpr double kPrecision = Eigen::NumTraits<double>::dummy_precision();

/**
 * @brief Status codes for ellipsoid computation operations.
 */
enum Status {
  kOk = 0,               // Operation completed successfully
  kNonInvertible = 1,    // Covariance matrix is non-invertible
  kUnderdetermined = 2,  // Insufficient points for full rank covariance
  kFatal = 3             // Fatal error preventing computation
};

/**
 * @brief Computes the log-volume of an n-dimensional ellipsoid.
 *
 * Calculates the logarithm of the volume using the formula:
 * V = sqrt(prod(eigenvalues) * scale^n) * pi^(n/2) / Gamma(n/2 + 1)
 *
 * @param e_values Eigenvalues of the ellipsoid's covariance matrix.
 * @param scale Scale factor from the enclosing ellipsoid computation.
 * @return The natural logarithm of the ellipsoid volume.
 */
inline double LogVolume(const ConstRef<Vector> e_values, const double scale) {
  int n_dim = e_values.size();
  double log_vol = log(e_values.prod());
  log_vol = 0.5 * (log_vol + n_dim * log(scale));
  log_vol += (n_dim / 2.0) * log(M_PI) - lgamma(n_dim / 2.0 + 1.0);
  return log_vol;
}

/**
 * @brief Computes the scale factor for the minimum enclosing ellipsoid.
 *
 * Finds the maximum Mahalanobis distance from the center to any point
 * in the dataset, which determines the scale of the enclosing ellipsoid.
 *
 * @param X Matrix of points (rows are points, columns are dimensions).
 * @param loc Center location of the ellipsoid.
 * @param precision Precision matrix (inverse covariance) of the ellipsoid.
 * @return The scale factor (maximum squared Mahalanobis distance).
 */
inline double ScaleFactor(const ConstRef<Matrix> X,
                          const ConstRef<RowVector> loc,
                          const ConstRef<Matrix> precision) {
  double d2 = R_NegInf;
  Eigen::RowVectorXd temp(X.cols());

  for (int i = 0; i < X.rows(); ++i) {
    temp = X.row(i) - loc;
    double d2_row = temp * precision * temp.transpose();
    d2 = Rf_fmax2(d2, d2_row);
  }
  return d2;
}

Status GetEigendecomposition(const ConstRef<Matrix> X,
                             const ConstRef<RowVector> loc,
                             Ref<Vector> e_values, Ref<Matrix> e_vectors);

Status Ellipsoid(ConstRef<Matrix> X, Ref<RowVector> loc, Ref<Matrix> A,
                 Ref<Matrix> scaledInvSqrtA, double &scale, double &log_volume);
}  // namespace bounding