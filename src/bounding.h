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

/**
 * @brief Status codes for ellipsoid computation operations.
 */
enum Status {
  kOk = 0,             // Operation completed successfully
  kNonInvertible = 1,  // Covariance matrix is non-invertible
  kSingular = 2,       // Insufficient points for full rank covariance
  kFatal = 3           // Fatal error preventing computation
};

/// Reference to mutable matrix
using RefMatrix = Eigen::Ref<Eigen::MatrixXd>;
/// Reference to immutable matrix
using RefConstMatrix = Eigen::Ref<const Eigen::MatrixXd>;
/// Reference to immutable vector
using RefConstVector = Eigen::Ref<const Eigen::VectorXd>;
/// Reference to mutable row vector
using RefRowVector = Eigen::Ref<Eigen::RowVectorXd>;

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
inline double LogVolume(RefConstVector e_values, const double scale) {
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
inline double ScaleFactor(RefConstMatrix X,
                          Eigen::Ref<const Eigen::RowVectorXd> loc,
                          RefConstMatrix precision) {
  double d2 = R_NegInf;
  Eigen::RowVectorXd temp(X.cols());

  for (int i = 0; i < X.rows(); ++i) {
    temp = X.row(i) - loc;
    double d2_row = temp * precision * temp.transpose();
    d2 = Rf_fmax2(d2, d2_row);
  }
  return d2;
}

/**
 * @brief Constructs the transformation matrix for ellipsoid sampling.
 *
 * Creates a matrix that transforms the unit sphere to the ellipsoid
 * by scaling eigenvectors by the square root of their eigenvalues.
 *
 * @param e_values Eigenvalues of the covariance matrix.
 * @param e_vectors Eigenvectors of the covariance matrix.
 * @return Transformation matrix (transposed for sampling efficiency).
 */
inline Eigen::MatrixXd GetTMatrix(RefConstVector e_values,
                                  RefConstMatrix e_vectors) {
  Eigen::MatrixXd t_matrix = e_vectors;
  for (int col = 0; col < t_matrix.cols(); col++) {
    t_matrix.col(col) *= sqrt(e_values[col]);
  }
  return t_matrix.transpose();
}

/**
 * @brief Computes the minimum volume ellipsoid enclosing a set of points.
 *
 * This function implements an algorithm to find the minimum volume ellipsoid
 * that contains all given points. It handles degenerate cases such as
 * insufficient points and singular covariance matrices.
 *
 * @param X Input matrix where each row is a point and each column is a
 * dimension.
 * @param loc Output parameter for the ellipsoid center (mean of points).
 * @param cov Output parameter for the ellipsoid covariance matrix.
 * @param trans Output parameter for the transformation matrix used in sampling.
 * @param scale Output parameter for the scale factor of the enclosing
 * ellipsoid.
 * @param log_volume Output parameter for the natural logarithm of ellipsoid
 * volume.
 * @return Status code indicating success or type of failure encountered.
 */
Status Ellipsoid(RefConstMatrix X, RefRowVector loc, RefMatrix cov,
                 RefMatrix trans, double &scale, double &log_volume);

}  // namespace bounding