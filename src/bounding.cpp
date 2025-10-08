/**
 * @file Bounding.cpp
 * @brief Implementation of ellipsoidal bounding algorithms.
 *
 * This file contains the implementation of functions for computing minimum
 * volume ellipsoids that bound point sets, with special handling for
 * degenerate cases and numerical stability issues.
 */

#include "bounding.h"

using namespace bounding;

Status bounding::Ellipsoid(RefConstMatrix X, RefRowVector loc, RefMatrix cov,
                           RefMatrix trans, double& scale, double& log_volume) {
  enum Status result = kOk;

  int n_dim = X.cols();
  int n_point = X.rows();
  loc = X.colwise().mean();

  if (n_point <= 1) {
    log_volume = R_NegInf;
    return Status::kFatal;
  }

  Eigen::MatrixXd centered = X.rowwise() - loc;
  cov = (centered.adjoint() * centered) / double(n_point - 1);

  Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> eigensystem(cov);
  Eigen::VectorXd e_values = eigensystem.eigenvalues();
  Eigen::MatrixXd e_vectors = eigensystem.eigenvectors();

  // Set zero-eigenvalues to a constant.
  for (int i = 0; i < (n_dim - 1); i++) {
    if (e_values[i] <= 0.0) {
      result = kNonInvertible;
      e_values.head(i + 1).setConstant(e_values[i + 1] / 2.0);
    }
  }

  // Unconstrained dimensions set to the min constrained eigenvalue
  if (n_point < n_dim + 1) {
    result = kSingular;
    int unconstrained = n_dim + 1 - n_point;
    e_values.head(unconstrained).setConstant(e_values[unconstrained]);
  }

  Eigen::MatrixXd inv_cov =
      e_vectors * e_values.cwiseInverse().asDiagonal() * e_vectors.transpose();
  cov = e_vectors * e_values.asDiagonal() * e_vectors.transpose();

  scale = ScaleFactor(X, loc, inv_cov);
  trans = GetTMatrix(e_values, e_vectors);
  log_volume = LogVolume(e_values, scale);
  return result;
}