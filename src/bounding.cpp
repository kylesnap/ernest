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

/**
 * @brief Find the stable eigendecomposition for the given matrix.
 *
 * @param X Input matrix where each row is a point and each column is a
 * dimension.
 * @param loc Input parameter for the ellipsoid center (mean of points).
 * @param e_values Output vector for the eigenvalues (high-to-low).
 * @param e_vectors Output matrix for the eigenvectors.
 * @return Int code representing the outcome of the decomposition.
 */
Status bounding::GetEigendecomposition(const ConstRef<Matrix> X,
                                       const ConstRef<RowVector> loc,
                                       Ref<Vector> e_values,
                                       Ref<Matrix> e_vectors) {
  Status code = kOk;
  Matrix centered = X.rowwise() - loc;
  Matrix cov = (centered.adjoint() * centered) / double(X.rows() - 1);
  Eigen::SelfAdjointEigenSolver<Matrix> eigensystem(cov);
  if (eigensystem.info() == Eigen::NoConvergence) {
    return Status::kFatal;
  }

  e_values = eigensystem.eigenvalues();
  e_vectors = eigensystem.eigenvectors();
  std::cout << "[Ellipsoid] Eigenvalues:\n" << e_values << std::endl;
  std::cout << "[Ellipsoid] Eigenvectors:\n" << e_vectors << std::endl;

  // Correct zero-valued eigenvalues.
  int zero_ev = (e_values.array() <= kPrecision).count();
  if (zero_ev == e_values.size()) {
    return kFatal;
  } else if (zero_ev != 0) {
    std::cout << "[Ellipsoid] ZERO COUNT:\n" << zero_ev << std::endl;
    code = kNonInvertible;
    e_values.head(zero_ev).setConstant(e_values[zero_ev] / 2.0);
    std::cout << "[Ellipsoid] Repaired zero-valued eigenvalues:\n"
              << e_values << std::endl;
  }

  // Address underdetermined systems
  if (X.rows() < X.cols() + 1) {
    code = kUnderdetermined;
    int unconstrained = X.cols() + 1 - X.rows();
    e_values.head(unconstrained).setConstant(e_values[unconstrained]);
    std::cout << "[Ellipsoid] Modified eigenvalues for unconstrained dims:\n"
              << e_values << std::endl;
  }
  return code;
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
 * @param A Output parameter describing the ellipsoid's axes.
 * @param scaledInvSqrtA Output parameter for the sphere transformation matrix.
 * @param scale Output parameter for the scale factor of the enclosing
 * ellipsoid.
 * @param log_volume Output parameter for the natural logarithm of ellipsoid
 * volume.
 * @return Status code indicating success or type of failure encountered.
 */
Status bounding::Ellipsoid(ConstRef<Matrix> X, Ref<RowVector> loc,
                           Ref<Matrix> A, Ref<Matrix> scaledInvSqrtA,
                           double& scale, double& log_volume) {
  std::cout << "[Ellipsoid] Starting computation..." << std::endl;

  int n_dim = X.cols();
  int n_point = X.rows();
  std::cout << "[Ellipsoid] n_dim: " << n_dim << ", n_point: " << n_point
            << std::endl;

  loc = X.colwise().mean();
  std::cout << "[Ellipsoid] Computed mean (loc): " << loc << std::endl;

  if (n_point <= 1) {
    std::cout << "[Ellipsoid] Not enough points. Returning kFatal."
              << std::endl;
    log_volume = R_NegInf;
    return Status::kFatal;
  }

  Vector e_values = Vector(n_dim);
  Matrix e_vectors = Matrix(n_dim, n_dim);
  Status code = GetEigendecomposition(X, loc, e_values, e_vectors);
  A = e_vectors * e_values.cwiseInverse().asDiagonal() * e_vectors.transpose();

  scale = ScaleFactor(X, loc, A);
  std::cout << "[Ellipsoid] Scale factor: " << scale << std::endl;

  scaledInvSqrtA = A;
  Eigen::SelfAdjointEigenSolver<Matrix> decomp(scaledInvSqrtA);
  scaledInvSqrtA = decomp.operatorInverseSqrt();
  scaledInvSqrtA *= sqrt(scale);
  std::cout << "[Ellipsoid] Transformation matrix:\n"
            << scaledInvSqrtA << std::endl;

  log_volume = LogVolume(e_values, scale);
  std::cout << "[Ellipsoid] Log volume: " << log_volume << std::endl;

  std::cout << "[Ellipsoid] Returning status: " << code << std::endl;
  return code;
}