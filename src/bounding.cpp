/**
 * @file bounding.cpp
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
void Ellipsoid::FindAxes(const ConstRef<Matrix> X) {
  _code = kOk;
  Matrix centered = X.rowwise() - _loc;
  _A = (centered.adjoint() * centered) / double(X.rows() - 1);

  _work.compute(_A);
  if (_work.info() == Eigen::NoConvergence) {
    _code = kFatal;
    return;
  }

  _e_values = _work.eigenvalues();
  _e_vectors = _work.eigenvectors();

  // Correct zero-valued eigenvalues.
  int zero_ev = (_e_values.array() <= kPrecision).count();
  if (zero_ev == _e_values.size()) {
    _code = kFatal;
  } else if (zero_ev != 0) {
    _code = kNonInvertible;
    _e_values.head(zero_ev).setConstant(_e_values[zero_ev] / 2.0);
  }

  // Address underdetermined systems
  if (X.rows() < _n_dim + 1) {
    _code = kUnderdetermined;
    int unconstrained = _n_dim + 1 - X.rows();
    _e_values.head(unconstrained).setConstant(_e_values[unconstrained]);
  }
}

/**
 * @brief Computes the scale factor for the minimum enclosing ellipsoid.
 *
 * Finds the maximum Mahalanobis distance from the center to any point
 * in the dataset, which determines the scale of the enclosing ellipsoid.
 *
 * @param X Matrix of points (rows are points, columns are dimensions).
 * @return The scale factor (maximum squared Mahalanobis distance).
 */
void Ellipsoid::ScaleFactor(const ConstRef<Matrix> X) {
  _scale = R_NegInf;
  Eigen::RowVectorXd temp(X.cols());
  for (int i = 0; i < X.rows(); ++i) {
    temp = X.row(i) - _loc;
    double scale = temp * _A * temp.transpose();
    _scale = Rf_fmax2(_scale, scale);
  }
}

void Ellipsoid::Fit(ConstRef<Matrix> X) {
  int n_dim = X.cols();
  int n_point = X.rows();
  std::cout << "[Ellipsoid] n_dim: " << n_dim << ", n_point: " << n_point
            << std::endl;
  if (n_dim != _n_dim) {
    _code = kFatal;
    return;
  }

  _loc = X.colwise().mean();

  if (n_point <= 1) {
    _code = kFatal;
    return;
  }

  FindAxes(X);
  if (_code == kFatal) {
    return;
  }
  _A = _e_vectors * _e_values.cwiseInverse().asDiagonal() *
       _e_vectors.transpose();

  ScaleFactor(X);

  _work.compute(_A);
  _scaled_inv_sqrt_A = _work.operatorInverseSqrt();
  _scaled_inv_sqrt_A *= sqrt(_scale);

  CalcLogVolume();
  return;
}

std::vector<int> find_clusters(const std::vector<int>& rows,
                               const ConstRef<Vector> cluster_idx,
                               const int cluster) {
  std::vector<int> new_indices;
  for (int i = 0; i < rows.size(); i++) {
    if (cluster_idx[i] == cluster) new_indices.push_back(rows[i]);
  }
  return new_indices;
}

void bounding::FitMultiEllipsoids(const ConstRef<Matrix> X,
                                  std::vector<Ellipsoid>& ellipsoids,
                                  const double log_volume_reduction) {
  int n_dim = X.cols();
  int n_point = X.rows();
  std::deque<EllipsoidWithIdx> proposals;
  Eigen::ArrayXXd cluster_loc(2, n_dim);

  Matrix lh_X(n_point, n_dim), rh_X(n_point, n_dim);

  proposals.emplace_back(Ellipsoid(n_dim), std::vector<int>(n_point));
  proposals.front().first.Fit(X);
  std::iota(proposals.front().second.begin(), proposals.front().second.end(),
            0);

  while (!proposals.empty()) {
    Ellipsoid& cur = proposals.front().first;
    std::vector<int>& rows = proposals.front().second;
    if (cur.code() == kFatal) {
      proposals.pop_front();
      continue;
    }

    std::cout << "[MULTI] Current rows: ";
    for (const auto& idx : rows) {
      std::cout << idx << " ";
    }
    std::cout << std::endl;
    Matrix sub_X = X(rows, Eigen::all);
    Eigen::ArrayXd cluster_idx(sub_X.rows());
    kmeans_rex::RunKMeans(sub_X, 2, 100, kMethod, cluster_loc, cluster_idx);

    EllipsoidWithIdx lh =
        std::make_pair(Ellipsoid(n_dim), find_clusters(rows, cluster_idx, 0));
    lh.first.Fit(X(lh.second, Eigen::all));

    EllipsoidWithIdx rh =
        std::make_pair(Ellipsoid(n_dim), find_clusters(rows, cluster_idx, 1));
    rh.first.Fit(X(rh.second, Eigen::all));

    double old_log_vol = cur.log_volume() + log_volume_reduction;
    double new_log_volume =
        Rf_logspace_add(lh.first.log_volume(), rh.first.log_volume());
    std::cout << "[MULTI] Log Volume " << cur.log_volume() << " -> "
              << new_log_volume << std::endl;
    bool split = false;
    if (R_FINITE(new_log_volume) && new_log_volume <= old_log_vol) {
      split = true;
    }

    if (split) {
      std::cout << "[MULTI] Splitting into two" << std::endl;
      proposals.emplace_back(std::move(lh));
      proposals.emplace_back(std::move(rh));
    } else {
      std::cout << "[MULTI] Accepting new ellipsoid" << std::endl;
      ellipsoids.emplace_back(std::move(cur));
    }
    proposals.pop_front();
  }
}