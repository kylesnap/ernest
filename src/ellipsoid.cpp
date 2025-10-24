// File: /Users/ksnap/Projects/ernest/src/ellipsoid.cpp
// Created Date: Monday, October 20th 2025
// Author: Kyle Dewsnap
//
// Copyright (c) 2025 Kyle Dewsnap
// GNU General Public License v3.0 or later
// https://www.gnu.org/licenses/gpl-3.0-standalone.html
//
// Implements ellipsoid fitting and manipulation routines for nested sampling.
#include "ellipsoid.h"

#include <R_ext/BLAS.h>
#include <R_ext/Lapack.h>

#include <functional>
#include <numeric>

using namespace ern;

//// Ellipsoid Constructors
vol::Ellipsoid::Ellipsoid(const int n_dim) : n_dim_(n_dim), solve_(n_dim_) {
  if (n_dim == 0) {
    error_ = kFatal;
    return;
  }
  // Circumradius of unit hypercube is sqrt(n_dim)/2.
  center_ = Vector::Constant(n_dim_, 0.5);
  L_ = Matrix::Identity(n_dim_, n_dim_) * (2 / sqrt(n_dim_));
  inv_sqrt_shape_ = Matrix::Identity(n_dim_, n_dim_) * (sqrt(n_dim_) / 2.0);
  double log_det_L = n_dim_ * log(2.0) - 0.5 * n_dim_ * log(n_dim_);
  log_volume_ = log_volume_sphere() - log_det_L;
}

vol::Ellipsoid::Ellipsoid(const ConstRef<Matrix> X) : Ellipsoid(X.cols()) {
  (*this).Fit(X);
};

vol::Ellipsoid::Ellipsoid(const ConstRef<Vector> center,
                          const ConstRef<Matrix> shape)
    : Ellipsoid(center.size()) {
  if (!((shape.rows() == shape.cols()) && (shape.rows() == n_dim_))) {
    error_ = kFatal;
    return;
  }
  center_ = center;
  Eigen::SelfAdjointEigenSolver<Matrix> work(shape);
  if (work.info() != Eigen::Success) {
    error_ = kFatal;
    return;
  }
  Eigendecomposition eig{work.eigenvalues(), work.eigenvectors()};
  (*this).SetShape(eig);
}

//// Ellipsoid Tools

// Find the closest point on the ellipsoid defined by (L, center) to `point`.
// Translation of the method "ell_pt_near_far" described in ELL_LIB.
static Vector FindClosest(const ConstRef<Matrix> L,
                          const ConstRef<Vector> center,
                          const ConstRef<Vector> point, vol::Solver& solver) {
  Matrix gi = L.inverse();  // g_i = L^-1
  Vector b = center - point;
  b = gi * b;  // b = G^{-1} * [c-p]
  Vector y(L.rows());

  Matrix a = gi * gi.transpose();  // a = G^(-1) * G^(-T)
  double delta = 1.0;

  int info = solver.Solve(a, b, y, delta);
  if (info != 1 && info != 2) {
    cpp11::warning("dgqt incomplete convergence (%i)", info);
  }
  return gi.transpose() * y + center;  // x = G^{-T} * y + c
}

double vol::Ellipsoid::Distance(const ConstRef<Vector> point) const {
  Vector y = point - center_;
  y = L_.transpose() * y;
  double y_sum = y.squaredNorm();
  // y_sum Indicates whether the point is inside, on, or outside the ellipsoid
  if (y_sum > 0) {
    return 1 / sqrt(y_sum);
  } else if (point.isApprox(center_)) {
    // y_sum is zero only if point == center_
    return NAN;
  } else {
    return R_NegInf;
  }
}

Vector vol::Ellipsoid::Closest(const ConstRef<Vector> point) {
  return FindClosest(L_, center_, point, solve_);
};

bool vol::Ellipsoid::Intersects(const Ellipsoid& other) {
  // An abbreviated Part 1 from ell_pair_separate in ELL_LIB
  Matrix g1 = L_;
  Matrix g2 = other.L_;

  // Transform to y-space:  y = G1^T * (x-c1);  G2y = G1^{-1} * G2
  Matrix g2y = g1.inverse() * g2;
  Vector c2y = other.center_ - center_;
  c2y = g1.transpose() * c2y;

  // Find point in ell(c2y, g2y) closest to origin.
  Vector v = Vector::Zero(n_dim_);
  Vector y2 = FindClosest(g2y, c2y, v, solve_);

  // If y2 is less than unity, the ellipsoids intersect.
  return y2.squaredNorm() <= 1.0;
}

// Ellipsoid Fitting

void vol::Ellipsoid::Fit(ConstRef<Matrix> X) {
  log_volume_ = R_NegInf;
  if (X.cols() != n_dim_ || X.rows() <= 1) {
    error_ = kFatal;
    return;
  }

  center_ = X.colwise().mean().transpose();
  Eigendecomposition eig = FindAxes(X);
  if (eig.values.size() == 0) {
    Status error = error_;
    *this = Ellipsoid(n_dim_);
    error_ = error;
    return;
  }
  ScaleAxes(X, eig);
  SetShape(eig);
}

vol::Eigendecomposition vol::Ellipsoid::FindAxes(const ConstRef<Matrix> X) {
  // Store the covariance matrix in L_.
  error_ = kOk;
  Matrix centered = X.rowwise() - center_.transpose();
  L_ = (centered.adjoint() * centered) / double(X.rows() - 1);

  // Perform eigendecomposition of the covariance matrix.
  Eigen::SelfAdjointEigenSolver<Matrix> work(L_);
  if (work.info() != Eigen::Success) {
    error_ = kFatal;
    return Eigendecomposition();
  }

  // Condition the eigenvalues to avoid numerical issues.
  // If possible, set small eigenvalues to half the smallest nonzero eigenvalue.
  Vector e_values = work.eigenvalues();
  int zero_ev = (e_values.array() <= kPrecision).count();
  if (zero_ev == e_values.size()) {
    error_ = kNilpotent;
    return Eigendecomposition();
  } else if (zero_ev != 0) {
    error_ = kDegenerate;
    e_values.head(zero_ev).setConstant(e_values[zero_ev] / 2.0);
  }

  // Handle underdetermined case.
  // Set underdetermined eigenvalues to the smallest determined eigenvalue.
  if (X.rows() < n_dim_ + 1) {
    error_ = kUnderdetermined;
    int unconstrained = n_dim_ + 1 - X.rows();
    e_values.head(unconstrained).setConstant(e_values[unconstrained]);
  }

  // Return the eigendecomposition of the corrected PRECISION matrix
  return {e_values.cwiseInverse(), work.eigenvectors()};
}

void vol::Ellipsoid::ScaleAxes(ConstRef<Matrix> X, Eigendecomposition& eig) {
  // L_ now stores the full shape matrix.
  L_ = eig.vectors * eig.values.asDiagonal() * eig.vectors.transpose();

  // Scale eigenvalues by the maximum squared distance from center to the data.
  Matrix diff = X.rowwise() - center_.transpose();
  double max_dist2 = R_NegInf;
  for (auto row : diff.rowwise()) {
    double dist2 = row * L_ * row.transpose();
    max_dist2 = fmax2(dist2, max_dist2);
  }
  eig.values /= max_dist2;
}

void vol::Ellipsoid::SetShape(Eigendecomposition& eig) {
  // L_ = V * D^{1/2};
  L_ = eig.vectors * eig.values.cwiseSqrt().asDiagonal();
  Eigen::HouseholderQR<Matrix> qr(L_.transpose());
  // A = LQ, such that A^T = QR, so L = R^T
  L_ = qr.matrixQR().triangularView<Eigen::Upper>().transpose();
  log_volume_ = log_volume_sphere() + -qr.logAbsDeterminant();

  // inv_sqrt_shape_ = V * D^{-1/2} * V^T
  inv_sqrt_shape_ = eig.vectors *
                    eig.values.cwiseInverse().cwiseSqrt().asDiagonal() *
                    eig.vectors.adjoint();
}

// Clustering Helpers

// Finds the rows in `rows` that belong to `cluster` as indicated by `sub_idx`.
static std::vector<int> find_clusters(const std::vector<int>& rows,
                                      const ConstRef<Vector> sub_idx,
                                      const int cluster) {
  std::vector<int> cluster_rows;
  for (int i = 0; i < rows.size(); i++) {
    if (sub_idx[i] == cluster) {
      cluster_rows.push_back(rows[i]);
    }
  }
  return cluster_rows;
}

// Splits the data points in `rows` into two sub-ellipsoids using K-means.
// Returns true if both sub-ellipsoids were non-degenerate.
static bool split_cluster(const ConstRef<Matrix> X,
                          const std::vector<int>& rows, vol::SubEllipsoid& lh,
                          vol::SubEllipsoid& rh) {
  Matrix sub_X = X(rows, Eigen::all);
  Eigen::ArrayXd sub_idx(sub_X.rows());
  Eigen::ArrayXXd mu(2, sub_X.cols());
  kmeans_rex::RunKMeans(sub_X, 2, 100, mu, sub_idx);

  lh.rows = find_clusters(rows, sub_idx, 0);
  lh.ell.Fit(X(lh.rows, Eigen::all));
  rh.rows = find_clusters(rows, sub_idx, 1);
  rh.ell.Fit(X(rh.rows, Eigen::all));

  return lh.ell.error() == vol::kOk && rh.ell.error() == vol::kOk;
}

// Moves the sub-ellipsoids onto the proposal queue and pops the pre-split
// ellipsoid.
static void split(std::deque<vol::SubEllipsoid>& proposals,
                  vol::SubEllipsoid& lh, vol::SubEllipsoid& rh) {
  int n_dim = lh.ell.n_dim();
  proposals.emplace_back(std::move(lh));
  proposals.emplace_back(std::move(rh));
  lh = vol::SubEllipsoid({vol::Ellipsoid(n_dim), std::vector<int>()});
  rh = vol::SubEllipsoid({vol::Ellipsoid(n_dim), std::vector<int>()});
  proposals.pop_front();
}

// Move the pre-split ellipsoid onto the vector of finalized ellipsoids.
static void merge(std::vector<vol::Ellipsoid>& ellipsoids,
                  std::deque<vol::SubEllipsoid>& proposals) {
  ellipsoids.emplace_back(std::move(proposals.front().ell));
  proposals.pop_front();
}

std::vector<vol::Ellipsoid> vol::Ellipsoid::FitMultiEllipsoids(
    const ConstRef<Matrix> X, const double min_reduction,
    const bool allow_contact) {
  int n_dim = X.cols();
  int n_point = X.rows();
  std::vector<Ellipsoid> ellipsoids;
  std::deque<vol::SubEllipsoid> proposals;

  vol::SubEllipsoid lh{Ellipsoid(n_dim), std::vector<int>()};
  vol::SubEllipsoid rh{Ellipsoid(n_dim), std::vector<int>()};
  Ellipsoid intersect(n_dim);

  proposals.emplace_back(
      vol::SubEllipsoid{Ellipsoid(n_dim), std::vector<int>(n_point)});
  proposals.front().ell.Fit(X);
  if (proposals.front().ell.error() != kOk) {
    // If the initial ellipsoid is invalid, return it as the only ellipsoid.
    merge(ellipsoids, proposals);
    return ellipsoids;
  }
  std::iota(proposals.front().rows.begin(), proposals.front().rows.end(), 0);

  while (!proposals.empty()) {
    Ellipsoid& cur = proposals.front().ell;
    std::vector<int>& rows = proposals.front().rows;
    if (cur.error() != kOk) {
      proposals.pop_front();
      continue;
    }

    bool splitable = split_cluster(X, rows, lh, rh);
    if (!splitable) {
      merge(ellipsoids, proposals);
      continue;
    }

    // Check whether merging the two sub-ellipsoids reduces the volume.
    double cur_log_vol = cur.log_volume() + log(min_reduction);
    double tot_log_vol =
        Rf_logspace_add(lh.ell.log_volume(), rh.ell.log_volume());
    bool less_volume = R_FINITE(tot_log_vol) && tot_log_vol <= cur_log_vol;
    if (!less_volume) {
      merge(ellipsoids, proposals);
      continue;
    }

    // Check whether the two sub-ellipsoids intersect (if required).
    if (!allow_contact && lh.ell.Intersects(rh.ell)) {
      merge(ellipsoids, proposals);
    } else {
      split(proposals, lh, rh);
    }
  }
  return ellipsoids;
}