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

#include <functional>
#include <numeric>

using namespace vol;

//// Ellipsoid Constructors
Ellipsoid::Ellipsoid(const int n_dim) : n_dim_(n_dim), solve_(n_dim_) {
  if (n_dim == 0) {
    error_ = kFatal;
    return;
  }
  // Circumradius of unit hypercube is sqrt(n_dim)/2.
  center_ = Vector::Constant(n_dim_, 0.5);
  L_ = Matrix::Identity(n_dim_, n_dim_) * (2 / sqrt(n_dim_));
  eigensystem_ = Eigendecomposition{Vector::Ones(n_dim_) * (2.0 / sqrt(n_dim_)),
                                    Matrix::Identity(n_dim_, n_dim_)};
  inv_sqrt_shape_ = Matrix::Identity(n_dim_, n_dim_) * (sqrt(n_dim_) / 2.0);
  double log_det_L = n_dim_ * log(2.0) - 0.5 * n_dim_ * log(n_dim_);
  log_volume_ = log_volume_sphere() - log_det_L;
}

vol::Ellipsoid::Ellipsoid(const ConstRef<Matrix> X) : Ellipsoid(X.cols()) {
  (*this).Fit(X);
};

vol::Ellipsoid::Ellipsoid(const ConstRef<Vector> center, const ConstRef<Matrix> shape)
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
  eigensystem_.values = work.eigenvalues();
  eigensystem_.vectors = work.eigenvectors();
  (*this).SetShape();
}

//// Ellipsoid Tools

// Find the closest point on the ellipsoid defined by (L, center) to `point`.
// Translation of the method "ell_pt_near_far" described in ELL_LIB.
static Vector FindClosest(const ConstRef<Matrix> L, const ConstRef<Vector> center,
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

Matrix Ellipsoid::major_axes() const {
  // Largest axis
  Vector axes = eigensystem_.values.cwiseInverse().cwiseSqrt();
  Eigen::Index max_i;
  axes.maxCoeff(&max_i);
  axes = inv_sqrt_shape_.col(max_i);  // The major axis of the ellipsoid
  Matrix endpoints(2, n_dim_);
  endpoints.row(0) = center_ + axes;
  endpoints.row(1) = center_ - axes;
  return endpoints;
}

// Ellipsoid Fitting

void Ellipsoid::Fit(ConstRef<Matrix> X) {
  log_volume_ = R_NegInf;
  if (X.cols() != n_dim_ || X.rows() <= 1) {
    error_ = kFatal;
    return;
  }

  center_ = X.colwise().mean().transpose();
  FindAxes(X);
  if (error_ == kFatal || error_ == kNilpotent) {
    Status error = error_;
    *this = Ellipsoid(n_dim_);
    error_ = error;
    return;
  }
  ScaleAxes(X);
  SetShape();
}

void Ellipsoid::FindAxes(const ConstRef<Matrix> X) {
  // Store the covariance matrix in L_.
  error_ = kOk;
  Matrix centered = X.rowwise() - center_.transpose();
  L_ = (centered.adjoint() * centered) / double(X.rows() - 1);

  // Perform eigendecomposition of the covariance matrix.
  Eigen::SelfAdjointEigenSolver<Matrix> work(L_);
  if (work.info() != Eigen::Success) {
    error_ = kFatal;
    return;
  }

  // Condition the eigenvalues to avoid numerical issues.
  // If possible, set small eigenvalues to half the smallest nonzero eigenvalue.
  Vector e_values = work.eigenvalues();
  int zero_ev = (e_values.array() <= ern::kPrecision).count();
  if (zero_ev == e_values.size()) {
    error_ = kNilpotent;
    return;
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
  eigensystem_.values = e_values.cwiseInverse();
  eigensystem_.vectors = work.eigenvectors();
}

void vol::Ellipsoid::ScaleAxes(ConstRef<Matrix> X) {
  // L_ now stores the full shape matrix.
  L_ = eigensystem_.vectors * eigensystem_.values.asDiagonal() *
       eigensystem_.vectors.transpose();

  // Scale eigenvalues by the maximum squared distance from center to the data.
  Matrix diff = X.rowwise() - center_.transpose();
  double max_dist2 = R_NegInf;
  for (auto row : diff.rowwise()) {
    double dist2 = row * L_ * row.transpose();
    max_dist2 = fmax2(dist2, max_dist2);
  }
  eigensystem_.values /= max_dist2;
}

void vol::Ellipsoid::SetShape() {
  // L_ = V * D^{1/2};
  L_ = eigensystem_.vectors * eigensystem_.values.cwiseSqrt().asDiagonal();
  Eigen::HouseholderQR<Matrix> qr(L_.transpose());
  // A = LQ, such that A^T = QR, so L = R^T
  L_ = qr.matrixQR().triangularView<Eigen::Upper>().transpose();
  log_volume_ = log_volume_sphere() + -qr.logAbsDeterminant();

  // inv_sqrt_shape_ = V * D^{-1/2} * V^T
  inv_sqrt_shape_ = eigensystem_.vectors *
                    eigensystem_.values.cwiseInverse().cwiseSqrt().asDiagonal() *
                    eigensystem_.vectors.adjoint();
}

// Clustering Helpers
// Helper function for recursive splitting
static std::deque<EllipsoidAndData> SplitEllipsoid(const vol::EllipsoidAndData& cur,
                                                   const double min_reduction,
                                                   const bool allow_contact,
                                                   const double point_volume) {
  // Perform k-means clustering with k=2
  size_t n_dim = cur.ell.n_dim();
  size_t n_points = cur.data.rows();
  Matrix endpoints = cur.ell.major_axes();
  Vector labels(n_points);
  kmeans_rex::RunKMeans(cur.data, 2, 10, endpoints, labels);

  // Split points into two clusters
  std::vector<int> idx0, idx1;
  for (int i = 0; i < n_points; ++i) {
    if (labels[i] == 0) {
      idx0.push_back(i);
    } else {
      idx1.push_back(i);
    }
  }

  // BASE CASE 1: Clusters get too small
  if (idx0.size() < 2 * n_dim || idx1.size() < 2 * n_dim) {
    return std::deque<EllipsoidAndData>();
  }

  // Fit ellipsoids to each cluster
  Matrix lh_data = cur.data(idx0, Eigen::all), rh_data = cur.data(idx1, Eigen::all);
  vol::Ellipsoid lh_ell(lh_data), rh_ell(rh_data);

  // BASE CASE 2: Ellipsoids are malformed or touch.
  if (lh_ell.error() != vol::kOk || rh_ell.error() != vol::kOk) {
    return std::deque<EllipsoidAndData>();
  }
  if (!allow_contact && lh_ell.Intersects(rh_ell)) {
    return std::deque<EllipsoidAndData>();
  }

  // Recursive case: Ellipsoids have reduced volume and can be split further.
  double total_log_vol = logspace_add(lh_ell.log_volume(), rh_ell.log_volume());
  double log_vol_criterion = log(min_reduction) + cur.ell.log_volume();

  vol::EllipsoidAndData lh{std::move(lh_ell), std::move(lh_data)};
  vol::EllipsoidAndData rh{std::move(rh_ell), std::move(rh_data)};

  // If total volume decreased significantly, accept split and recurse
  if (total_log_vol < log_vol_criterion) {
    std::deque<EllipsoidAndData> split;
    split.emplace_back(std::move(lh));
    split.emplace_back(std::move(rh));
    return split;
  }

  // If cur volume much larger than expected, try splitting again.
  double double_expected_vol = M_LN2 + point_volume + log(cur.data.rows());
  if (cur.ell.log_volume() > double_expected_vol) {
    auto sum_log_vol = [](double prev, const EllipsoidAndData& i) {
      return logspace_add(prev, i.ell.log_volume());
    };
    std::deque<EllipsoidAndData> lh_split =
        SplitEllipsoid(lh, min_reduction, allow_contact, point_volume);
    std::deque<EllipsoidAndData> rh_split =
        SplitEllipsoid(rh, min_reduction, allow_contact, point_volume);
    double new_total_log_vol =
        lh_split.empty()
            ? lh.ell.log_volume()
            : std::accumulate(lh_split.begin() + 1, lh_split.end(),
                              lh_split.front().ell.log_volume(), sum_log_vol);
    new_total_log_vol +=
        rh_split.empty()
            ? rh.ell.log_volume()
            : std::accumulate(rh_split.begin() + 1, rh_split.end(),
                              rh_split.front().ell.log_volume(), sum_log_vol);
    if (new_total_log_vol < log_vol_criterion) {
      lh_split.insert(lh_split.end(), rh_split.begin(), rh_split.end());
      return lh_split;
    }
  }

  // Otherwise, return empty deque.
  return std::deque<EllipsoidAndData>();
}

std::vector<Ellipsoid> vol::FitMultiEllipsoids(const ConstRef<Matrix> X,
                                               const double min_reduction,
                                               const bool allow_contact,
                                               const double expected_volume) {
  std::vector<Ellipsoid> ell;
  std::deque<EllipsoidAndData> partitions;

  // Insert and check largest ellipsoid.
  EllipsoidAndData& first = partitions.emplace_back(EllipsoidAndData{Ellipsoid(X), X});
  if (first.ell.error() != kOk) {
    ell.emplace_back(std::move(first.ell));
    return ell;
  }

  double point_log_vol = expected_volume - log(X.rows());
  while (!partitions.empty()) {
    EllipsoidAndData first = std::move(partitions.front());
    partitions.pop_front();
    std::deque<EllipsoidAndData> splits =
        SplitEllipsoid(first, min_reduction, allow_contact, point_log_vol);
    if (splits.empty()) {  // The split failed
      ell.emplace_back(std::move(first.ell));
    } else {  // Split successful
      partitions.emplace_back(std::move(splits[0]));
      partitions.emplace_back(std::move(splits[1]));
    }
  }
  return ell;
}
