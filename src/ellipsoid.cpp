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

//// CONSTRUCTORS

Ellipsoid::Ellipsoid(const ConstRef<Vector> center, const ConstRef<Matrix> shape) {
  n_dim_ = center.size();
  if (!((shape.rows() == shape.cols()) && (shape.rows() == n_dim_))) {
    error_ = kFatal;
    return;
  }
  center_ = center;
  shape_ = shape;
  work_ = Eigen::SelfAdjointEigenSolver<Matrix>(n_dim_);
  SetShape();
}

//// ELLIPSOIDAL CALCULUS

double vol::Ellipsoid::Distance(const ConstRef<Vector> point) const {
  Vector y = point - center_;
  y = shape_.llt().matrixU() * y;
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

bool vol::Ellipsoid::Covered(const ConstRef<Vector> point) const {
  double dist = Distance(point);
  if (!R_FINITE(dist)) {
    if (R_IsNaN(dist)) return true;
    cpp11::stop("Numerical issue encountered.");
    return false;
  }
  return dist >= 1.0;
};

Matrix Ellipsoid::major_axis() const {
  Eigen::Index max_i;
  axial_lengths_.maxCoeff(&max_i);
  Vector axes = inv_sqrt_shape_.col(max_i);
  Matrix endpoints(2, n_dim_);
  endpoints.row(0) = center_ + axes;
  endpoints.row(1) = center_ - axes;
  return endpoints;
}

//// MIN VOL FITTING

void Ellipsoid::Fit(ConstRef<Matrix> X, double point_log_volume) {
  log_volume_ = R_NegInf;
  int n_points = X.rows();
  if (X.cols() != n_dim_ || n_points < 1) {
    error_ = kFatal;
    return;
  }

  if (n_points == 1) {
    // Degenerate case: A n-dim sphere with volume `min_log_volume`
    center_ = X.row(0).transpose();
    if (R_IsNA(point_log_volume)) {
      point_log_volume = 0.0;
    }
    double radius_ = R_pow_di(exp(point_log_volume - log_volume_sphere()), 1.0 / n_dim_);
    shape_.setIdentity();
    shape_ *= radius_;
    error_ = kUnderdetermined;
    SetShape();
    return;
  }

  center_ = X.colwise().mean().transpose();
  Matrix centered_ = X.rowwise() - center_.transpose();
  shape_ = centered_.adjoint() * centered_ / double(X.rows() - 1);

  // Adjust for O(1/n_dim) bias in sample covariance
  shape_ *= n_dim_ + 2.0;

  // Find the axes of the ellipsoid
  double log_target = R_IsNA(point_log_volume)
                          ? 0.0
                          : 2 * (log(n_points) - log_volume_sphere() + point_log_volume);
  FindAxes(shape_, log_target);
  if (error_ == kFatal || error_ == kNilpotent) {
    Status prev_error = error_;
    *this = Ellipsoid(n_dim_);
    error_ = prev_error;
    return;
  }

  // Scale the axes to fit the data.
  ScaleAxes(X);
  SetShape();

  // Scale to target volume
  if (!R_IsNA(point_log_volume)) {
    double log_target = log(n_points) + point_log_volume;
    if (log_volume_ < log_target) {
      log_volume(log_target);
    }
  }
}

// Inverts `cov` in-place, checking first for zero eigenvalues. If any (but not
// all) eigenvalues are zero, they are set to a value such that the resulting
// precision matrix has log-determinant `log_target`.
void Ellipsoid::FindAxes(Ref<Matrix> cov, const double log_target) {
  work_.compute(cov);
  Eigen::VectorXd eigvals = work_.eigenvalues();
  if ((eigvals.array() >= kAlmostZero).all()) {
    cov = work_.eigenvectors() * eigvals.cwiseInverse().asDiagonal() *
          work_.eigenvectors().adjoint();
    return;
  } else if ((eigvals.array() < kAlmostZero).all()) {
    error_ = kNilpotent;
    return;
  }

  error_ = kDegenerate;
  double nonzero_prod = 1;
  std::vector<int> zero;
  for (int i = 0; i < eigvals.size(); ++i) {
    if (eigvals[i] > kAlmostZero) {
      nonzero_prod *= eigvals[i];
    } else {
      zero.push_back(i);
    }
  }

  for (int idx : zero) {
    eigvals[idx] = R_pow_di(exp(log_target - log(nonzero_prod)), 1 / zero.size());
  }
  cov = work_.eigenvectors() * eigvals.cwiseInverse().asDiagonal() *
        work_.eigenvectors().adjoint();
}

// Calculates squared mahalanobis distances of points in `X` to the ellipsoid center,
// then scales the shape matrix so that no point lies outside self.
void vol::Ellipsoid::ScaleAxes(ConstRef<Matrix> X) {
  Eigen::MatrixXd diff = X.rowwise() - center_.transpose();
  Eigen::VectorXd distances(diff.rows());
  for (int i = 0; i < diff.rows(); ++i) {
    Eigen::VectorXd point = diff.row(i).transpose();
    distances(i) = point.transpose() * shape_ * point;
  }

  double max_dist = distances.maxCoeff();
  if (max_dist <= kAlmostZero) {
    error_ = kFatal;
    return;
  }
  if (max_dist > kOneMinusEpsilon) {
    shape_ *= kOneMinusEpsilon / max_dist;
  }
}

void vol::Ellipsoid::SetShape() {
  log_volume_ = log_volume_sphere() + (log(1 / shape_.determinant()) / 2.0);
  work_.compute(shape_);
  axial_lengths_ = work_.eigenvalues().cwiseInverse().cwiseSqrt();
  inv_sqrt_shape_ = work_.operatorInverseSqrt();
}

//// MULTI-ELLIPSOID FITTING

std::list<Ellipsoid> vol::Ellipsoid::FitMany(const ConstRef<Matrix> X,
                                             double point_log_volume) {
  ern::RandomEngine rng;
  Ellipsoid largest(X, point_log_volume);
  return largest.Split(X, point_log_volume);
}

// 1. Perform k-means clustering with k=2 to split the points into two clusters.
// 2. Fit ellipsoids to each cluster. Exit if either is malformed.
// 3. If the sum of the log-volumes of the two ellipsoids is less than half the
//    log-volume of the parent ellipsoid, recursively split each child.
// 4. Else, if the parent ellipsoid's log-volume exceeds the target log-volume,
//    recursively split each child to see whether it's possible to reduce volume further
//    by some signficant volume (currently, by 1/2).
// 5. Else, return the parent ellipsoid.
std::list<Ellipsoid> vol::Ellipsoid::Split(const ConstRef<Matrix> X,
                                           double point_log_volume, int depth) const {
  int n_points = X.rows();
  Matrix endpoints = major_axis();
  Vector labels(n_points);
  kmeans_rex::RunKMeans(X, 2, 10, endpoints, labels);
  std::vector<int> idx0, idx1;
  for (int i = 0; i < n_points; ++i) {
    if (labels[i] == 0)
      idx0.push_back(i);
    else
      idx1.push_back(i);
  }

  // BASE CASE 1: Clusters too small to split
  if (idx0.size() < 2 * n_dim_ || idx1.size() < 2 * n_dim_) {
    return {*this};
  }

  // Fit ellipsoids to each cluster
  Ellipsoid left(X(idx0, Eigen::all), point_log_volume);
  Ellipsoid right(X(idx1, Eigen::all), point_log_volume);

  // BASE CASE 2: Malformed ellipsoids
  if (left.error() != kOk || right.error() != kOk) {
    return {*this};
  }

  // Expected min log volume decrease between two ellipsoids
  // See `bounding_ellipsoids_` in dynesty and Feroz (2008) for mathematical details.
  double log_volume_decrement = ((n_dim_ * (n_dim_ + 3)) / 2) * log(n_points) / n_points;
  double child_log_vol = logspace_add(left.log_volume(), right.log_volume());

  // RECURSIVE STEP: Split children into left and right lists
  std::list<Ellipsoid> left_list =
      left.Split(X(idx0, Eigen::all), point_log_volume, depth + 1);
  std::list<Ellipsoid> right_list =
      right.Split(X(idx1, Eigen::all), point_log_volume, depth + 1);
  left_list.splice(left_list.end(), std::move(right_list));

  if (child_log_vol - log_volume_ <= -log_volume_decrement) {
    return left_list;
  }
  double cum_log_vol = std::accumulate(
      left_list.begin()++, left_list.end(), left_list.begin()->log_volume(),
      [](double prev, const Ellipsoid& i) { return logspace_add(prev, i.log_volume()); });
  if (cum_log_vol - log_volume_ <= -log_volume_decrement * (left_list.size() - 1)) {
    return left_list;
  }
  return {*this};
}

cpp11::list vol::Ellipsoid::as_list(const std::list<Ellipsoid>& ellipsoids) {
  cpp11::writable::doubles probs;
  cpp11::writable::list ellipsoid_list;
  double total_log_vol = -1e300;
  for (auto ell : ellipsoids) {
    probs.push_back(ell.log_volume());
    total_log_vol = Rf_logspace_add(total_log_vol, ell.log_volume());
    ellipsoid_list.push_back(ell.as_list());
  }
  std::transform(probs.begin(), probs.end(), probs.begin(),
                 [total_log_vol](double p) { return exp(p - total_log_vol); });
  return cpp11::writable::list({"prob"_nm = probs, "ellipsoid"_nm = ellipsoid_list,
                                "tot_log_vol"_nm = total_log_vol});
}
