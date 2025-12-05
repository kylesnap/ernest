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

// Ellipsoid Constructors
Ellipsoid::Ellipsoid(const ConstRef<Vector> center, const ConstRef<Matrix> shape) {
  n_dim_ = center.size();
  if (!((shape.rows() == shape.cols()) && (shape.rows() == n_dim_))) {
    error_ = kFatal;
    return;
  }
  center_ = center;
  shape_ = shape;
  SetShape();
}

void vol::Ellipsoid::SetShape() {
  log_volume_ = log_volume_sphere() + (log(1 / shape_.determinant()) / 2.0);
  // inv_sqrt_shape_ = V * D^{-1/2} * V^T
  Eigen::SelfAdjointEigenSolver<Matrix> work(shape_);
  axial_lengths_ = work.eigenvalues().cwiseInverse().cwiseSqrt();
  inv_sqrt_shape_ = work.operatorInverseSqrt();
}

//// Ellipsoid Tools

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

Matrix Ellipsoid::major_axes() const {
  // Largest axis
  Eigen::Index max_i;
  axial_lengths_.maxCoeff(&max_i);
  Vector axes = inv_sqrt_shape_.col(max_i);  // The major axis of the ellipsoid
  Matrix endpoints(2, n_dim_);
  endpoints.row(0) = center_ + axes;
  endpoints.row(1) = center_ - axes;
  return endpoints;
}

// Ellipsoid Fitting

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
    double radius_ = R_pow_di(exp(point_log_volume - log_volume_sphere()), 1.0 / n_dim_);
    shape_.setIdentity();
    shape_ *= radius_;
    SetShape();
    return;
  }

  center_ = X.colwise().mean().transpose();
  Matrix centered_ = X.rowwise() - center_.transpose();
  // Store covariance in `shape_L_`
  shape_ = centered_.adjoint() * centered_ / double(X.rows() - 1);

  // Adjust for O(1/n_dim) bias in sample covariance
  shape_ *= n_dim_ + 2.0;

  // Find the axes of the ellipsoid
  double log_target = 2 * (log(n_points) - log_volume_sphere() + point_log_volume);
  FindAxes(shape_, log_target);
  if (error_ == kFatal || error_ == kNilpotent) {
    Status prev_error = error_;
    *this = Ellipsoid(n_dim_);
    error_ = prev_error;
    return;
  }

  // Scale the axes to fit the data before finalizing
  ScaleAxes(X);
  SetShape();
}

void Ellipsoid::FindAxes(Ref<Matrix> cov, const double log_target) {
  Eigen::SelfAdjointEigenSolver<Matrix> work(cov);
  Eigen::VectorXd eigvals = work.eigenvalues();
  if ((eigvals.array() >= 1e-10).all()) {
    // cov is well-conditioned
    cov = work.eigenvectors() * eigvals.cwiseInverse().asDiagonal() *
          work.eigenvectors().adjoint();
    return;
  } else if ((eigvals.array() < 1e-10).all()) {
    // cov is nilpotent
    error_ = kNilpotent;
    return;
  } else {
    // cov has at least one non-zero eigenvalues.
    error_ = kDegenerate;
  }

  double nonzero_prod = 1;
  std::vector<int> zero;
  for (int i = 0; i < eigvals.size(); ++i) {
    if (eigvals[i] > 1e-10) {
      nonzero_prod *= eigvals[i];
    } else {
      zero.push_back(i);
    }
  }

  for (int idx : zero) {
    eigvals[idx] = R_pow_di(exp((log_target - log(nonzero_prod))), 1 / zero.size());
  }
  cov = work.eigenvectors() * eigvals.cwiseInverse().asDiagonal() *
        work.eigenvectors().adjoint();
}

void vol::Ellipsoid::ScaleAxes(ConstRef<Matrix> X) {
  // Calculate squared Mahalanobis distances
  Eigen::MatrixXd diff = X.rowwise() - center_.transpose();
  Eigen::VectorXd distances(diff.rows());
  for (int i = 0; i < diff.rows(); ++i) {
    Eigen::VectorXd point = diff.row(i).transpose();
    distances(i) = point.transpose() * shape_ * point;
  }

  // Find maximum distance
  double max_dist = distances.maxCoeff();
  if (max_dist <= 0) {
    error_ = kFatal;
    return;
  }

  double one_minus_eps = 1.0 - 1e-10;
  if (max_dist > one_minus_eps) {
    shape_ *= one_minus_eps / max_dist;
  }
}

// Clustering Helpers

// static std::deque<EllipsoidAndData> SplitEllipsoid(const vol::EllipsoidAndData& cur,
//                                                    const double min_reduction,
//                                                    const bool allow_contact,
//                                                    const double point_volume) {
//   // Perform k-means clustering with k=2
//   size_t n_dim = cur.ell.n_dim();
//   size_t n_points = cur.data.rows();
//   Matrix endpoints = cur.ell.major_axes();
//   Vector labels(n_points);
//   kmeans_rex::RunKMeans(cur.data, 2, 10, endpoints, labels);

//   // Split points into two clusters
//   std::vector<int> idx0, idx1;
//   for (int i = 0; i < n_points; ++i) {
//     if (labels[i] == 0) {
//       idx0.push_back(i);
//     } else {
//       idx1.push_back(i);
//     }
//   }

//   // BASE CASE 1: Clusters get too small
//   if (idx0.size() < 2 * n_dim || idx1.size() < 2 * n_dim) {
//     return std::deque<EllipsoidAndData>();
//   }

//   // Fit ellipsoids to each cluster
//   Matrix lh_data = cur.data(idx0, Eigen::all), rh_data = cur.data(idx1, Eigen::all);
//   vol::Ellipsoid lh_ell(lh_data), rh_ell(rh_data);

//   // BASE CASE 2: Ellipsoids are malformed or touch.
//   if (lh_ell.error() != vol::kOk || rh_ell.error() != vol::kOk) {
//     return std::deque<EllipsoidAndData>();
//   }
//   if (!allow_contact && lh_ell.Intersects(rh_ell)) {
//     return std::deque<EllipsoidAndData>();
//   }

//   // Recursive case: Ellipsoids have reduced volume and can be split further.
//   double total_log_vol = logspace_add(lh_ell.log_volume(), rh_ell.log_volume());
//   double log_vol_criterion = log(min_reduction) + cur.ell.log_volume();

//   vol::EllipsoidAndData lh{std::move(lh_ell), std::move(lh_data)};
//   vol::EllipsoidAndData rh{std::move(rh_ell), std::move(rh_data)};

//   // If total volume decreased significantly, accept split and recurse
//   if (total_log_vol < log_vol_criterion) {
//     std::deque<EllipsoidAndData> split;
//     split.emplace_back(std::move(lh));
//     split.emplace_back(std::move(rh));
//     return split;
//   }

//   // If cur volume much larger than expected, try splitting again.
//   double double_expected_vol = M_LN2 + point_volume + log(cur.data.rows());
//   if (cur.ell.log_volume() > double_expected_vol) {
//     auto sum_log_vol = [](double prev, const EllipsoidAndData& i) {
//       return logspace_add(prev, i.ell.log_volume());
//     };
//     std::deque<EllipsoidAndData> lh_split =
//         SplitEllipsoid(lh, min_reduction, allow_contact, point_volume);
//     std::deque<EllipsoidAndData> rh_split =
//         SplitEllipsoid(rh, min_reduction, allow_contact, point_volume);
//     double new_total_log_vol =
//         lh_split.empty()
//             ? lh.ell.log_volume()
//             : std::accumulate(lh_split.begin() + 1, lh_split.end(),
//                               lh_split.front().ell.log_volume(), sum_log_vol);
//     new_total_log_vol +=
//         rh_split.empty()
//             ? rh.ell.log_volume()
//             : std::accumulate(rh_split.begin() + 1, rh_split.end(),
//                               rh_split.front().ell.log_volume(), sum_log_vol);
//     if (new_total_log_vol < log_vol_criterion) {
//       lh_split.insert(lh_split.end(), rh_split.begin(), rh_split.end());
//       return lh_split;
//     }
//   }

//   // Otherwise, return empty deque.
//   return std::deque<EllipsoidAndData>();
// }

// std::vector<Ellipsoid> vol::FitMultiEllipsoids(const ConstRef<Matrix> X,
//                                                const double min_reduction,
//                                                const bool allow_contact,
//                                                const double expected_volume) {
//   std::vector<Ellipsoid> ell;
//   std::deque<EllipsoidAndData> partitions;

//   // Insert and check largest ellipsoid.
//   EllipsoidAndData& first = partitions.emplace_back(EllipsoidAndData{Ellipsoid(X), X});
//   if (first.ell.error() != kOk) {
//     ell.emplace_back(std::move(first.ell));
//     return ell;
//   }

//   double point_log_vol = expected_volume - log(X.rows());
//   while (!partitions.empty()) {
//     EllipsoidAndData first = std::move(partitions.front());
//     partitions.pop_front();
//     std::deque<EllipsoidAndData> splits =
//         SplitEllipsoid(first, min_reduction, allow_contact, point_log_vol);
//     if (splits.empty()) {  // The split failed
//       ell.emplace_back(std::move(first.ell));
//     } else {  // Split successful
//       partitions.emplace_back(std::move(splits[0]));
//       partitions.emplace_back(std::move(splits[1]));
//     }
//   }
//   return ell;
// }
