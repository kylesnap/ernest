#include "bounding.h"

using namespace bounding;

// Constructs an ellipsoid from a given center and shape matrix.
//
// `center`: The center point of the ellipsoid (n_dim vector).
// `shape`: The shape matrix (n_dim x n_dim positive definite matrix).
bounding::Ellipsoid::Ellipsoid(const ConstRef<Vector> center,
                               const ConstRef<Matrix> shape)
    : n_dim_(center.size()), work_(center.size()) {
  center_ = RowVector(center);
  shape_ = Matrix(shape);

  Eigen::FullPivLU<Matrix> lu(shape);
  if (!lu.isInvertible()) {
    error_ = kFatal;
    return;
  }
  inverse_shape_ = lu.inverse();

  work_.compute(inverse_shape_);
  sqrt_shape_ = work_.operatorInverseSqrt();

  double det_shape = R_pow_di(work_.eigenvalues().prod(), -1);
  log_volume_ = log_volume_const() + log(sqrt(det_shape));
}

// Constructs an empty ellipsoid with specified dimensionality.
//
// `n_dim`: The number of dimensions for the ellipsoid.
Ellipsoid::Ellipsoid(const int n_dim) : n_dim_(n_dim), work_(n_dim) {
  center_ = RowVector::Zero(n_dim_);
  shape_ = Matrix::Zero(n_dim_, n_dim_);
  inverse_shape_ = Matrix::Zero(n_dim_, n_dim_);
  sqrt_shape_ = Matrix::Zero(n_dim_, n_dim_);
};

// Constructs an ellipsoid by fitting to a matrix of points.
//
// Delegates to the empty constructor and then calls Fit() to compute
// the minimum volume ellipsoid enclosing all points.
//
// `X`: Matrix where each row is a point (n_point x n_dim).
Ellipsoid::Ellipsoid(ConstRef<Matrix> X) : Ellipsoid(X.cols()) {
  (*this).Fit(X);
};

// Computes the minimum volume ellipsoid that encloses all points in X using
// a three-step process:
// 1. Compute the center as the mean of all points.
// 2. Find principal axes via eigendecomposition of the covariance matrix.
// 3. Scale axes to ensure all points are enclosed.
//
// The method handles several edge cases:
// - Sets error_ to kFatal if X has wrong dimensions or too few points.
// - Propagates errors from FindAxes() for degenerate cases.
//
// `X`: Matrix where each row is a point (n_point x n_dim).
//
// Post-conditions:
// - error_ indicates success (kOk) or type of numerical issue.
// - If error_ is kFatal, log_volume_ set to R_NegInf.
void Ellipsoid::Fit(ConstRef<Matrix> X) {
  log_volume_ = R_NegInf;
  int n_point = X.rows();
  if (X.cols() != n_dim_) {
    error_ = kFatal;
    return;
  }
  if (n_point <= 1) {
    error_ = kFatal;
    return;
  }

  center_ = X.colwise().mean();
  FindAxes(X);
  ScaleAxes(X);

  work_.compute(inverse_shape_);
  sqrt_shape_ = work_.operatorInverseSqrt();
  double det_shape = R_pow_di(work_.eigenvalues().prod(), -1);
  log_volume_ = log_volume_const() + log(sqrt(det_shape));
}

// Finds the principal axes of the ellipsoid via eigendecomposition of the
// covariance matrix. Handles several degenerate cases:
// - Replaces zero eigenvalues with half of the smallest non-zero eigenvalue.
// - Replaces unconstrained eigenvalues in underdetermined systems.
//
// `X`: Matrix of points (n_point x n_dim), where each row is a point.
//
// Post-conditions:
// - shape_ is set to the sample covariance matrix.
// - inverse_shape_ is set to the inverse of the eigenvalue-scaled matrix.
// - error_ is updated to reflect any numerical issues encountered.
void Ellipsoid::FindAxes(const ConstRef<Matrix> X) {
  error_ = kOk;
  Matrix centered = X.rowwise() - center_;
  shape_ = (centered.adjoint() * centered) / double(X.rows() - 1);

  work_.compute(shape_);
  if (work_.info() == Eigen::NoConvergence) {
    error_ = kFatal;
    return;
  }

  Vector e_values = work_.eigenvalues();
  Matrix e_vectors = work_.eigenvectors();

  int zero_ev = (e_values.array() <= kPrecision).count();
  if (zero_ev == e_values.size()) {
    error_ = kFatal;
  } else if (zero_ev != 0) {
    error_ = kDegenerate;
    e_values.head(zero_ev).setConstant(e_values[zero_ev] / 2.0);
  }

  if (X.rows() < n_dim_ + 1) {
    error_ = kUnderdetermined;
    int unconstrained = n_dim_ + 1 - X.rows();
    e_values.head(unconstrained).setConstant(e_values[unconstrained]);
  }

  inverse_shape_ =
      e_vectors * e_values.cwiseInverse().asDiagonal() * e_vectors.transpose();
}

// Computes the maximum Mahalanobis distance from the center to any point in
// the dataset. This distance determines the scale factor needed to ensure
// all points lie within or on the ellipsoid boundary defined by the shape
// matrix.
//
// `X`: Matrix of points (n_point x n_dim), where each row is a point.
//
// Post-conditions:
// - inverse_shape_ is scaled to ensure all points satisfy the ellipsoid eq.
// - shape_ is scaled accordingly to maintain consistency.
void Ellipsoid::ScaleAxes(const ConstRef<Matrix> X) {
  Matrix diff = X;
  double scale2 = R_NegInf;
  diff.rowwise() -= center_;
  for (auto row : diff.rowwise()) {
    scale2 = Rf_fmax2(scale2, row * inverse_shape_ * row.transpose());
  }
  inverse_shape_ /= scale2;
  shape_ /= scale2;
}

// std::vector<int> find_clusters(const std::vector<int>& rows,
//                                const ConstRef<Vector> cluster_idx,
//                                const int cluster) {
//   std::vector<int> new_indices;
//   for (int i = 0; i < rows.size(); i++) {
//     if (cluster_idx[i] == cluster) new_indices.push_back(rows[i]);
//   }
//   return new_indices;
// }

// void bounding::FitMultiEllipsoids(const ConstRef<Matrix> X,
//                                   std::vector<Ellipsoid>& ellipsoids,
//                                   const double log_volume_reduction) {
//   int n_dim = X.cols();
//   int n_point = X.rows();
//   std::deque<EllipsoidWithIdx> proposals;
//   Eigen::ArrayXXd cluster_loc(2, n_dim);

//   Matrix lh_X(n_point, n_dim), rh_X(n_point, n_dim);

//   proposals.emplace_back(Ellipsoid(n_dim), std::vector<int>(n_point));
//   proposals.front().first.Fit(X);
//   std::iota(proposals.front().second.begin(), proposals.front().second.end(),
//             0);

//   while (!proposals.empty()) {
//     Ellipsoid& cur = proposals.front().first;
//     std::vector<int>& rows = proposals.front().second;
//     if (cur.code() == kFatal) {
//       proposals.pop_front();
//       continue;
//     }

//     std::cout << "[MULTI] Current rows: ";
//     for (const auto& idx : rows) {
//       std::cout << idx << " ";
//     }
//     std::cout << std::endl;
//     Matrix sub_X = X(rows, Eigen::all);
//     Eigen::ArrayXd cluster_idx(sub_X.rows());
//     kmeans_rex::RunKMeans(sub_X, 2, 100, kMethod, cluster_loc, cluster_idx);

//     EllipsoidWithIdx lh =
//         std::make_pair(Ellipsoid(n_dim), find_clusters(rows, cluster_idx,
//         0));
//     lh.first.Fit(X(lh.second, Eigen::all));

//     EllipsoidWithIdx rh =
//         std::make_pair(Ellipsoid(n_dim), find_clusters(rows, cluster_idx,
//         1));
//     rh.first.Fit(X(rh.second, Eigen::all));

//     double old_log_vol = cur.log_volume() + log_volume_reduction;
//     double new_log_volume =
//         Rf_logspace_add(lh.first.log_volume(), rh.first.log_volume());
//     std::cout << "[MULTI] Log Volume " << cur.log_volume() << " -> "
//               << new_log_volume << std::endl;
//     bool split = false;
//     if (R_FINITE(new_log_volume) && new_log_volume <= old_log_vol) {
//       split = true;
//     }

//     if (split) {
//       std::cout << "[MULTI] Splitting into two" << std::endl;
//       proposals.emplace_back(std::move(lh));
//       proposals.emplace_back(std::move(rh));
//     } else {
//       std::cout << "[MULTI] Accepting new ellipsoid" << std::endl;
//       ellipsoids.emplace_back(std::move(cur));
//     }
//     proposals.pop_front();
//   }
// }
