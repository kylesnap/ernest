#include "bounding.h"

#include <functional>
#include <numeric>

#include "R_ext/Applic.h"

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
  error_ = kOk;
}

// Constructs an empty ellipsoid with specified dimensionality.
//
// `n_dim`: The number of dimensions for the ellipsoid.
Ellipsoid::Ellipsoid(const int n_dim) : n_dim_(n_dim), work_(n_dim) {
  if (n_dim == 0) {
    error_ = kFatal;
    return;
  }
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

// Detect whether two ellipsoids intersect
//
// Employs the method found in: Gilitschenski and U. D. Hanebeck,
// "A robust computational test for overlap of two arbitrary-dimensional
// ellipsoids in fault-detection of Kalman filters," 2012 15th International
// Conference on Information Fusion, Singapore, 2012, pp. 396-401.
//
// `other`: An ellipsoid with same dimensions as `this`.
// `tau`: Scaling parameter (default 1.0).
//
// Returns:
// TRUE if ellipsoids touch, FALSE if they do not touch.
bool Ellipsoid::Overlaps(const Ellipsoid& other, double tau) const {
  // SET UP BRENT METHOD PROBLEM.
  return false;
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

// Extracts row indices that belong to a specific cluster.
//
// `rows`: Original row indices from the parent dataset.
// `sub_idx`: Cluster assignments for each point (0 or 1).
// `cluster`: Which cluster to extract (0 or 1).
//
// Returns: Vector of row indices belonging to the specified cluster.
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

// Splits a cluster into two sub-ellipsoids using k-means clustering.
//
// `X`: Full dataset matrix (n_point x n_dim).
// `rows`: Row indices of points in the current cluster.
// `n_dim`: Number of dimensions.
// `lh`: Output left-hand sub-ellipsoid.
// `rh`: Output right-hand sub-ellipsoid.
//
// Returns: true if splitting resulted in two nondegenerate ellipsoids,
// otherwise false.
static bool split_cluster(const ConstRef<Matrix> X,
                          const std::vector<int>& rows, const int n_dim,
                          SubEllipsoid& lh, SubEllipsoid& rh) {
  Matrix sub_X = X(rows, Eigen::all);
  Eigen::ArrayXd sub_idx(sub_X.rows());
  Eigen::ArrayXXd mu(2, n_dim);
  kmeans_rex::RunKMeans(sub_X, 2, 100, mu, sub_idx);

  lh.rows = find_clusters(rows, sub_idx, 0);
  lh.ell.Fit(X(lh.rows, Eigen::all));
  rh.rows = find_clusters(rows, sub_idx, 1);
  rh.ell.Fit(X(rh.rows, Eigen::all));

  return lh.ell.error() == kOk && rh.ell.error() == kOk;
}

static void split(std::deque<SubEllipsoid>& proposals, SubEllipsoid& lh,
                  SubEllipsoid& rh) {
  int n_dim = lh.ell.n_dim();
  proposals.emplace_back(std::move(lh));
  proposals.emplace_back(std::move(rh));
  lh = SubEllipsoid({Ellipsoid(n_dim), std::vector<int>()});
  rh = SubEllipsoid({Ellipsoid(n_dim), std::vector<int>()});
  proposals.pop_front();
}

static void merge(std::vector<Ellipsoid>& ellipsoids,
                  std::deque<SubEllipsoid>& proposals) {
  ellipsoids.emplace_back(std::move(proposals.front().ell));
  proposals.pop_front();
}

std::vector<Ellipsoid> bounding::FitMultiEllipsoids(const ConstRef<Matrix> X,
                                                    const double min_reduction,
                                                    const double min_dist) {
  int n_dim = X.cols();
  int n_point = X.rows();
  std::vector<Ellipsoid> ellipsoids;
  std::deque<SubEllipsoid> proposals;

  SubEllipsoid lh{Ellipsoid(n_dim), std::vector<int>()};
  SubEllipsoid rh{Ellipsoid(n_dim), std::vector<int>()};
  Ellipsoid intersect(n_dim);

  proposals.emplace_back(
      SubEllipsoid{Ellipsoid(n_dim), std::vector<int>(n_point)});
  proposals.front().ell.Fit(X);
  if (proposals.front().ell.error() != kOk) {
    merge(ellipsoids, proposals);
    return ellipsoids;
  }
  std::iota(proposals.front().rows.begin(), proposals.front().rows.end(), 0);

  while (!proposals.empty()) {
    Ellipsoid& cur = proposals.front().ell;
    std::vector<int>& rows = proposals.front().rows;
    std::cout << "[DEBUG] Proposals size: " << proposals.size()
              << ", Current rows: " << rows.size() << std::endl;
    if (cur.error() != kOk) {
      std::cout << "[DEBUG] Current ellipsoid error: " << cur.error()
                << std::endl;
      proposals.pop_front();
      continue;
    }

    bool splitable = split_cluster(X, rows, n_dim, lh, rh);
    if (!splitable) {
      std::cout << "[DEBUG] Merging after degenerate ellipsoids." << std::endl;
      merge(ellipsoids, proposals);
      continue;
    }

    double cur_log_vol = cur.log_volume() + log(min_reduction);
    double tot_log_vol =
        Rf_logspace_add(lh.ell.log_volume(), rh.ell.log_volume());
    std::cout << "[DEBUG] cur_log_vol: " << cur_log_vol
              << ", tot_log_vol: " << tot_log_vol << std::endl;
    bool less_volume = R_FINITE(tot_log_vol) && tot_log_vol <= cur_log_vol;
    if (!less_volume) {
      std::cout << "[DEBUG] Merging due to insufficient volume reduction."
                << std::endl;
      merge(ellipsoids, proposals);
      continue;
    }

    bool touching = lh.ell.Overlaps(rh.ell);
    std::cout << (touching ? "[DEBUG] Touching" : "[DEBUG] Separate")
              << std::endl;
    if (FALSE) {
      std::cout << "[DEBUG] Merging due to touching clusters." << std::endl;
      merge(ellipsoids, proposals);
    } else {
      std::cout << "[DEBUG] Splitting cluster." << std::endl;
      split(proposals, lh, rh);
    }
  }
  return ellipsoids;
}