#include "bounding.h"

#include <R_ext/BLAS.h>
#include <R_ext/Lapack.h>

#include <functional>
#include <numeric>

using namespace bounding;

// -----------------------------------------------------------------------------
// MINIPACK-2 wrappers
// -----------------------------------------------------------------------------
extern "C" {
void dgqt_(int* n, double* a, int* lda, double* b, double* delta, double* rtol,
           double* atol, int* itmax, double* par, double* f, double* x,
           int* info, int* iter, double* z, double* wa1, double* wa2);
};

int bounding::Solver::Solve(Ref<Matrix> A, Ref<Vector> b, Ref<Vector> x,
                            double delta) {
  if (A.rows() != n_ || A.cols() != n_) {
    info_ = -1;
    return info_;
  }
  if (b.size() != n_ || x.size() != n_) {
    info_ = -1;
    return info_;
  }
  dgqt_(&n_, A.data(), &n_, b.data(), &delta, &rtol_, &atol_, &it_max_, &par_,
        &f_, x.data(), &info_, &iter_, z_.data(), wa1_.data(), wa2_.data());
  return info_;
}

// -----------------------------------------------------------------------------
// Ellipsoid Constructors
// -----------------------------------------------------------------------------
Ellipsoid::Ellipsoid(const int n_dim) : n_dim_(n_dim) {
  if (n_dim == 0) {
    error_ = kFatal;
    return;
  }
  center_ = Vector::Zero(n_dim_);
  L_ = Matrix::Zero(n_dim_, n_dim_);
  inv_sqrt_shape_ = Matrix::Zero(n_dim_, n_dim_);
};

bounding::Ellipsoid::Ellipsoid(const ConstRef<Vector> center,
                               const ConstRef<Matrix> shape)
    : Ellipsoid(center.size()) {
  if (!((shape.rows() == shape.cols()) && (shape.rows() == n_dim_))) {
    error_ = kFatal;
    return;
  }
  center_ = center;
  Eigen::SelfAdjointEigenSolver<Matrix> work(shape);
  Eigensystem eig{work.eigenvalues(), work.eigenvectors()};
  (*this).SetShape(eig);
  error_ = kOk;
}

Ellipsoid::Ellipsoid(ConstRef<Matrix> X) : Ellipsoid(X.cols()) {
  (*this).Fit(X);
};

// -----------------------------------------------------------------------------
// Ellipsoid Fitting and Shape
// -----------------------------------------------------------------------------
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

  center_ = X.colwise().mean().transpose();
  Eigensystem eig = FindAxes(X);
  ScaleAxes(X, eig);
  SetShape(eig);
}

double Ellipsoid::Distance(const ConstRef<Vector> point) const {
  Vector y = point - center_;
  y = L_.transpose() * y;
  double y_sum = y.squaredNorm();
  if (y_sum > 0) {
    return 1 / sqrt(y_sum);
  } else if (point.isApprox(center_)) {
    return NAN;
  } else {
    return R_NegInf;
  }
}

static Vector FindClosest(const ConstRef<Matrix> L,
                          const ConstRef<Vector> center,
                          const ConstRef<Vector> point) {
  Matrix gi = L.inverse();    // g_i = L^-1
  Vector b = center - point;  //  b = G^{-1} * [c-p]
  Vector y(L.rows());
  b = gi * b;

  Matrix a = gi * gi.transpose();  // a = G^(-1) * G^(-T)
  double delta = 1.0;

  Solver s(L.rows());
  int info = s.Solve(a, b, y, delta);
  if (info > 2) {
    cpp11::warning("dgqt incomplete convergence");  // xnf = G^{-T} * ynf + c
  }
  return gi.transpose() * y + center;
}

Vector Ellipsoid::Closest(const ConstRef<Vector> point) const {
  return FindClosest(L_, center_, point);
};

double Ellipsoid::Distance(const Ellipsoid& other) const {
  Matrix g1 = L_;
  Matrix g2 = other.L_;

  // Transform to y-space:  y = G1^T * (x-c1);  G2y = G1^{-1} * G2
  Matrix g2y = g1.inverse() * g2;
  Vector c2y = other.center_ - center_;
  c2y = g1.transpose() * c2y;

  // Find point in ell(c2y, g2y) closest to origin.
  Vector v = Vector::Zero(n_dim_);
  Vector y2 = FindClosest(g2y, c2y, v);
  if (y2.squaredNorm() <= 1.0) {
    return R_NegInf;
  }

  // Transform back to x-space
  double y2_norm = y2.norm();
  g1.triangularView<Eigen::Lower>().transpose().solveInPlace(y2);
  Vector x2 = y2 + center_;  // x2 = G1^{-T} * y2 + c1
  Vector x1 = y2 / y2_norm + center_;
  return (x2 - x1).norm();
}

Eigensystem Ellipsoid::FindAxes(const ConstRef<Matrix> X) {
  error_ = kOk;
  Matrix centered = X.rowwise() - center_.transpose();
  Matrix L_ = (centered.adjoint() * centered) / double(X.rows() - 1);

  Eigen::SelfAdjointEigenSolver<Matrix> work(L_);
  if (work.info() != Eigen::Success) {
    error_ = kFatal;
    return {Vector::Zero(n_dim_), Matrix::Zero(n_dim_, n_dim_)};
  }

  Vector e_values = work.eigenvalues();
  int zero_ev = (e_values.array() <= kPrecision).count();
  if (zero_ev == e_values.size()) {
    error_ = kNilpotent;
    return {Vector::Zero(n_dim_), Matrix::Zero(n_dim_, n_dim_)};
  } else if (zero_ev != 0) {
    error_ = kDegenerate;
    e_values.head(zero_ev).setConstant(e_values[zero_ev] / 2.0);
  }

  if (X.rows() < n_dim_ + 1) {
    error_ = kUnderdetermined;
    int unconstrained = n_dim_ + 1 - X.rows();
    e_values.head(unconstrained).setConstant(e_values[unconstrained]);
  }

  return {e_values.cwiseInverse(), work.eigenvectors()};
}

void Ellipsoid::ScaleAxes(ConstRef<Matrix> X, Eigensystem& eig) {
  L_ = eig.vectors * eig.values.asDiagonal() * eig.vectors.transpose();

  Matrix diff = X.rowwise() - center_.transpose();
  double max_dist2 = R_NegInf;
  for (int i = 0; i < diff.rows(); ++i) {
    double dist2 = diff.row(i) * L_ * diff.row(i).transpose();
    if (dist2 > max_dist2) max_dist2 = dist2;
  }
  eig.values /= max_dist2;

  diff = X.rowwise() - center_.transpose();
  L_ = eig.vectors * eig.values.asDiagonal() * eig.vectors.transpose();
  max_dist2 = R_NegInf;
  for (int i = 0; i < diff.rows(); ++i) {
    double dist2 = diff.row(i) * L_ * diff.row(i).transpose();
    if (dist2 > max_dist2) max_dist2 = dist2;
  }
}

void Ellipsoid::SetShape(Eigensystem& eig) {
  inv_sqrt_shape_ = eig.vectors * eig.values.cwiseSqrt().asDiagonal();
  Eigen::HouseholderQR<Matrix> qr(inv_sqrt_shape_.transpose());
  L_ = qr.matrixQR().triangularView<Eigen::Upper>().transpose();
  log_volume_ = log_volume_const() + -qr.logAbsDeterminant();
  inv_sqrt_shape_ = eig.vectors *
                    eig.values.cwiseInverse().cwiseSqrt().asDiagonal() *
                    eig.vectors.adjoint();
}

// -----------------------------------------------------------------------------
// Cluster Extraction and Splitting
// -----------------------------------------------------------------------------

// Extracts row indices belonging to a specific cluster.
// Parameters:
//   rows    - Original row indices from parent dataset.
//   sub_idx - Cluster assignments for each point (0 or 1).
//   cluster - Which cluster to extract (0 or 1).
// Returns:
//   Vector of row indices for the specified cluster.
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
// Parameters:
//   X      - Full dataset matrix (n_point x n_dim).
//   rows   - Row indices of points in current cluster.
//   n_dim  - Number of dimensions.
//   lh     - Output left-hand sub-ellipsoid.
//   rh     - Output right-hand sub-ellipsoid.
// Returns:
//   true if split yields two nondegenerate ellipsoids, false otherwise.
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

// -----------------------------------------------------------------------------
// Proposal Management (Splitting/Merging)
// -----------------------------------------------------------------------------

// Splits proposals into left and right sub-ellipsoids.
static void split(std::deque<SubEllipsoid>& proposals, SubEllipsoid& lh,
                  SubEllipsoid& rh) {
  int n_dim = lh.ell.n_dim();
  proposals.emplace_back(std::move(lh));
  proposals.emplace_back(std::move(rh));
  lh = SubEllipsoid({Ellipsoid(n_dim), std::vector<int>()});
  rh = SubEllipsoid({Ellipsoid(n_dim), std::vector<int>()});
  proposals.pop_front();
}

// Merges current proposal into ellipsoids.
static void merge(std::vector<Ellipsoid>& ellipsoids,
                  std::deque<SubEllipsoid>& proposals) {
  ellipsoids.emplace_back(std::move(proposals.front().ell));
  proposals.pop_front();
}

// -----------------------------------------------------------------------------
// Multi-Ellipsoid Fitting
// -----------------------------------------------------------------------------

// Fits multiple ellipsoids to the dataset using recursive splitting.
// Parameters:
//   X             - Data matrix (n_point x n_dim).
//   min_reduction - Minimum required volume reduction for splitting.
//   allow_contact      - Are the ellipsoids allowed to touch?
// Returns:
//   Vector of fitted Ellipsoid objects.
std::vector<Ellipsoid> bounding::FitMultiEllipsoids(const ConstRef<Matrix> X,
                                                    const double min_reduction,
                                                    const bool allow_contact) {
  int n_dim = X.cols();
  int n_point = X.rows();
  std::vector<Ellipsoid> ellipsoids;
  std::deque<SubEllipsoid> proposals;

  SubEllipsoid lh{Ellipsoid(n_dim), std::vector<int>()};
  SubEllipsoid rh{Ellipsoid(n_dim), std::vector<int>()};
  Ellipsoid intersect(n_dim);

  std::string dbg_prefix = "[FitMultiEllipsoids] ";

  proposals.emplace_back(
      SubEllipsoid{Ellipsoid(n_dim), std::vector<int>(n_point)});
  proposals.front().ell.Fit(X);
  if (proposals.front().ell.error() != kOk) {
    std::cout << dbg_prefix
              << "Initial fit error: " << proposals.front().ell.error()
              << std::endl;
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

    bool splitable = split_cluster(X, rows, n_dim, lh, rh);
    if (!splitable) {
      merge(ellipsoids, proposals);
      continue;
    }

    double cur_log_vol = cur.log_volume() + log(min_reduction);
    double tot_log_vol =
        Rf_logspace_add(lh.ell.log_volume(), rh.ell.log_volume());
    bool less_volume = R_FINITE(tot_log_vol) && tot_log_vol <= cur_log_vol;
    if (!less_volume) {
      merge(ellipsoids, proposals);
      continue;
    }

    if (!allow_contact && lh.ell.Distance(rh.ell) < 0) {
      merge(ellipsoids, proposals);
    } else {
      split(proposals, lh, rh);
    }
  }
  return ellipsoids;
}