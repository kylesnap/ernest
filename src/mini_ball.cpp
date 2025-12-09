#include "mini_ball.h"

#include <numeric>

vol::Matrix vol::SquaredEuclideanDistances::operator()(const ConstRef<Matrix> X) {
  Matrix dist = X * X.transpose();
  Eigen::VectorXd diag = dist.diagonal();
  dist *= -2;
  dist += diag.transpose().replicate(dist.rows(), 1) + diag.replicate(1, dist.cols());
  dist.diagonal().setConstant(R_PosInf);
  return dist;
}

vol::Matrix vol::ChebshevDistances::operator()(const ConstRef<Matrix> X) {
  Matrix dist = Matrix::Zero(X.rows(), X.rows());
  for (int i = 0; i < X.rows(); ++i) {
    for (int j = 0; j < X.rows(); ++j) {
      dist(i, j) = (X.row(i) - X.row(j)).cwiseAbs().maxCoeff();
    }
  }
  dist.diagonal().setConstant(R_PosInf);
  return dist;
}

vol::MiniBall::MiniBall(const ConstRef<Matrix> points,
                        std::unique_ptr<DistanceMatrix> dist_method)
    : dist_method_(std::move(dist_method)), distances_((*dist_method_)(points)) {}

double vol::MiniBall::bootstrap(int n_bootstraps) const {
  ern::RandomEngine rng;
  double max_d = 0.0;
  int n_points = distances_.rows();
  std::vector<int> all_indices(n_points);
  std::iota(all_indices.begin(), all_indices.end(), 0);

  std::vector<int> choice;
  std::vector<int> anti_choice;

  for (int boot = 0; boot < n_bootstraps; ++boot) {
    // Full bootstrap: sample n_points indices with replacement
    std::vector<int> bootstrap_indices(n_points);
    for (int i = 0; i < n_points; ++i) {
      bootstrap_indices[i] = static_cast<int>(unif_rand() * n_points);
    }
    // Mark which indices are in the bootstrap sample
    std::vector<bool> in_sample(n_points, false);
    for (int idx : bootstrap_indices) {
      in_sample[idx] = true;
    }
    choice.clear();
    anti_choice.clear();
    for (int i = 0; i < n_points; ++i) {
      if (in_sample[i]) {
        choice.push_back(i);
      } else {
        anti_choice.push_back(i);
      }
    }
    // Ensure both sets are non-empty
    if (choice.empty() || anti_choice.empty()) {
      continue;
    }
    double d = ComputeMaxRadius_(choice, anti_choice);
    max_d = fmax2(max_d, d);
  }
  return max_d;
}

double vol::MiniBall::jacknife() const {
  double max_d = 0.0;
  int n_points = distances_.rows();
  for (int i = 0; i < n_points; ++i) {
    double d = distances_.col(i).minCoeff();
    max_d = fmax2(max_d, d);
  }
  return max_d;
}
