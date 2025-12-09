#pragma once

#include "Rmath.h"
#include "utils.h"

namespace vol {
using ern::Matrix, ern::Vector, ern::RowVector, ern::Ref, ern::ConstRef;

// Abstract base class for distance matrix computation.
class DistanceMatrix {
 public:
  virtual Matrix operator()(const ConstRef<Matrix> X) = 0;
  virtual ~DistanceMatrix() = default;
};

// Squared Euclidean distance matrix.
class SquaredEuclideanDistances : public DistanceMatrix {
 public:
  Matrix operator()(const ConstRef<Matrix> X) override;
};

// Chebshev distance matrix.
class ChebshevDistances : public DistanceMatrix {
 public:
  Matrix operator()(const ConstRef<Matrix> X) override;
};

// MiniBall class for computing minimum enclosing ball with resampling methods.
class MiniBall {
 public:
  // Constructor
  MiniBall(const ConstRef<Matrix> points, std::unique_ptr<DistanceMatrix> dist_method);
  // Bootstrap method to estimate maximum radius.
  double bootstrap(int n_bootstraps = 30) const;
  // Jacknife method to estimate maximum radius.
  double jacknife() const;

 private:
  std::unique_ptr<DistanceMatrix> dist_method_;  // Distance computation method
  Eigen::MatrixXd distances_;                    // Precomputed distance matrix

  // Compute maximum radius given two sets of points in 'distances_'.
  inline double ComputeMaxRadius_(const std::vector<int>& apts,
                                  const std::vector<int>& bpts) const {
    return distances_(apts, bpts).colwise().minCoeff().maxCoeff();
  }
};

}  // namespace vol