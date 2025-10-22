#include <cmath>

#include "KMeansRexCore.h"
#include "bounding.h"

/**
 * @brief Find the bounding ellipsoid of the given points.
 * @param X A matrix of points.
 * @returns A list, containing the mean, covariance, transformation matrix,
 * and the log volume of the points, or just NULL if log volume is -Infy.
 */
[[cpp11::register]]
cpp11::list MultiBoundingEllipsoids(cpp11::doubles_matrix<> X,
                                    const double min_reduction,
                                    const bool allow_contact) {
  Eigen::MatrixXd X_eigen = as_Matrix(X);
  std::vector<bounding::Ellipsoid> ellipsoids =
      bounding::FitMultiEllipsoids(X_eigen, min_reduction, allow_contact);
  cpp11::writable::list ellipsoid_list;
  cpp11::writable::doubles prob;
  for (auto ell : ellipsoids) {
    prob.push_back(ell.log_volume());
    ellipsoid_list.push_back(ell.as_list());
  }
  double total_log_vol = logspace_sum(REAL(prob.data()), prob.size());
  for (auto p : prob) {
    p = exp(p - total_log_vol);
  }
  using namespace cpp11::literals;
  return cpp11::writable::list(
      {"prob"_nm = prob, "ellipsoid"_nm = ellipsoid_list});
}