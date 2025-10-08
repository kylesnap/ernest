#include <cmath>

#include "bounding.h"
#include "random_generator.h"
#include "utils.h"

/**
 * @brief Find the bounding ellipsoid of the given points.
 * @param X A matrix of points.
 * @returns A list, containing the mean, covariance, transformation matrix,
 * and the log volume of the points, or just NULL if log volume is -Infy.
 */
[[cpp11::register]]
cpp11::list BoundingEllipsoid(cpp11::doubles_matrix<> X) {
  int n_dim = X.ncol();
  Eigen::MatrixXd X_eigen = as_Matrix(X);
  Eigen::RowVectorXd loc(n_dim);
  Eigen::MatrixXd cov(n_dim, n_dim), trans(n_dim, n_dim);
  double scale = R_NegInf, log_vol;

  int result = bounding::Ellipsoid(X_eigen, loc, cov, trans, scale, log_vol);

  using namespace cpp11::literals;
  if (log_vol == R_NegInf) {
    return cpp11::writable::list({"log_vol"_nm = log_vol, "error"_nm = result});
  } else {
    return cpp11::writable::list(
        {"loc"_nm = as_row_doubles(loc), "cov"_nm = as_doubles_matrix(cov),
         "scale"_nm = scale, "trans"_nm = as_doubles_matrix(trans),
         "log_vol"_nm = log_vol, "error"_nm = result});
  }
}

/**
 * @brief LRPS using uniform points from an ellipsoid.
 *
 * @param unit_log_fn Function to compute the log-likelihood of a proposal.
 * @param criterion The minimum log-likelihood required for acceptance.
 * @param trans The transformation matrix of the ellipsoid.
 * @param loc Vector specifying the location to shift the sampled point.
 * @param scale The scale of the ellipsoid.
 * @param max_loop Maximum number of sampling attempts.
 * @return cpp11::list containing the accepted proposal, its log-likelihood, and
 * the number of calls, or only the number of calls if no proposal meets the
 * criterion.
 */
[[cpp11::register]]
cpp11::list EllipsoidImpl(cpp11::function unit_log_fn, double criterion,
                          cpp11::doubles_matrix<> trans, cpp11::doubles loc,
                          double scale, int max_loop) {
  random_generator::RandomEngine rng;
  Eigen::RowVectorXd proposal(loc.size());
  Eigen::MatrixXd wrapped_trans = as_Matrix(trans);
  Eigen::RowVectorXd wrapped_loc = as_row_vector(loc);

  for (int i = 0; i < max_loop; i++) {
    random_generator::UniformInEllipsoid(wrapped_trans, scale, wrapped_loc,
                                         proposal);
    if (random_generator::IsOutsideUnitCube(proposal)) {
      continue;
    }
    double log_lik = unit_log_fn(as_row_doubles(proposal));
    if (log_lik >= criterion) {
      using namespace cpp11::literals;
      return cpp11::writable::list(
          {"unit"_nm = as_row_doubles(proposal.transpose()),
           "log_lik"_nm = log_lik, "n_call"_nm = i + 1});
    }
  }
  using namespace cpp11::literals;
  return cpp11::writable::list({"n_call"_nm = max_loop});
}