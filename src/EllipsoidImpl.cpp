#include <cmath>

#include "bounding.h"
#include "random_generator.h"
#include "utils.h"

/**
 * @brief Find the bounding ellipsoid of the given points.
 * @param X A matrix of points.
 * @returns A list, containing the mean, precision matrix,
 * transformation matrix, scale, and the log volume of the ellipsoid,
 * or just NULL if log volume is -Infy.
 */
[[cpp11::register]]
cpp11::list BoundingEllipsoid(cpp11::doubles_matrix<> X) {
  Eigen::MatrixXd X_eigen = as_Matrix(X);
  bounding::Ellipsoid ell(X_eigen);
  return ell.as_list();
}

/**
 * @brief LRPS using uniform points from an ellipsoid.
 *
 * @param unit_log_fn Function to compute the log-likelihood of a proposal.
 * @param criterion The minimum log-likelihood required for acceptance.
 * @param scaledInvSqrtA The transformation matrix for the ellipsoid.
 * @param loc Vector specifying the location to shift the sampled point.
 * @param max_loop Maximum number of sampling attempts.
 * @return cpp11::list containing the accepted proposal, its log-likelihood, and
 * the number of calls, or only the number of calls if no proposal meets the
 * criterion.
 */
[[cpp11::register]]
cpp11::list EllipsoidImpl(cpp11::function unit_log_fn, double criterion,
                          cpp11::doubles_matrix<> scaledInvSqrtA,
                          cpp11::doubles loc, int max_loop) {
  random_generator::RandomEngine rng;
  Eigen::RowVectorXd proposal(loc.size());
  Eigen::MatrixXd wrapped_tA = as_Matrix(scaledInvSqrtA);
  Eigen::RowVectorXd wrapped_loc = as_row_vector(loc);

  for (int i = 0; i < max_loop; i++) {
    random_generator::UniformInEllipsoid(wrapped_tA, wrapped_loc, proposal);
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