#include "random_vector.h"

/**
 * @brief LRPS using uniform points from an ellipsoid.
 *
 * @param unit_log_fn Function to compute the log-likelihood of a proposal.
 * @param criterion The minimum log-likelihood required for acceptance.
 * @param chol_precision The cholesky-decomposition of the precision matrix of
 * the ellipsoid.
 * @param loc Vector specifying the location to shift the sampled point.
 * @param d2 The average squared radius of the ellipsoid.
 * @param max_loop Maximum number of sampling attempts.
 * @return cpp11::list containing the accepted proposal, its log-likelihood, and
 * the number of calls, or only the number of calls if no proposal meets the
 * criterion.
 */
[[cpp11::register]]
cpp11::list EllipsoidImpl(cpp11::function unit_log_fn, double criterion,
                          cpp11::doubles_matrix<> chol_precision,
                          cpp11::doubles loc, double d2, int max_loop) {
  cpp11::writable::doubles proposal(loc.size());
  random_vector::RandomEngine rng;
  for (int i = 0; i < max_loop; i++) {
    random_vector::UniformInEllipsoid(chol_precision, loc, d2, proposal);
    if (random_vector::IsOutsideUnitCube(proposal)) {
      continue;
    }
    double log_lik = unit_log_fn(proposal);
    if (log_lik >= criterion) {
      using namespace cpp11::literals;
      return cpp11::writable::list(
          {"unit"_nm = proposal, "log_lik"_nm = log_lik, "n_call"_nm = i});
    }
  }
  using namespace cpp11::literals;
  return cpp11::writable::list({"n_call"_nm = max_loop});
}
