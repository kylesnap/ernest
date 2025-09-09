#include "random_vector.h"

/**
 * @brief LRPS using uniform points from an ellipsoid.
 *
 * @param unit_log_fn Function to compute the log-likelihood of a proposal.
 * @param criterion The minimum log-likelihood required for acceptance.
 * @param axes Matrix specifying the axes of the ellipsoid.
 * @param loc Vector specifying the location to shift the sampled point.
 * @param max_loop Maximum number of sampling attempts.
 * @return cpp11::list containing the accepted proposal, its log-likelihood, and
 * the number of calls, or only the number of calls if no proposal meets the
 * criterion.
 */
[[cpp11::register]]
cpp11::list EllipsoidImpl(cpp11::function unit_log_fn, double criterion,
                          cpp11::doubles_matrix<> axes, cpp11::doubles loc,
                          int max_loop) {
  cpp11::writable::doubles proposal(axes.nrow());
  random_vector::RandomEngine rng;
  for (int i = 0; i < max_loop; i++) {
    random_vector::UniformInEllipsoid(axes, proposal);
    lapack_wrapper::daxpy(1.0, loc, proposal);
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
