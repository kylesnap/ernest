#include "random_vector.h"

/**
 * @brief LRPS using points in the unit hypercube.
 *
 * @param n_dim Length of parameter vector.
 * @param unit_log_fn Function to compute the log-likelihood of a proposal.
 * @param criterion The minimum log-likelihood required for acceptance.
 * @param max_loop Maximum number of sampling attempts.
 * @return cpp11::list containing the accepted proposal, its log-likelihood, and
 * the number of calls, or only the number of calls if no proposal meets the
 * criterion.
 */
[[cpp11::register]]
cpp11::list CubeImpl(int n_dim, cpp11::function unit_log_fn, double criterion,
                     int max_loop) {
  cpp11::writable::doubles proposal(n_dim);
  random_vector::RandomEngine rng;
  for (int i = 0; i < max_loop; i++) {
    random_vector::RUnif(proposal);
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