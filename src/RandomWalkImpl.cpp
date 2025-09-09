#include "random_vector.h"

/**
 * @brief LRPS with the random walk Metropolis algorithm with reflection.
 *
 * @param original Initial point for the random walk (vector of doubles).
 * @param log_lik_fn Function that computes the log-likelihood.
 * @param criterion Minimum log-likelihood value required for acceptance.
 * @param steps Number of random walk steps to perform.
 * @param epsilon Scaling factor for the proposal step size.
 * @param chol_cov Cholesky factor of the covariance matrix for proposal
 * generation.
 * @return cpp11::list containing the accepted proposal, its log-likelihood, the
 * number of calls and acceptances.
 */
[[cpp11::register]]
cpp11::list RandomWalkImpl(cpp11::doubles original, cpp11::function log_lik_fn,
                           double criterion, int steps, double epsilon,
                           cpp11::doubles_matrix<> chol_cov) {
  const int n_dim = original.size();
  random_vector::RandomEngine rng;

  // Setups
  cpp11::writable::doubles prev_draw(original);
  cpp11::writable::doubles next_draw(n_dim);
  cpp11::writable::doubles rand_vec(n_dim);
  size_t n_accept = 0;

  for (size_t draw = 0; draw < steps; draw++) {
    lapack_wrapper::dcopy(prev_draw, next_draw);
    random_vector::RNorm(rand_vec);
    lapack_wrapper::dsymv(epsilon, chol_cov, rand_vec, 1.0, next_draw);
    if (random_vector::IsOutsideUnitCube(next_draw)) {
      random_vector::ReflectWithinUnitCube(next_draw);
    }
    double log_lik = log_lik_fn(next_draw);
    if (log_lik >= criterion) {
      lapack_wrapper::dcopy(next_draw, prev_draw);
      n_accept++;
    }
  }
  using namespace cpp11::literals;
  return cpp11::writable::list({"unit"_nm = prev_draw,
                                "log_lik"_nm = log_lik_fn(prev_draw),
                                "n_call"_nm = steps, "n_accept"_nm = n_accept});
}
