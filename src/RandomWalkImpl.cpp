#include "random_vector.h"

/**
 * @brief LRPS with the random walk Metropolis algorithm with reflection.
 *
 * @param original Initial point for the random walk (vector of doubles).
 * @param unit_log_fn Function that computes the log-likelihood.
 * @param criterion Minimum log-likelihood value required for acceptance.
 * @param steps Number of random walk steps to perform.
 * @param epsilon Scaling factor for the proposal step size.
 * @return cpp11::list containing the accepted proposal, its log-likelihood, the
 * number of calls and acceptances.
 */
[[cpp11::register]]
cpp11::list RandomWalkImpl(cpp11::doubles original, cpp11::function unit_log_fn,
                           double criterion, int steps, double epsilon) {
  const int n_dim = original.size();
  random_vector::RandomEngine rng;

  // Setups
  Eigen::RowVectorXd next_draw(n_dim), rand_vec(n_dim);
  Eigen::RowVectorXd prev_draw = wrap::as_eigen(original);
  size_t n_accept = 0;

  for (size_t draw = 0; draw < steps; draw++) {
    next_draw = prev_draw;
    random_vector::UniformInBall(rand_vec, epsilon);
    next_draw += rand_vec;
    if (random_vector::IsOutsideUnitCube(next_draw)) {
      random_vector::ReflectWithinUnitCube(next_draw);
    }
    double log_lik = unit_log_fn(wrap::as_doubles(next_draw));
    if (log_lik >= criterion) {
      prev_draw = next_draw;
      n_accept++;
    }
  }

  using namespace cpp11::literals;
  return cpp11::writable::list(
      {"unit"_nm = wrap::as_doubles(prev_draw),
       "log_lik"_nm = unit_log_fn(wrap::as_doubles(prev_draw)),
       "n_call"_nm = steps, "n_accept"_nm = n_accept});
}