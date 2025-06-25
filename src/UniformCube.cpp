#include <cpp11.hpp>
#include "R_ext/Random.h"

#include <iostream>
#include "RandomData.hpp"

// Scope guard to ensure PutRNGstate() is always called
struct RNGScopeGuard {
  RNGScopeGuard() { GetRNGstate(); }
  ~RNGScopeGuard() { PutRNGstate(); }
};

/*
 * UniformCube
 * -----------
 * Generates samples from a unit hypercube, accepting only those samples
 * whose log-likelihood (as computed by the provided R function) exceeds
 * the corresponding criterion value.
 *
 * Args:
 *   criteria: Numeric vector of threshold values for log-likelihood.
 *   unit_log_lik: R function taking a vector and returning a log-likelihood.
 *   num_dim: Number of dimensions for each sample (length of each row).
 *   max_loop: Maximum number of attempts to find a valid sample for each criterion.
 *
 * Returns:
 *   A num_dim x length(criteria) matrix of accepted samples.
 *   The attribute "n_call" gives the total number of unit_log_lik calls.
 */
[[cpp11::register]]
cpp11::list UniformCube(const cpp11::doubles criteria,
                        cpp11::function unit_log_lik,
                        const int num_dim,
                        const int max_loop)  {
  RNGScopeGuard rng_guard;

  const std::size_t n_criteria = criteria.size();
  cpp11::writable::doubles_matrix<cpp11::by_row> out(num_dim, n_criteria);
  cpp11::writable::doubles out_lik(n_criteria);
  auto new_lik = out_lik.begin();
  auto out_ptr = REAL(out.data());
  cpp11::writable::doubles row(num_dim);
  int n_call = 0;

  for (const auto& criterion : criteria) {
    double *row_d = REAL(row.data());
    do {
      RandomData::unif_rand_n(num_dim, row_d);
      *new_lik = unit_log_lik(row);
      ++n_call;
    } while (*new_lik < criterion && n_call < max_loop);

    if (n_call > max_loop) {
      cpp11::stop("Maximum number of attempts (%d) exceeded for criterion %f", max_loop, criterion);
    }

    for (auto r : row) {
      *out_ptr = r;
      out_ptr++;
    }
    ++new_lik;
  }

  using namespace cpp11::literals;
  return cpp11::writable::list({
    "unit"_nm = out,
    "n_call"_nm = n_call,
    "log_lik"_nm = out_lik
  });
}
