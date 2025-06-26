#include <cpp11.hpp>
#include "R_ext/Random.h"

#include <iostream>
#include "RandomData.hpp"

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
  RandomData::RNGScopeGuard rng_guard;
  cpp11::writable::doubles_matrix<> out(criteria.size(), num_dim); 
  cpp11::writable::doubles_matrix<> nxt(criteria.size(), num_dim);
  cpp11::writable::doubles nxt_lik(criteria.size());
  std::vector<bool> is_valid(criteria.size(), false);
  int n_call = 0;

  while (n_call < max_loop) {
    // Generate a new set of random points in the unit cube.
    RandomData::uniform_in_cube(nxt);
    for (int i = 0; i < criteria.size(); ++i) {
      if (is_valid[i]) {
        continue;
      }
      cpp11::writable::doubles row(num_dim);
      for (int j = 0; j < num_dim; ++j) {
        row[j] = static_cast<double>(nxt(i, j));
      }
      double log_lik = unit_log_lik(row);
      n_call++;
      if (log_lik > criteria[i]) {
        // If the log-likelihood exceeds the criterion, accept the sample.
        is_valid[i] = true;
        nxt_lik[i] = log_lik;
        for (int j = 0; j < num_dim; ++j) {
          out(i, j) = static_cast<double>(nxt(i, j));
        }
      }
    }
    if (std::find(is_valid.begin(), is_valid.end(), false) == is_valid.end()) {
      break;
    }
  }

  if (n_call >= max_loop) {
    cpp11::stop("Maximum number of attempts (%d) reached without finding valid samples.", max_loop);
  }

  using namespace cpp11::literals;
  return cpp11::writable::list({
    "unit"_nm = out,
    "n_call"_nm = n_call,
    "log_lik"_nm = nxt_lik
  });
}
