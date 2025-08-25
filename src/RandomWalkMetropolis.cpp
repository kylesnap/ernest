#include <cpp11.hpp>
#include "Rmath.h"
#include "R_ext/Random.h"
#include "RandomData.h"
#include <iostream>
#include <algorithm>

/*
 * RandomWalkMetropolis
 * --------------------
 * Performs a random walk Metropolis algorithm on a set of points.
 * For each row in the input matrix, proposes new points within a sphere
 * and accepts them if the log-likelihood exceeds the given criterion.
 *
 * Args:
 *   unit: Initial matrix of points (each row is a point).
 *   criteria: Vector of log-likelihood thresholds, one per row.
 *   unit_log_lik: R function to compute log-likelihood for a point.
 *   steps: Number of Metropolis steps per row.
 *   epsilon: Proposal radius for the random walk.
 *
 * Returns:
 *   Matrix of updated points (same shape as input).
 *   Attributes:
 *     "n_call": total number of log-likelihood evaluations.
 *     "n_accept": total number of accepted proposals.
 */
[[cpp11::register]]
cpp11::list RandomWalkMetropolis(const cpp11::doubles_matrix<> unit,
                                 const cpp11::doubles criteria,
                                 cpp11::function unit_log_lik,
                                 const int num_dim,
                                 const int steps,
                                 const double epsilon)  {
  cpp11::writable::doubles_matrix<> cur(criteria.size(), num_dim);
  cpp11::writable::doubles_matrix<> nxt(criteria.size(), num_dim);
  cpp11::writable::doubles_matrix<> shift(criteria.size(), num_dim);
  cpp11::writable::doubles cur_lik(criteria);
  std::vector<bool> swapped(criteria.size(), false);
  int n_call = 0;
  int n_accept = 0;

  int num_el = criteria.size() * num_dim;
  int inc = 1;
  // Copy the original points into the cur matrix.
  dcopy_(&num_el, REAL(unit.data()), &inc, REAL(cur.data()), &inc);

  GetRNGstate();
  for (int s = 0; s < steps; ++s) {
    RandomData::uniform_in_sphere(shift);
    dcopy_(&num_el, REAL(cur.data()), &inc, REAL(nxt.data()), &inc);
    daxpy_(&num_el, &epsilon, REAL(shift.data()), &inc, REAL(nxt.data()), &inc);

    for (int i = 0; i < criteria.size(); ++i) {
      cpp11::writable::doubles row(num_dim);
      bool invalid = false;
      for (int j = 0; j < num_dim; ++j) {
        row[j] = static_cast<double>(nxt(i, j));
        invalid = invalid || (row[j] < 0.0 || row[j] > 1.0);
      }
      n_call++;
      if (invalid) continue;
      double log_lik = unit_log_lik(row);
      if (log_lik > criteria[i]) {
        // If the log-likelihood exceeds the criterion, accept the sample.
        swapped[i] = true;
        cur_lik[i] = log_lik;
        n_accept++;
        for (int j = 0; j < num_dim; ++j) {
          cur(i, j) = static_cast<double>(nxt(i, j));
        }
      }
    }
  }
  PutRNGstate();
  for (int i = 0; i < criteria.size(); ++i) {
    if (!swapped[i]) {
      // If no swap occurred, keep the original point.
      cur_lik[i] = NA_REAL;
    }
  }

  using namespace cpp11::literals;
  return cpp11::writable::list({
    "unit"_nm = cur,
    "n_call"_nm = n_call,
    "n_accept"_nm = n_accept,
    "log_lik"_nm = cur_lik
  });
}
