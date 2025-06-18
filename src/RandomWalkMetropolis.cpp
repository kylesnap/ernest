#include <cpp11.hpp>
#include "Rmath.h"
#include "R_ext/Random.h"
#include <iostream>
#include <algorithm>

// Scope guard to ensure PutRNGstate() is always called
struct RNGScopeGuard {
  RNGScopeGuard() { GetRNGstate(); }
  ~RNGScopeGuard() { PutRNGstate(); }
};

cpp11::writable::doubles point_in_sphere(int m) {
  double exponent = 1.0 / m;
  double norm = 0.0;

  cpp11::writable::doubles x(m);
  // Fill vector with uniformly distributed values
  for (auto val : x) {
    val = norm_rand();
    norm += val * val;
  }
  norm = sqrt(norm);
  double radius = pow(unif_rand(), exponent);
  for (auto val : x) {
    val /= norm;
    val *= radius;
  }
  return x;
}

/*
 * runif_in_sphere
 * ---------------
 * Generates a random point uniformly within a sphere of given radius,
 * modifies the input vector in-place, and returns true if the point is valid.
 * Returns false if any coordinate is outside [0, 1] after normalization.
 *
 * Args:
 *   vec: Vector to be filled with the random point.
 *   radius: Radius of the sphere.
 *
 * Returns:
 *   true if the generated point is valid, false otherwise.
 */
bool shift_in_sphere(cpp11::writable::doubles &vec, double radius) {
  cpp11::writable::doubles shift = point_in_sphere(vec.size());
  for (size_t i = 0; i < vec.size(); ++i) {
    vec[i] = vec[i] + (radius * shift[i]);
    if (vec[i] < 0 || vec[i] > 1) {
      return false;
    }
  }
  return true;
}

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
  RNGScopeGuard rng_guard;

  const std::size_t n_criteria = criteria.size();
  cpp11::writable::doubles_matrix<> out(num_dim, n_criteria);
  cpp11::writable::doubles out_lik;
  auto out_ptr = REAL(out.data());
  int n_call = 0;
  int n_accept = 0;

  for (std::size_t row = 0; row < criteria.size(); ++row) {
    cpp11::writable::doubles cur(num_dim);
    for (std::size_t col = 0; col < cur.size(); ++col) {
      cur[col] = static_cast<double>(unit(row, col));
    }

    double cur_lik = unit_log_lik(cur);

    for (std::size_t step = 0; step < steps; ++step) {
      ++n_call;
      cpp11::writable::doubles nxt(cur);
      if (!shift_in_sphere(nxt, epsilon)) {
        continue;
      }
      double nxt_lik = unit_log_lik(nxt);
      if (nxt_lik >= criteria[row]) {
        cur = std::move(nxt);
        cur_lik = nxt_lik;
        ++n_accept;
      }
    }

    for (auto c : cur) {
      *out_ptr = c;
      out_ptr++;
    }
    out_lik.push_back(cur_lik);
    ++cur_lik;
  }

  using namespace cpp11::literals;
  return cpp11::writable::list({
    "unit"_nm = out,
    "n_call"_nm = n_call,
    "n_accept"_nm = n_accept,
    "log_lik"_nm = out_lik
  });
}
