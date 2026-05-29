// File: /Users/ksnap/Projects/ernest/src/propose.cpp
// Created Date: Friday, October 24th 2025
// Author: Kyle Dewsnap
//
// Copyright (c) 2025 Kyle Dewsnap
// GNU General Public License v3.0 or later
// https://www.gnu.org/licenses/gpl-3.0-standalone.html
//
// Implements proposal mechanisms for MCMC sampling within nested sampling.
// These are called from R.
#include "rectangle.h"

// Runs a basic Random Walk Metropolis-Hastings sampler with fixed step size.
// Proposal: X' ~ N(X_n, ε I).
[[cpp11::register]]
cpp11::list RandomWalkImpl(cpp11::doubles original, cpp11::function unit_log_fn,
                           double criterion, int steps, double epsilon) {
  const int nvar = original.size();
  ern::RandomEngine rng;

  // Setups
  ern::Vector next_draw(nvar), rand_vec(nvar);
  ern::Vector prev_draw = as_Matrix(original);
  size_t n_accept = 0;

  for (size_t draw = 0; draw < steps; draw++) {
    next_draw = prev_draw;
    ern::UniformInBall(rand_vec, epsilon);
    next_draw += rand_vec;
    if (ern::IsOutsideUnitCube(next_draw)) {
      ern::ReflectWithinUnitCube(next_draw);
    }
    double log_lik = unit_log_fn(as_doubles(next_draw));
    if (log_lik >= criterion) {
      prev_draw = next_draw;
      n_accept++;
    }
  }

  using namespace cpp11::literals;
  return cpp11::writable::list({"unit"_nm = as_doubles(prev_draw),
                                "log_lik"_nm = unit_log_fn(as_doubles(prev_draw)),
                                "neval"_nm = steps, "n_accept"_nm = n_accept});
}

// Runs a slice sampler within an initial hyperrectangle.
[[cpp11::register]]
cpp11::list SliceImpl(cpp11::doubles original, cpp11::function unit_log_fn,
                      double criterion, int steps, const int max_loop) {
  // Setups
  vol::Rectangle rect(original.size());
  Eigen::VectorXd next_draw = as_Matrix(original);
  Eigen::VectorXd inner = next_draw;

  size_t draw = 1;
  for (; draw < max_loop; draw++) {
    rect.UniformSample(next_draw);
    double log_lik = unit_log_fn(as_doubles(next_draw));
    if (log_lik >= criterion) break;
    if (!rect.Clamp(inner, next_draw)) break;
  }

  using namespace cpp11::literals;
  return cpp11::writable::list({"unit"_nm = as_doubles(next_draw),
                                "log_lik"_nm = unit_log_fn(as_doubles(next_draw)),
                                "neval"_nm = draw});
}

// Slice sampling with an additional check for the mahalnobis distance
// between the current point and the previous point.
[[cpp11::register]]
cpp11::list SliceImplAdaptive(cpp11::doubles original, cpp11::function unit_log_fn,
                              double criterion, int steps, const int max_loop,
                              cpp11::doubles_matrix<> whitening) {
  cpp11::writable::list sample =
      SliceImpl(original, unit_log_fn, criterion, steps, max_loop);

  // Mahalanobis distance between original and new point.
  Eigen::VectorXd diff = as_Matrix(cpp11::as_doubles(sample["unit"]));
  diff -= as_Matrix(original);
  double dist = sqrt(diff.transpose() * as_Matrix(whitening) * diff);

  using namespace cpp11::literals;
  sample.push_back("distance"_nm = dist);
  return sample;
}
