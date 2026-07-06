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
                           double criterion, unsigned int steps, double epsilon) {
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
<<<<<<< HEAD
                      double criterion, int steps, const int max_loop) {
  // Setup
  const size_t nvar = original.size();
=======
                      double criterion, cpp11::doubles lower, cpp11::doubles upper,
                      unsigned int max_loop) {
  // Setups
  vol::Rectangle rect(lower, upper);
>>>>>>> main
  Eigen::VectorXd next_draw = as_Matrix(original);
  Eigen::VectorXd inner(nvar);
  vol::Rectangle rect(nvar);

  // Helper for evaluating the log-likelihood
  cpp11::writable::doubles x(nvar);
  double* x_ptr = REAL(x);
  auto eval = [&](const Eigen::VectorXd& v) {
    std::copy(v.data(), v.data() + nvar, x_ptr);
    return static_cast<double>(unit_log_fn(x));
  };

  double log_lik = R_NegInf;
  int neval = 0;
  for (int draw = 0; draw < steps && neval < max_loop; ++draw) {
    inner = next_draw;
    rect.Clear();
    while (neval < max_loop) {
      ++neval;
      rect.UniformSample(next_draw);
      if ((log_lik = eval(next_draw)) >= criterion) break;
      if (!rect.Clamp(inner, next_draw)) break;
    }
  }

  using namespace cpp11::literals;
  return cpp11::writable::list(
      {"unit"_nm = x, "log_lik"_nm = log_lik, "neval"_nm = neval});
}
