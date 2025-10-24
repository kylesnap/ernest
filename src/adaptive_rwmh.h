// File: /Users/ksnap/Projects/ernest/src/adaptive_rwmh.h
// Created Date: Wednesday, October 15th 2025
// Author: Kyle Dewsnap
//
// Copyright (c) 2025 Kyle Dewsnap
// GNU General Public License v3.0 or later
// https://www.gnu.org/licenses/gpl-3.0-standalone.html
//
// Implements the adaptive random-walk Metropolis-Hastings (RWMH) sampler
// for nested sampling, following Spencer (2021).
#pragma once
#include "random_generator.h"

namespace ern {

const double kSqrtPiDiv2 = 1.25331413731550025120788264241;  // sqrt(pi/2)
const double kLn3 = 1.09861228866810969139524523692;         // ln(3)

// Computes the outer product of a row vector with itself.
static inline Matrix outer(const ConstRef<RowVector> x) {
  return x.transpose() * x;
}

// Adaptive Random Walk Metropolis-Hastings sampler with proposal tuning.
//
// Implements an adaptive RWMH algorithm that tunes its proposal distribution
// during sampling to achieve a target acceptance rate. The covariance matrix
// and step size are updated online using exponentially weighted observations.
class AdaptiveRWMH {
 public:
  // Constructor
  AdaptiveRWMH(cpp11::doubles mean, cpp11::doubles_matrix<> cov,
               const double target_acceptance, const double prior_strength,
               const double forgetfulness, const double epsilon_init,
               const double epsilon_min);

  // Run adaptive RWMH sampling for a specified number of steps.
  void Run(cpp11::doubles original, cpp11::function unit_log_fn,
           double criterion, int steps);

  // Converts the sampler state to an R list representation.
  inline const cpp11::list as_list(cpp11::function unit_log_fn) {
    using namespace cpp11::literals;
    return cpp11::writable::list(
        {"unit"_nm = as_row_doubles(cur_draw_),
         "log_lik"_nm = unit_log_fn(as_row_doubles(cur_draw_)),
         "n_call"_nm = steps_, "n_accept"_nm = accept_,
         "epsilon"_nm = epsilon_cur_, "mean"_nm = mean_,
         "cov"_nm = as_doubles_matrix(cov_)});
  };

 private:
  size_t n_dim_;  // Number of dimensions.
  RowVector next_draw_, cur_draw_, old_draw_, rand_vec_;  // Draw states.
  ern::RandomEngine rng;  // Random number generator.

  // Proposal distribution parameters.
  Eigen::RowVectorXd mean_;  // Current mean estimate.
  Eigen::MatrixXd cov_;      // Current covariance estimate.
  double epsilon_cur_;       // Current scaling factor.
  double epsilon_init_;      // Initial scaling factor.
  double epsilon_min_;       // Minimum scaling factor.

  // Adaptation parameters.
  double epsilon_scaling_;    // Rate of epsilon adjustment.
  double cov_scaling_;        // Rate of covariance adjustment.
  double prior_strength_;     // Prior strength on covariance.
  double target_acceptance_;  // Target acceptance rate.
  double forgetfulness_;      // Exponential forgetting rate.

  // Sampling statistics.
  int steps_ = 0;   // Total number of steps taken.
  int accept_ = 0;  // Number of accepted proposals.

  // Computes the forgetting sequence step index.
  inline int ForgetSeq(int step) {
    return static_cast<int>(std::floor(forgetfulness_ * step));
  }

  // Updates proposal distribution after the first step.
  inline void UpdateFirstStep(cpp11::doubles original) {
    RowVector orig = as_row_vector(original);
    mean_ = 0.5 * (cur_draw_ + orig);
    cov_ *= (prior_strength_ + n_dim_ + 1);
    cov_ += (outer(cur_draw_) + outer(orig)) - (2 * outer(mean_));
    cov_ /= (prior_strength_ + n_dim_ + 3);
    old_draw_ = cur_draw_;
  }

  // Incorporates a new observation into the proposal distribution.
  void IncludeObservation();

  // Replaces an old observation in the proposal distribution.
  inline void ReplaceObservation();

  // Updates the scaling factor epsilon based on acceptance rate.
  inline void UpdateScaling(bool accept, size_t step) {
    double target_scale = target_acceptance_ * epsilon_cur_;
    target_scale = accept ? (epsilon_cur_ - target_scale) : -1.0 * target_scale;
    epsilon_cur_ *= exp(target_scale / step);
    epsilon_cur_ = fmax2(epsilon_min_, epsilon_cur_);
    if (fabs(log(epsilon_cur_ / epsilon_init_)) > kLn3) {
      epsilon_cur_ = epsilon_init_;
    }
  }
};

};  // namespace ern
