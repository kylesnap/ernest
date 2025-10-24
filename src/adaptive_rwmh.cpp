// File: /Users/ksnap/Projects/ernest/src/adaptive_rwmh.cpp
// Created Date: Tuesday, October 14th 2025
// Author: Kyle Dewsnap
//
// Copyright (c) 2025 Kyle Dewsnap
// GNU General Public License v3.0 or later
// https://www.gnu.org/licenses/gpl-3.0-standalone.html
//
// Implements the adaptive random-walk Metropolis-Hastings (RWMH) sampler
// for nested sampling, following Spencer (2021).

#include "adaptive_rwmh.h"

using namespace ern;

AdaptiveRWMH::AdaptiveRWMH(cpp11::doubles mean, cpp11::doubles_matrix<> cov,
                           const double target_acceptance,
                           const double prior_strength,
                           const double forgetfulness,
                           const double epsilon_init, const double epsilon_min)
    : n_dim_(mean.size()),
      next_draw_(n_dim_),
      cur_draw_(n_dim_),
      old_draw_(n_dim_),
      rand_vec_(n_dim_),
      mean_(as_row_vector(mean)),
      cov_(as_Matrix(cov)),
      epsilon_cur_(epsilon_init),
      epsilon_init_(epsilon_init),
      epsilon_min_(epsilon_min),
      cov_scaling_(R_pow_di(2.38, 2) / n_dim_),
      prior_strength_(prior_strength),
      target_acceptance_(target_acceptance),
      forgetfulness_(forgetfulness) {
  // A = Φ^(-1)(target_acceptance / 2).
  double A = -Rf_qnorm5(target_acceptance_, 0.0, 1.0, 1, 0);

  // Robbins-Munro scaling rate
  double first_term = 1.0 - 1.0 / n_dim_;
  double second_term = (1.0 / A) * kSqrtPiDiv2 * std::exp(A * A / 2.0);
  double third_term =
      1.0 / (n_dim_ * target_acceptance_ * (1.0 - target_acceptance_));
  epsilon_scaling_ = first_term * second_term + third_term;
}

// Includes a new observation in the covariance estimate.
void AdaptiveRWMH::IncludeObservation() {
  // Eq. 3 in Spencer 2021
  int f_n = ForgetSeq(steps_);
  double weight = 1.0 / (steps_ - f_n + 1);

  // Update mean: μ_n.
  Eigen::MatrixXd prev_mean_outer = outer(mean_);
  mean_ = ((steps_ - f_n) * weight) * mean_ + weight * cur_draw_;

  // Update covariance using Bayesian posterior mode with
  // normal-inverse-Wishart.
  Eigen::MatrixXd cur_outer = outer(cur_draw_);
  Eigen::MatrixXd mean_outer = outer(mean_);
  cov_ = (1.0 / (steps_ - f_n + prior_strength_ + n_dim_ + 2)) *
         ((steps_ - f_n + prior_strength_ + n_dim_ + 1) * cov_ + cur_outer +
          (steps_ - f_n) * prev_mean_outer - (steps_ - f_n + 1) * mean_outer);
}

void AdaptiveRWMH::ReplaceObservation() {
  // Eq. 4 in Spencer 2021
  int f_n = ForgetSeq(steps_);
  double weight = 1.0 / (steps_ - f_n + 1);

  // Update mean: μ_n = μ_{n-1} + w_n * (X_n - X_{f(n)}).
  Eigen::MatrixXd diff_mean = outer(mean_);
  mean_ += weight * (cur_draw_ - old_draw_);
  diff_mean -= outer(mean_);

  // Update covariance by replacing the oldest point.
  Eigen::MatrixXd diff_draw = outer(cur_draw_) - outer(old_draw_);
  cov_ += (1.0 / (steps_ - f_n + prior_strength_ + n_dim_ + 2)) *
          (diff_draw + (steps_ - f_n + 1) * diff_mean);
  old_draw_ = cur_draw_;
}

void AdaptiveRWMH::Run(cpp11::doubles original, cpp11::function unit_log_fn,
                       double criterion, int steps) {
  cur_draw_ = as_row_vector(original);
  bool accept;

  for (steps_ = 1; steps_ < steps; steps_++) {
    next_draw_ = cur_draw_;
    // Generate proposal: X' = X_n + L_n * Z, where Z ~ N(0, I) and
    // L_n L_n' = ε_n * c_n * Σ_n
    ern::RNorm(rand_vec_);
    Eigen::LLT<Eigen::MatrixXd> llt(epsilon_cur_ * cov_scaling_ * cov_);
    next_draw_ += rand_vec_ * llt.matrixL();
    if (ern::IsOutsideUnitCube(next_draw_)) {
      accept = false;
    } else {
      double log_lik = unit_log_fn(as_row_doubles(next_draw_));
      accept = log_lik >= criterion;
    }
    if (accept) {
      cur_draw_ = next_draw_;
      accept_++;
    }
    // Special handling for first iteration to initialize covariance estimate.
    if (steps_ == 1) {
      UpdateFirstStep(original);
      UpdateScaling(accept, steps);
      continue;
    }
    int f_n = ForgetSeq(steps);
    int f_n_prev = ForgetSeq(steps - 1);
    if (f_n == f_n_prev) {
      IncludeObservation();
    } else if (f_n == f_n_prev + 1) {
      ReplaceObservation();
    }
    // Update scaling factor ε_n using Robbins-Munro algorithm (Eq. 5)
    UpdateScaling(accept, steps);
  }
}