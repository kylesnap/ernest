#include "random_generator.h"

#define K_SQRT2d2_SQRTPI 1.25331413731550025120788264241
#define K_LN3 1.09861228866810969139524523692

struct AdaptiveState {
  Eigen::RowVectorXd mean;
  Eigen::MatrixXd cov;
  double start_epsilon;
  double epsilon;
  double min_epsilon;
  double epsilon_scaling;
  double cov_scaling;
  double prior_strength;
  double target_accept;
  double forgetfulness;
  size_t n_dim;
  size_t step_start;
};

/**
 * @brief Initializes the adaptive state for the accelerated scaling and shaping
 * algorithm.
 *
 * Sets up the initial parameters for the adaptive Metropolis-Hastings random
 * walk, including scaling factors, covariance matrix initialization, and
 * epsilon scaling parameter calculation based on the target acceptance rate.
 *
 * @param mean Initial mean vector for the proposal distribution
 * @param covariance Initial covariance matrix for the proposal distribution
 * @param target_accept Target acceptance rate for the MCMC chain
 * @param prior_strength Strength of prior information for covariance estimation
 * (usually number of points in the live set)
 * @param forgetfulness Parameter controlling the forgetting sequence for
 * covariance updates
 * @param epsilon Initial step-size parameter.
 * @param min_epsilon Minimum allowed value for the step-size parameter
 *
 * @return AdaptiveState Initialized state structure containing all adaptive
 * parameters
 */
AdaptiveState Initalize(cpp11::doubles& mean,
                        cpp11::doubles_matrix<>& covariance,
                        double target_accept, double prior_strength,
                        double forgetfulness, double epsilon,
                        double min_epsilon) {
  AdaptiveState state;
  state.mean = as_row_vector(mean);
  state.cov = as_Matrix(covariance);
  state.target_accept = target_accept;
  state.prior_strength = prior_strength;
  state.forgetfulness = forgetfulness;
  state.start_epsilon = state.epsilon = epsilon;
  state.min_epsilon = min_epsilon;

  int n_dim = mean.size();
  state.n_dim = n_dim;
  state.cov_scaling = R_pow_di(2.38, 2) / n_dim;
  state.step_start = 5 / (target_accept * (1 - target_accept));

  // Calculating the epsilon scaling parameter ('delta' in literature)
  double first_term = 1.0 - 1.0 / n_dim;
  double A = -Rf_qnorm5(target_accept, 0.0, 1.0, 1, 0);
  double second_term = (1.0 / A) * K_SQRT2d2_SQRTPI * std::exp(A * A / 2.0);
  double third_term = 1.0 / (n_dim * target_accept * (1.0 - target_accept));
  state.epsilon_scaling = first_term * second_term + third_term;

  return state;
}

/**
 * @brief Computes the forgetting sequence function.
 *
 * @param step Current iteration step number
 * @param forget Forgetfulness parameter (0 < forget <= 1)
 *
 * @return The forgetting sequence at the given step
 */
int ForgetSeq(int step, double forget) {
  return static_cast<int>(std::floor(forget * step));
}

/**
 * @brief Add a new observation to the current window of samples used for
 * covariance estimation.
 *
 * @param state Reference to the adaptive state to be updated
 * @param step Current iteration step number
 * @param cur_draw Current sample vector to be included in the estimates
 */
void IncludeObservation(AdaptiveState& state, int step,
                        const Eigen::Ref<const Eigen::RowVectorXd> cur_draw) {
  int f_n = ForgetSeq(step, state.forgetfulness);
  double weight = 1.0 / (step - f_n + 1);

  // Update mean
  Eigen::MatrixXd prev_mean_outer = random_generator::outer(state.mean);
  state.mean = ((step - f_n) * weight) * state.mean + weight * cur_draw;

  // Update covariance
  Eigen::MatrixXd cur_outer = random_generator::outer(cur_draw);
  Eigen::MatrixXd mean_outer = random_generator::outer(state.mean);
  state.cov =
      (1.0 / (step - f_n + state.prior_strength + state.n_dim + 2)) *
      ((step - f_n + state.prior_strength + state.n_dim + 1) * state.cov +
       cur_outer + (step - f_n) * prev_mean_outer -
       (step - f_n + 1) * mean_outer);
}

/**
 * @brief Updates covariance and mean estimates by replacing the oldest
 * observation.
 *
 * @param state Reference to the adaptive state to be updated
 * @param step Current iteration step number
 * @param cur_draw Current sample vector to be included
 * @param old_draw Oldest sample vector to be removed from the estimates
 */
void ReplaceObservation(AdaptiveState& state, int step,
                        const Eigen::Ref<const Eigen::RowVectorXd> cur_draw,
                        const Eigen::Ref<const Eigen::RowVectorXd> old_draw) {
  int f_n = ForgetSeq(step, state.forgetfulness);
  double weight = 1.0 / (step - f_n + 1);

  Eigen::MatrixXd diff_mean = random_generator::outer(state.mean);
  state.mean += weight * (cur_draw - old_draw);
  diff_mean -= random_generator::outer(state.mean);

  Eigen::MatrixXd diff_draw =
      random_generator::outer(cur_draw) - random_generator::outer(old_draw);
  state.cov += (1.0 / (step - f_n + state.prior_strength + state.n_dim + 2)) *
               (diff_draw + (step - f_n + 1) * diff_mean);
}

/**
 * @brief Updates the step-size scaling parameter epsilon based on acceptance,
 * with a safety check to ensure abs(epsilon) doesn't exceed log(3)
 *
 * @param state Reference to the adaptive state containing epsilon and target
 * acceptance
 * @param accept Boolean indicating whether the current proposal was accepted
 * @param step Current iteration step number
 */
void UpdateScaling(AdaptiveState& state, bool accept, size_t step) {
  double target_scale = state.target_accept * state.epsilon;
  target_scale = accept ? (state.epsilon - target_scale) : -1.0 * target_scale;
  state.epsilon *= exp(target_scale / step);
  state.epsilon = fmax2(state.min_epsilon, state.epsilon);
  if (fabs(log(state.epsilon)) > K_LN3) {
    state.epsilon = state.start_epsilon;
  }
}

/**
 * @brief Performs adaptive random walk Metropolis-Hastings sampling with
 * reflection.
 *
 * @param original Initial point for the random walk (vector of doubles).
 * @param unit_log_fn Function that computes the log-likelihood for proposals.
 * @param criterion Minimum log-likelihood value required for acceptance
 *                  (log-likelihood constraint).
 * @param steps Number of random walk steps to perform.
 * @param epsilon Initial step-size.
 * @param min_epsilon Minimum allowed step-size parameter to prevent degeneracy.
 * @param target_acceptance Target acceptance rate for optimal efficiency.
 * @param mean Initial mean vector for the proposal distribution.
 * @param covariance Initial covariance matrix estimate (e.g., from live
 * points).
 * @param strength Prior strength for covariance matrix (typically number of
 *                 parameters).
 * @param forgetfulness Forgetting parameter controlling memory length
 *                      (0 < forgetfulness <= 1).
 *
 * @return A list containing:
 *         - unit: Final accepted sample point
 *         - log_lik: Log-likelihood of the final point
 *         - n_call: Total number of function evaluations (equals steps)
 *         - n_accept: Number of accepted proposals
 *         - epsilon: Final adapted step-size parameter
 *         - mean: Final adapted mean vector
 *         - covariance: Final adapted covariance matrix
 */
[[cpp11::register]]
cpp11::list AdaptiveRWImpl(cpp11::doubles original, cpp11::function unit_log_fn,
                           double criterion, int steps, double epsilon,
                           double min_epsilon, double target_acceptance,
                           cpp11::doubles mean,
                           cpp11::doubles_matrix<> covariance, double strength,
                           double forgetfulness) {
  random_generator::RandomEngine rng;

  AdaptiveState state = Initalize(mean, covariance, target_acceptance, strength,
                                  forgetfulness, epsilon, min_epsilon);
  Eigen::RowVectorXd next_draw(state.n_dim), old_draw(state.n_dim),
      rand_vec(state.n_dim);
  Eigen::RowVectorXd cur_draw = as_row_vector(original);
  bool accept;
  size_t n_accept = 0;

  for (int step = 1; step <= steps; step++) {
    next_draw = cur_draw;
    random_generator::RNorm(rand_vec);
    Eigen::LLT<Eigen::MatrixXd> llt(state.epsilon * state.cov_scaling *
                                    state.cov);
    next_draw += rand_vec * llt.matrixL();
    if (random_generator::IsOutsideUnitCube(next_draw)) {
      accept = false;
    } else {
      double log_lik = unit_log_fn(as_row_doubles(next_draw));
      accept = log_lik >= criterion;
    }
    if (accept) {
      cur_draw = next_draw;
      n_accept++;
    }
    if (step == 1) {
      Eigen::RowVectorXd orig = as_row_vector(original);
      state.mean = 0.5 * (cur_draw + orig);
      state.cov *= (state.prior_strength + state.n_dim + 1);
      state.cov +=
          (random_generator::outer(cur_draw) + random_generator::outer(orig)) -
          (2 * random_generator::outer(state.mean));
      state.cov /= (state.prior_strength + state.n_dim + 3);
      old_draw = cur_draw;
      UpdateScaling(state, accept, step);
      continue;
    }
    int f_n = ForgetSeq(step, state.forgetfulness);
    int f_n_prev = ForgetSeq(step - 1, state.forgetfulness);
    if (f_n == f_n_prev) {
      IncludeObservation(state, step, cur_draw);
    } else if (f_n == f_n_prev + 1) {
      ReplaceObservation(state, step, cur_draw, old_draw);
      old_draw = cur_draw;
    }
    UpdateScaling(state, accept, step);
  }

  return cpp11::writable::list(
      {"unit"_nm = as_row_doubles(cur_draw),
       "log_lik"_nm = unit_log_fn(as_row_doubles(cur_draw)),
       "n_call"_nm = steps, "n_accept"_nm = n_accept,
       "epsilon"_nm = state.epsilon, "mean"_nm = state.mean,
       "covariance"_nm = as_doubles_matrix(state.cov)});
}