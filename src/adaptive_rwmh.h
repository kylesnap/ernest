#include "random_generator.h"

namespace ernest {
const double kSqrtPiDiv2 = 1.25331413731550025120788264241;
const double kLn3 = 1.09861228866810969139524523692;

inline Matrix outer(const ConstRef<RowVector> x) { return x.transpose() * x; }

class AdaptiveRWMH {
 public:
  AdaptiveRWMH(cpp11::doubles mean, cpp11::doubles_matrix<> cov,
               const double target_acceptance, const double prior_strength,
               const double forgetfulness, const double epsilon_init,
               const double epsilon_min);
  void Run(cpp11::doubles original, cpp11::function unit_log_fn,
           double criterion, int steps);

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
  size_t n_dim_;
  RowVector next_draw_, cur_draw_, old_draw_, rand_vec_;
  random_generator::RandomEngine rng();

  // Proposal distribution
  Eigen::RowVectorXd mean_;
  Eigen::MatrixXd cov_;
  double epsilon_cur_;
  double epsilon_init_;
  double epsilon_min_;

  // Proposal updating
  double epsilon_scaling_;
  double cov_scaling_;
  double prior_strength_;
  double target_acceptance_;
  double forgetfulness_;

  // Run paces
  int steps_ = 0;
  int accept_ = 0;

  inline int ForgetSeq(int step) {
    return static_cast<int>(std::floor(forgetfulness_ * step));
  }
  inline void UpdateFirstStep(cpp11::doubles original) {
    RowVector orig = as_row_vector(original);
    mean_ = 0.5 * (cur_draw_ + orig);
    cov_ *= (prior_strength_ + n_dim_ + 1);
    cov_ += (outer(cur_draw_) + outer(orig)) - (2 * outer(mean_));
    cov_ /= (prior_strength_ + n_dim_ + 3);
    old_draw_ = cur_draw_;
  }
  void IncludeObservation();
  inline void ReplaceObservation();

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
};  // namespace ernest
