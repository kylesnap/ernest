#include <cpp11.hpp>

#include "Rmath.h"

[[cpp11::register]]
double logspace_add_c(const double x, const double y) {
  return Rf_logspace_add(x, y);
}

// Plateau detection in log-likelihoods to correct log-volume estimation
// based on Fowlie et. al (2021) https://doi.org/10.1093/mnras/stab590
[[cpp11::register]]
cpp11::doubles get_points(cpp11::doubles log_lik, int init_nlive) {
  size_t n = log_lik.size();
  int plateau = 0;
  cpp11::writable::doubles result(n);
  for (size_t i = 0; i < n; ++i) {
    result[i] = std::max(init_nlive - plateau, 1);  // Protect against zero nlive
    plateau = (i < n - 1 && log_lik[i] == log_lik[i + 1]) ? plateau + 1 : 0;
  }
  return result;
}

[[cpp11::register]]
cpp11::list get_log_w_cpp(cpp11::doubles_matrix<> log_lik,
                          cpp11::doubles_matrix<> log_volume) {
  const int draws = log_lik.nrow();
  const int iter = log_lik.ncol();
  cpp11::writable::doubles_matrix<> log_weight(draws, iter);
  cpp11::writable::doubles_matrix<> log_z(draws, iter);

  auto logspace_qadd = [](double logx, double logy) {
    double diff = logx - logy;
    if (diff > 35) {
      return logx;
    } else if (diff < -35) {
      return logy;
    } else {
      return logspace_add(logx, logy);
    }
  };

  for (int i = 0; i < draws; ++i) {
    double avg = logspace_qadd(log_volume(i, 0), log_volume(i, 1)) - M_LN2;
    double w = logspace_sub(0.0, avg) + log_lik(i, 0);
    log_weight(i, 0) = w;
    double cur_log_z = w;
    log_z(i, 0) = cur_log_z;
    for (int j = 1; j < iter - 1; ++j) {
      w = logspace_sub(log_volume(i, j - 1), log_volume(i, j + 1)) - M_LN2;
      w += log_lik(i, j);
      log_weight(i, j) = w;
      cur_log_z = logspace_qadd(cur_log_z, w);
      log_z(i, j) = cur_log_z;
    }
    w = logspace_qadd(log_volume(i, iter - 2), log_volume(i, iter - 1)) - M_LN2;
    w += log_lik(i, iter - 1);
    log_weight(i, iter - 1) = w;
    cur_log_z = logspace_qadd(cur_log_z, w);
    log_z(i, iter - 1) = cur_log_z;
  }
  using namespace cpp11::literals;
  return cpp11::list({"log_weight"_nm = log_weight, "log_z"_nm = log_z});
}

[[cpp11::register]]
cpp11::doubles_matrix<cpp11::by_row> logspace_cumsum_mat(
    cpp11::doubles_matrix<cpp11::by_row> x) {
  cpp11::writable::doubles_matrix<cpp11::by_row> result(x.nrow(), x.ncol());
  for (int row = 0; row < x.nrow(); ++row) {
    double cumsum = x[row][0];
    result[row][0] = cumsum;
    for (int col = 1; col < x.ncol(); ++col) {
      cumsum = Rf_logspace_add(cumsum, x[row][col]);
      result[row][col] = cumsum;
    }
  }
  return result;
}
