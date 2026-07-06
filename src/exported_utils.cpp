#include <cpp11.hpp>

#include "Rmath.h"

[[cpp11::register]]
double logspace_add_c(const double x, const double y) {
  return Rf_logspace_add(x, y);
}

// Plateau detection in log-likelihoods to correct log-volume estimation
// based on Fowlie et. al (2021) https://doi.org/10.1093/mnras/stab590
[[cpp11::register]]
<<<<<<< HEAD
cpp11::integers get_points(cpp11::doubles log_lik, int nlive, bool add_live) {
  cpp11::writable::integers result(log_lik.size());
  auto col = result.begin();
  auto cur_lik = log_lik.begin();
  auto next_lik = log_lik.begin();
  auto last_dead = result.begin();
  std::advance(last_dead, add_live ? log_lik.size() - nlive : log_lik.size());
=======
cpp11::doubles get_points(cpp11::doubles log_lik, int init_nlive) {
  size_t n = log_lik.size();
>>>>>>> main
  int plateau = 0;
  cpp11::writable::doubles result(n);
  for (size_t i = 0; i < n; ++i) {
    result[i] = std::max(init_nlive - plateau, 1);  // Protect against zero nlive
    plateau = (i < n - 1 && log_lik[i] == log_lik[i + 1]) ? plateau + 1 : 0;
  }
  return result;
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
