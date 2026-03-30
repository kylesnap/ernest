#include <cpp11.hpp>

#include "Rmath.h"

[[cpp11::register]]
double logspace_add_c(const double x, const double y) {
  return Rf_logspace_add(x, y);
}

// Plateau detection in log-likelihoods to correct log-volume estimation
// based on Fowlie et. al (2021) https://doi.org/10.1093/mnras/stab590
[[cpp11::register]]
cpp11::doubles get_points(cpp11::doubles log_lik, int nlive) {
  cpp11::writable::doubles result(log_lik.size());
  auto col = result.begin();
  auto cur_lik = log_lik.begin();
  auto next_lik = log_lik.begin();
  auto last_dead = result.begin();
  std::advance(last_dead, log_lik.size() - nlive);
  int plateau = 0;
  for (std::advance(next_lik, 1); col != last_dead; ++col, ++cur_lik, ++next_lik) {
    *col = nlive - plateau;
    plateau = (*cur_lik == *next_lik) ? plateau + 1 : 0;
  }
  int nlive_remaining = nlive;
  for (; col != result.end(); ++col, --nlive_remaining) {
    *col = nlive_remaining;
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
