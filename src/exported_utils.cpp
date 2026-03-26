#include <cpp11.hpp>

#include "Rmath.h"

[[cpp11::register]]
double logspace_add_c(const double x, const double y) {
  return Rf_logspace_add(x, y);
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
