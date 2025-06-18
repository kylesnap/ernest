#include <cpp11.hpp>
#include <Rmath.h>
#include <numeric>
#include <iostream>

[[cpp11::register]]
double logaddexp(double x, double y) {
  return Rf_logspace_add(x, y);
}

[[cpp11::register]]
cpp11::doubles logaddexp_vec(cpp11::doubles x, cpp11::doubles y) {
  if (x.size() != y.size()) {
    cpp11::stop("Vectors must be of the same length");
  }

  cpp11::writable::doubles result(x.size());
  for (size_t i = 0; i < x.size(); ++i) {
    result[i] = Rf_logspace_add(x[i], y[i]);
  }

  return result;
}

[[cpp11::register]]
cpp11::doubles logcumsumexp(cpp11::doubles x) {
  cpp11::writable::doubles result(x.size());
  result[0] = x[0];
  for (size_t i = 1; i < x.size(); ++i) {
    result[i] = Rf_logspace_add(result[i - 1], x[i]);
  }
  return result;
}
