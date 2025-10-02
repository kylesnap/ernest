#include <Rmath.h>

#include <cpp11.hpp>
#include <numeric>

[[cpp11::register]]
double logaddexp(double x, double y) {
  return Rf_logspace_add(x, y);
}
