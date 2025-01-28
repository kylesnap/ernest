#include <cpp11.hpp>
#include <Rmath.h>

[[cpp11::register]]
double logaddexp(double x, double y) {
  return logspace_add(x, y);
}
