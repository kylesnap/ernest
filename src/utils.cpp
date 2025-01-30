#include <Rcpp.h>

// [[Rcpp::export]]
double logaddexp(double x, double y) {
  return R::logspace_add(x, y);
}

