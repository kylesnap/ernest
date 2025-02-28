#include "utils.h"

// [[Rcpp::export]]
double logaddexp(double x, double y) {
  return R::logspace_add(x, y);
}

// PURPOSE: Uniformly sample from unit cube (one vertex on origin)
// IN: n, the number of dimensions
// OUT: A point within the cube
Rcpp::NumericVector Ernest::runif_cube(int n) {
  return Rcpp::runif(n, 0, 1);
}

// PURPOSE: Replace `vec` with uniform sample from unit cube
// IN: &vec, the vector to replace
void Ernest::runif_cube(Rcpp::NumericVector &vec) {
  vec = std::move(runif_cube(vec.size()));
}

// PURPOSE: Uniformly sample within the unit sphere
// IN: n, the number of dimensions
// OUT: A point within the sphere
Rcpp::NumericVector Ernest::runif_sphere(int n) {
  Rcpp::NumericVector vec = Rcpp::rnorm(n, 0, 1);
  double norm = sqrt(Rcpp::sum(Rcpp::pow(vec, 2)));
  double u = pow(R::runif(0, 1), 1.0 / n) / norm;
  return vec * (u / norm);
}

// PURPOSE: Offset point `vec` by a point randomly selected from the unit sphere
// IN: &vec, the vector to offset, &epsilon, the shift length
bool Ernest::offset_sphere(Rcpp::NumericVector dest, Rcpp::NumericVector src, const double epsilon) {
  dest = src + epsilon * runif_sphere(src.size());
  return is_true(any(dest < 0)) || is_true(any(dest > 1));
}
