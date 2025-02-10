#pragma once

#include <Rcpp.h>

namespace Ernest {

// Uniform cube
Rcpp::NumericVector runif_cube(int n);
void runif_cube(Rcpp::NumericVector &vec);

// Uniform sphere
Rcpp::NumericVector runif_sphere(int n);
bool offset_sphere(Rcpp::NumericVector dest, Rcpp::NumericVector src, const double epsilon);

} // namespace Ernest
