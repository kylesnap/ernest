// File: /Users/ksnap/Projects/ernest/src/update_lrps-impl.cpp
// Created Date: Tuesday, October 14th 2025
// Author: Kyle Dewsnap
//
// Copyright (c) 2025 Kyle Dewsnap
// GNU General Public License v3.0 or later
// https://www.gnu.org/licenses/gpl-3.0-standalone.html
//
// Implements functions to compute bounding volumes for sets of points.
// These are called from R.
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

#include "ellipsoid.h"
#include "mini_ball.h"

// Find the bounding ellipsoid of the given points in X. If X has no rows,
// then return the bounding unit sphere in the appropriate dimension.
[[cpp11::register]]
cpp11::list BoundingEllipsoid(cpp11::doubles_matrix<> X, double point_log_volume) {
  vol::Ellipsoid ell(X.ncol());
  if (X.nrow() > 0) {
    Eigen::MatrixXd X_eigen = as_Matrix(X);
    ell.Fit(X_eigen, point_log_volume);
  }
  return ell.as_list();
}

// Fit multiple bounding ellipsoids to the provided data points `X`,
// returning their R list representations along with their scaled sampling
// probabilities. If `X` has no rows, return a single unit sphere in the
// appropriate dimension with probability 1.
[[cpp11::register]]
cpp11::list MultiBoundingEllipsoids(cpp11::doubles_matrix<> X, double point_log_volume) {
  if (X.nrow() == 0) {
    return vol::Ellipsoid::as_list(std::list<vol::Ellipsoid>({vol::Ellipsoid(X.ncol())}));
  }
  Eigen::MatrixXd X_eigen = as_Matrix(X);
  std::list<vol::Ellipsoid> ellipsoids =
      vol::Ellipsoid::FitMany(X_eigen, point_log_volume);
  return vol::Ellipsoid::as_list(ellipsoids);
}

// Compute the minimum enclosing ball of the provided data points `X`
// using either bootstrap or jacknife resampling to estimate the maximum radius.
[[cpp11::register]]
cpp11::list MiniBall(cpp11::doubles_matrix<> X, int n_bootstraps, char method) {
  std::unique_ptr<vol::DistanceMatrix> dist_method;
  switch (method) {
    case 'e':
      dist_method = std::make_unique<vol::SquaredEuclideanDistances>();
      break;
    case 'c':
      dist_method = std::make_unique<vol::ChebshevDistances>();
      break;
    default:
      cpp11::stop("Unknown distance method '%c'", method);
  }
  Eigen::MatrixXd X_eigen = as_Matrix(X);
  vol::MiniBall miniball(X_eigen, std::move(dist_method));
  double max_radius =
      n_bootstraps == 0 ? miniball.jacknife() : miniball.bootstrap(n_bootstraps);
  using namespace cpp11::literals;
  return cpp11::writable::list(
      {"max_radius"_nm = (method == 'e') ? std::sqrt(max_radius) : max_radius});
}
