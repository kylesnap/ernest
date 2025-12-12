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