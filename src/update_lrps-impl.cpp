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
#include "KMeansRexCore.h"
#include "ellipsoid.h"

// Find the bounding ellipsoid of the given points in X. If X has no rows,
// then return the bounding unit sphere in the appropriate dimension.
[[cpp11::register]]
cpp11::list BoundingEllipsoid(cpp11::doubles_matrix<> X) {
  if (X.nrow() == 0) {
    return ern::vol::Ellipsoid(X.ncol()).as_list();
  }
  Eigen::MatrixXd X_eigen = as_Matrix(X);
  ern::vol::Ellipsoid ell(X_eigen);
  return ell.as_list();
}

// Fit multiple bounding ellipsoids to the provided data points `X`,
// returning their R list representations along with their scaled sampling
// probabilities. If `X` has no rows, return a single unit sphere in the
// appropriate dimension with probability 1.
[[cpp11::register]]
cpp11::list MultiBoundingEllipsoids(cpp11::doubles_matrix<> X,
                                    const double min_reduction,
                                    const bool allow_contact) {
  if (X.nrow() == 0) {
    ern::vol::Ellipsoid sphere(X.ncol());
    using namespace cpp11::literals;
    return cpp11::writable::list(
        {"prob"_nm = cpp11::writable::doubles({1}),
         "ellipsoid"_nm = cpp11::writable::list(sphere.as_list()),
         "tot_log_vol"_nm = sphere.log_volume()});
  }
  Eigen::MatrixXd X_eigen = as_Matrix(X);
  std::vector<ern::vol::Ellipsoid> ellipsoids =
      ern::vol::Ellipsoid::FitMultiEllipsoids(X_eigen, min_reduction,
                                              allow_contact);
  cpp11::writable::list ellipsoid_list;
  cpp11::writable::doubles prob;
  for (auto ell : ellipsoids) {
    prob.push_back(ell.log_volume());
    ellipsoid_list.push_back(ell.as_list());
  }
  double total_log_vol = logspace_sum(REAL(prob.data()), prob.size());
  std::transform(prob.begin(), prob.end(), prob.begin(),
                 [total_log_vol](double p) { return exp(p - total_log_vol); });
  using namespace cpp11::literals;
  return cpp11::writable::list({"prob"_nm = prob,
                                "ellipsoid"_nm = ellipsoid_list,
                                "tot_log_vol"_nm = total_log_vol});
}