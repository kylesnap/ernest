// File: /Users/ksnap/Projects/ernest/src/rectangle.h
// Created Date: Friday, October 24th 2025
// Author: Kyle Dewsnap
//
// Copyright (c) 2025 Kyle Dewsnap
// GNU General Public License v3.0 or later
// https://www.gnu.org/licenses/gpl-3.0-standalone.html
//
// Describes the sampling space for multidimensional slice sampling.
#pragma once

#include "random_generator.h"

namespace ern {
namespace vol {

class Rectangle {
 public:
  // Construct the (0-1) unit hypercube.
  inline explicit Rectangle(const int n_dim)
      : n_dim_(n_dim),
        lower_(Eigen::ArrayXd::Zero(n_dim_)),
        upper_(Eigen::ArrayXd::Ones(n_dim_)) {};

  // Construct a rectangle from initial vectors of boundaries.
  inline Rectangle(cpp11::doubles& lower, cpp11::doubles& upper)
      : n_dim_(lower.size()) {
    Vector low = as_Matrix(lower), up = as_Matrix(upper);
    lower_ = low.cwiseMin(up);
    upper_ = up.cwiseMax(low);
    if (lower_.minCoeff() < 0 || upper_.maxCoeff() > 1) {
      cpp11::stop("Rectangle bounds OOB.");
    }
  }

  // Generate a uniform sample from the rectangle.
  inline Vector UniformSample() const {
    Vector sample(n_dim_);
    for (size_t d = 0; d < n_dim_; d++) {
      sample[d] = lower_[d] + (upper_[d] - lower_[d]) * unif_rand();
    }
    return sample;
  }

  // Fill `vec` with a uniform sample from the rectangle.
  inline void UniformSample(Ref<Vector> vec) const {
    if (vec.size() != n_dim_) return;
    vec = std::move(UniformSample());
  }

  // Check whether `point` is within the rectangle.
  inline bool Covered(const Eigen::Ref<const Vector> p) const {
    return (lower_.array() <= p.array()).all() &&
           (p.array() <= upper_.array()).all();
  }

  // Clamps the rectangle so the point `outer` falls on its boundary and
  // the point `inner` falls in its interior. Returns TRUE if the clamp is
  // successful, and FALSE if either `inner` or `outer` are out-of-bounds of the
  // rectangle or if they are equal.
  bool Clamp(const ConstRef<Vector> inner, const ConstRef<Vector> outer);

  // Return the rectangle as an R list.
  inline cpp11::list as_list() const {
    using namespace cpp11::literals;
    return cpp11::writable::list(
        {"lower"_nm = as_doubles(lower_), "upper"_nm = as_doubles(upper_)});
  }

  // Getters
  inline int n_dim() const { return n_dim_; }
  inline Vector lower() const { return lower_; }
  inline Vector upper() const { return upper_; }

 private:
  int n_dim_;             // Number of dimensions
  Eigen::ArrayXd lower_;  // Lower bounds of the rectangle axes.
  Eigen::ArrayXd upper_;  // Upper bounds of the rectangle axes.
  ern::RandomEngine gen;  // Random number generator.
};

}  // namespace vol
}  // namespace ern