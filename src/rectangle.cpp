// File: /Users/ksnap/Projects/ernest/src/rectangle.cpp
// Created Date: Friday, October 24th 2025
// Author: Kyle Dewsnap
//
// Copyright (c) 2025 Kyle Dewsnap
// GNU General Public License v3.0 or later
// https://www.gnu.org/licenses/gpl-3.0-standalone.html
//
// Implementation for Rectangle class
#include "rectangle.h"

#include <iostream>

using namespace vol;

bool Rectangle::Clamp(const ConstRef<Vector> inner, const ConstRef<Vector> outer) {
  if (!Covered(inner) || !Covered(outer) || inner.isApprox(outer)) {
    return false;
  }
  constexpr double eps = std::numeric_limits<double>::epsilon();

  // Clamp dimensions based on relative position of inner and outer.
<<<<<<< HEAD
  const Eigen::ArrayXd diff = outer - inner;
  lower_ = (diff < -eps).select(outer, lower_);
  upper_ = (diff > eps).select(outer, upper_);
  width_ = upper_ - lower_;
=======
  for (size_t d = 0; d < nvar_; d++) {
    if (outer[d] < inner[d]) {
      lower_[d] = outer[d];
    } else if (outer[d] > inner[d]) {
      upper_[d] = outer[d];
    }
  }
>>>>>>> main
  return true;
}
