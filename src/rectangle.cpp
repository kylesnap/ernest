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

using namespace ern::vol;

bool Rectangle::Clamp(const ConstRef<Vector> inner,
                      const ConstRef<Vector> outer) {
  if (!Covered(inner) || !Covered(outer) || inner.isApprox(outer)) {
    return false;
  }

  // Clamp dimensions based on relative position of inner and outer.
  for (size_t d = 0; d < n_dim_; d++) {
    double dist = outer[d] - inner[d];
    if (ern::isZero(dist)) continue;
    switch (static_cast<int>(sign(dist))) {
      case -1:  // LOW -- OUT -- IN -- UP
        lower_[d] = outer[d];
        break;
      case 1:  // LOW -- IN -- OUT -- UP
        upper_[d] = outer[d];
        break;
    }
  }
  return true;
}
