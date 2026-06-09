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

  // Clamp dimensions based on relative position of inner and outer.
  for (size_t d = 0; d < nvar_; d++) {
    if (outer[d] < inner[d]) {
      lower_[d] = outer[d];
    } else if (outer[d] > inner[d]) {
      upper_[d] = outer[d];
    }
  }
  return true;
}
