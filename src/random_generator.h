// File: /Users/ksnap/Projects/ernest/src/random_generator.h
// Created Date: Monday, October 20th 2025
// Author: Kyle Dewsnap
//
// Copyright (c) 2025 Kyle Dewsnap
// GNU General Public License v3.0 or later
// https://www.gnu.org/licenses/gpl-3.0-standalone.html
//
// Wrappers and utilities for R's random number generator.
#pragma once
#include <R_ext/Random.h>
#include <Rmath.h>

#include <algorithm>
#include <cmath>

#include "utils.h"

namespace ern {

// Wrapper for R's random number generator state.
struct RandomEngine {
  RandomEngine() { GetRNGstate(); }
  ~RandomEngine() { PutRNGstate(); }
};

// Fill `vec` with a point from the uniform distribution U(0,1).
template <class T>
inline void RUnif(T& vec) {
  std::generate(vec.begin(), vec.end(), unif_rand);
}

// Fill `vec` with a point from the standard normal distribution N(0,1).
template <class T>
inline void RNorm(T& vec) {
  std::generate(vec.begin(), vec.end(), norm_rand);
}

// Checks if any component of `vec` is outside the unit cube [0,1]^d.
inline bool IsOutsideUnitCube(ConstRef<RowVector> vec) {
  return (vec.array() < 0.0).any() || (vec.array() > 1.0).any();
}

// Reflects components of `vec` to ensure all lie within the unit cube [0,1]^d.
inline void ReflectWithinUnitCube(Ref<RowVector> vec) {
  for (size_t i = 0; i < vec.size(); ++i) {
    double val = vec[i];
    bool idx_even = std::fmod(val, 2.0) < 1.0;
    if (idx_even) {
      vec[i] = std::fabs(std::fmod(val, 1.0));
    } else {
      vec[i] = std::fabs(1.0 - std::fmod(val, 1.0));
    }
  }
}

// Fill `vec` with a point uniformly distributed on the surface of a sphere
// with given `radius`.
inline void UniformOnSphere(Ref<RowVector> vec, const double radius = 1) {
  RNorm(vec);
  double inv_norm = std::pow(vec.norm(), -1.0);
  vec *= inv_norm;
  vec *= radius;
}

// Fill `vec` with a point uniformly distributed inside a ball of given
// `radius`.
inline void UniformInBall(Ref<RowVector> vec, const double radius = 1) {
  const int n_dim = vec.size();
  UniformOnSphere(vec, pow(unif_rand(), 1.0 / n_dim));
  vec *= radius;
}

}  // namespace ern
