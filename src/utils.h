// File: /Users/ksnap/Projects/ernest/src/utils.h
// Created Date: Tuesday, October 14th 2025
// Author: Kyle Dewsnap
//
// Copyright (c) 2025 Kyle Dewsnap
// GNU General Public License v3.0 or later
// https://www.gnu.org/licenses/gpl-3.0-standalone.html
//
// Utility functions and type definitions for the Ernest project.
#pragma once
#include <R_ext/Random.h>
#include <Rmath.h>

#include "cpp11.hpp"
#include "cpp11eigen.hpp"

namespace ern {

constexpr double kPrecision = Eigen::NumTraits<double>::dummy_precision();
// Reference to nonmutable Eigen object.
template <class T>
using ConstRef = Eigen::Ref<const T>;
// Reference to mutable Eigen object.
template <class T>
using Ref = Eigen::Ref<T>;

// Shorthand names for Eigen objects
using RowVector = Eigen::RowVectorXd;
using ColVector = Eigen::VectorXd;
using Vector = Eigen::VectorXd;
using Matrix = Eigen::MatrixXd;

// Fuzzy compare `v` == `w`.
inline bool WithinRel(const double v, const double w,
                      const double prec = 100 *
                                          Eigen::NumTraits<double>::dummy_precision()) {
  return abs(v - w) <= (prec * fmin2(abs(v), abs(w)));
}

// Fuzzy compare `v` to zero.
inline bool isZero(const double v,
                   const double prec = Eigen::NumTraits<double>::dummy_precision()) {
  return abs(v) <= prec;
}

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