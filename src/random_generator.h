/**
 * @file random_generator.h
 * @brief Random number generation utilities for nested sampling algorithms.
 *
 * This module provides functions for generating random samples from various
 * distributions including uniform, normal, spherical, and ellipsoidal
 * distributions, with special support for unit cube constraints.
 */

#pragma once
#include <R_ext/Random.h>
#include <Rmath.h>

#include <algorithm>
#include <cmath>

#include "utils.h"

namespace random_generator {

/**
 * @brief RAII wrapper for R's random number generator state.
 *
 * Ensures proper initialization and cleanup of R's RNG state
 * to maintain reproducibility and thread safety.
 */
struct RandomEngine {
  RandomEngine() { GetRNGstate(); }
  ~RandomEngine() { PutRNGstate(); }
};

/**
 * @brief Checks if a point lies outside the unit hypercube [0,1]^n.
 *
 * @param vec A vector to check.
 * @return true if any component of vec is outside [0,1], false otherwise.
 */
inline bool IsOutsideUnitCube(const Eigen::Ref<Eigen::RowVectorXd> vec) {
  return (vec.array() < 0.0).any() || (vec.array() > 1.0).any();
}

/**
 * @brief Reflects a vector to lie within the unit hypercube [0,1]^n.
 *
 * @param vec Vector to be reflected in-place.
 */
void ReflectWithinUnitCube(Eigen::Ref<Eigen::RowVectorXd> vec);

/**
 * @brief Generates a point uniformly distributed on the surface of a sphere.
 *
 * @param vec Vector to fill with the generated point (modified in-place).
 * @param radius Radius of the sphere (default: 1.0).
 */
void UniformOnSphere(Eigen::Ref<Eigen::RowVectorXd> vec,
                     const double radius = 1);

/**
 * @brief Generates a point uniformly distributed inside a ball.
 *
 * @param vec Vector to fill with the generated point (modified in-place).
 * @param radius Radius of the ball (default: 1.0).
 */
void UniformInBall(Eigen::Ref<Eigen::RowVectorXd> vec, const double radius = 1);

/**
 * @brief Generates a point uniformly distributed inside an ellipsoid.
 *
 * @param trans Transformation matrix defining the ellipsoid's axes and scaling.
 * @param scale Additional scaling factor for the ellipsoid.
 * @param loc Center location of the ellipsoid.
 * @param vec Vector to fill with the generated point (modified in-place).
 */
void UniformInEllipsoid(const Eigen::Ref<Eigen::MatrixXd> trans,
                        const double scale,
                        const Eigen::Ref<Eigen::RowVectorXd> loc,
                        Eigen::Ref<Eigen::RowVectorXd> vec);

/**
 * @brief Fills a vector with uniformly distributed random numbers in [0,1].
 *
 * @tparam T Container type that supports begin() and end() iterators.
 * @param vec Vector to fill (modified in-place).
 */
template <class T>
inline void RUnif(T& vec) {
  std::generate(vec.begin(), vec.end(), unif_rand);
}

/**
 * @brief Fills a vector with standard normally-distributed random numbers.
 *
 * Generates samples from N(0,1) distribution.
 *
 * @tparam T Container type that supports begin() and end() iterators.
 * @param vec Vector to fill (modified in-place).
 */
template <class T>
inline void RNorm(T& vec) {
  std::generate(vec.begin(), vec.end(), norm_rand);
}

/**
 * @brief Computes the outer product of a row vector with itself.
 *
 * For a row vector x, computes x^T * x, which produces a square matrix.
 *
 * @param x Input row vector.
 * @return Square matrix representing the outer product x^T * x.
 */
inline Eigen::MatrixXd outer(const Eigen::Ref<const Eigen::RowVectorXd>& x) {
  return x.transpose() * x;
}

}  // namespace random_generator
