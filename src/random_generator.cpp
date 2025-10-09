/**
 * @file random_generator.cpp
 * @brief Implementation of random number generation utilities.
 *
 * This file contains implementations of various random sampling algorithms
 * for geometric distributions used in nested sampling. Some algorithms
 * are adapted from John Burkardt's random_data library.
 */

#include "random_generator.h"

/**
 * @brief Reflects each element of the vector within the unit cube [0, 1].
 *
 * @param vec Vector to be reflected in-place.
 */
void random_generator::ReflectWithinUnitCube(Ref<RowVector> vec) {
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

/**
 * @brief Generate a point on the surface of a sphere.
 *
 * @param vec Vector to fill (in-place).
 * @param radius Radius of the sphere.
 */
void random_generator::UniformOnSphere(Ref<RowVector> vec,
                                       const double radius) {
  RNorm(vec);
  double inv_norm = std::pow(vec.norm(), -1.0);
  vec *= inv_norm;
  vec *= radius;
}

/**
 * @brief Generate a point inside a ball.
 *
 * @param vec Vector to fill (in-place).
 * @param radius Radius of the ball.
 */
void random_generator::UniformInBall(Ref<RowVector> vec, const double radius) {
  const int n_dim = vec.size();
  UniformOnSphere(vec, pow(unif_rand(), 1.0 / n_dim));
  vec *= radius;
}

/**
 * @brief Generates a random vector uniformly distributed inside an ellipsoid.
 *
 * @param scaledInvSqrtA Scaled inverse square root of the precision matrix.
 * @param loc Vector specifying the location to shift the sampled point.
 * @param vec Vector to fill (in-place).
 */
void random_generator::UniformInEllipsoid(ConstRef<Matrix> scaledInvSqrtA,
                                          ConstRef<RowVector> loc,
                                          Ref<RowVector> vec) {
  UniformInBall(vec);
  vec = (vec * scaledInvSqrtA) + loc;
}