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
void random_generator::ReflectWithinUnitCube(
    Eigen::Ref<Eigen::RowVectorXd> vec) {
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
void random_generator::UniformOnSphere(Eigen::Ref<Eigen::RowVectorXd> vec,
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
void random_generator::UniformInBall(Eigen::Ref<Eigen::RowVectorXd> vec,
                                     const double radius) {
  const int n_dim = vec.size();
  UniformOnSphere(vec, pow(unif_rand(), 1.0 / n_dim));
  vec *= radius;
}

/**
 * @brief Generates a random vector uniformly distributed inside an ellipsoid.
 *
 * @param transformation The matrix defining the ellipsoid's axes.
 * @param scale The scaling factor for the ellipsoid.
 * @param loc Vector specifying the location to shift the sampled point.
 * @param vec Vector to fill (in-place).
 */
void random_generator::UniformInEllipsoid(
    const Eigen::Ref<Eigen::MatrixXd> trans, const double scale,
    const Eigen::Ref<Eigen::RowVectorXd> loc,
    Eigen::Ref<Eigen::RowVectorXd> vec) {
  UniformInBall(vec);
  vec = sqrt(scale) * (vec * trans) + loc;
}