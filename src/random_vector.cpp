/* LICENSE NOTICE
 *
 * This is a modified version of the original code written by John Burkardt,
 * licensed under the LPGL. The original and unaltered source code for this
 * document can be found at
 * https://people.math.sc.edu/Burkardt/cpp_src/random_data/random_data.html
 */

#include "random_vector.h"

#include <iostream>

/**
 * @brief Reflects each element of the vector within the unit cube [0, 1].
 *
 * @param vec Vector to be reflected in-place.
 */
void random_vector::ReflectWithinUnitCube(Eigen::Ref<Eigen::RowVectorXd> vec) {
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
void random_vector::UniformOnSphere(Eigen::Ref<Eigen::RowVectorXd> vec,
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
void random_vector::UniformInBall(Eigen::Ref<Eigen::RowVectorXd> vec,
                                  const double radius) {
  const int n_dim = vec.size();
  UniformOnSphere(vec, pow(unif_rand(), 1.0 / n_dim));
  vec *= radius;
}

/**
 * @brief Generates a random vector uniformly distributed inside an ellipsoid.
 *
 * @param cov The covariance matrix defining the ellipsoid.
 * @param d2 The average squared radius of the ellipsoid.
 * @param loc Vector specifying the location to shift the sampled point.
 * @param vec Vector to fill (in-place).
 */
void random_vector::UniformInEllipsoid(const Eigen::Ref<Eigen::MatrixXd> cov,
                                       const double d2,
                                       const Eigen::Ref<Eigen::RowVectorXd> loc,
                                       Eigen::Ref<Eigen::RowVectorXd> vec) {
  Eigen::LLT<Eigen::MatrixXd> llt(cov);
  UniformInBall(vec);
  vec = sqrt(d2) * (vec * llt.matrixU());
  vec += loc;
}
