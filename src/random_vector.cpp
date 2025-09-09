/* LICENSE NOTICE
 *
 * This is a modified version of the original code written by John Burkardt,
 * licensed under the LPGL. The original and unaltered source code for this
 * document can be found at
 * https://people.math.sc.edu/Burkardt/cpp_src/random_data/random_data.html
 */

#include "random_vector.h"

/**
 * @brief Reflects each element of the vector within the unit cube [0, 1].
 *
 * @param vec Vector to be reflected in-place.
 */
void random_vector::ReflectWithinUnitCube(cpp11::writable::doubles& vec) {
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
void random_vector::UniformOnSphere(cpp11::writable::doubles& vec,
                                    const double radius) {
  RNorm(vec);
  double norm = lapack_wrapper::dnrm2(vec);
  double inv_norm = R_pow_di(norm, -1);
  lapack_wrapper::dscal(inv_norm, vec);
  lapack_wrapper::dscal(radius, vec);
}

/**
 * @brief Generate a point inside a ball.
 *
 * @param vec Vector to fill (in-place).
 * @param radius Radius of the ball.
 */
void random_vector::UniformInBall(cpp11::writable::doubles& vec,
                                  const double radius) {
  const int n_dim = vec.size();
  UniformOnSphere(vec, R_pow(unif_rand(), 1.0 / n_dim));
  lapack_wrapper::dscal(radius, vec);
}

/**
 * @brief Generates a random vector uniformly distributed inside an ellipsoid.
 *
 * @param axes Matrix representing the ellipsoid axes.
 * @param vec Vector to fill (in-place).
 */
void random_vector::UniformInEllipsoid(cpp11::doubles_matrix<>& axes,
                                       cpp11::writable::doubles& vec) {
  UniformInBall(vec);
  lapack_wrapper::dtrmv(axes, vec);
}

/**
 * @brief Fills a vector with uniformly distributed random numbers in [0, 1].
 *
 * @param vec Vector to fill (in-place).
 */
void random_vector::RUnif(cpp11::writable::doubles& vec) {
  std::generate(vec.begin(), vec.end(), unif_rand);
}

/**
 * @brief Fills a vector with standard normally-distributed random numbers.
 *
 * @param vec Vector to fill (in-place).
 */
void random_vector::RNorm(cpp11::writable::doubles& vec) {
  std::generate(vec.begin(), vec.end(), norm_rand);
}