/* LICENSE NOTICE
 *
 * This is a modified version of the original code written by John Burkardt,
 * licensed under the LPGL. The original and unaltered source code for this
 * document can be found at
 * https://people.math.sc.edu/Burkardt/cpp_src/random_data/random_data.html
 */
#pragma once
#include <vector>

#include <R_ext/BLAS.h>
#include <R_ext/Lapack.h>
#include <R_ext/Random.h>
#include <Rmath.h>

#include "lapack_wrapper.h"

namespace random_vector {

/**
 * @brief Safely generate random numbers from R.
 */
struct RandomEngine {
  RandomEngine() { GetRNGstate(); };
  ~RandomEngine() { PutRNGstate(); };
};

/**
 * @brief Check if point is outside the unit hypercube
 *
 * @param vec A vector.
 * @return true If all components of `vec` are in (0, 1).
 * @return false Otherwise.
 */
inline bool IsOutsideUnitCube(const cpp11::doubles& vec) {
  return std::any_of(vec.cbegin(), vec.cend(),
                     [](double i) { return i < 0.0 || i > 1.0; });
}

void ReflectWithinUnitCube(cpp11::writable::doubles& vec);

void UniformOnSphere(cpp11::writable::doubles& vec, const double radius = 1);
void UniformInBall(cpp11::writable::doubles& vec, const double radius = 1);
void UniformInEllipsoid(cpp11::doubles_matrix<>& axes,
                        cpp11::writable::doubles& vec);

void RUnif(cpp11::writable::doubles& vec);
void RNorm(cpp11::writable::doubles& vec);

}  // namespace random_vector
