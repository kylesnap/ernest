/* LICENSE NOTICE
 *
 * This is a modified version of the original code written by John Burkardt,
 * licensed under the LPGL. The original and unaltered source code for this
 * document can be found at
 * https://people.math.sc.edu/Burkardt/cpp_src/random_data/random_data.html
 */
#pragma once
#include <R_ext/Random.h>

#include "wrap.h"

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
inline bool IsOutsideUnitCube(const Eigen::Ref<Eigen::RowVectorXd> vec) {
  return (vec.array() < 0.0).any() || (vec.array() > 1.0).any();
}

void ReflectWithinUnitCube(Eigen::Ref<Eigen::RowVectorXd> vec);

void UniformOnSphere(Eigen::Ref<Eigen::RowVectorXd> vec,
                     const double radius = 1);
void UniformInBall(Eigen::Ref<Eigen::RowVectorXd> vec, const double radius = 1);
void UniformInEllipsoid(const Eigen::Ref<Eigen::MatrixXd> cov, const double d2,
                        const Eigen::Ref<Eigen::RowVectorXd> loc,
                        Eigen::Ref<Eigen::RowVectorXd> vec);

/**
 * @brief Fills a vector with uniformly distributed random numbers in [0, 1].
 *
 * @param vec Vector to fill (in-place).
 */
template <class T>
inline void RUnif(T& vec) {
  std::generate(vec.begin(), vec.end(), unif_rand);
}

/**
 * @brief Fills a vector with standard normally-distributed random numbers.
 *
 * @param vec Vector to fill (in-place).
 */
template <class T>
inline void RNorm(T& vec) {
  std::generate(vec.begin(), vec.end(), norm_rand);
}

}  // namespace random_vector
