/* LICENSE NOTICE
 *
 * This is a modified version of the original code written by John Burkardt,
 * licensed under the LPGL. The original and unaltered source code for this
 * document can be found at
 * https://people.math.sc.edu/Burkardt/cpp_src/random_data/random_data.html
 */

#include "RandomData.h"
#include <iostream>

/*
 * Iteratively reflect numbers in a vector until they are all in [0, 1].
 * @param vec The doubles vector.
 * @returns Nothing; vec is modified in-place.
 */
void RandomData::reflect(cpp11::writable::doubles &vec) {
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


// /*
// * Generate uniform points in the unit hypercube [0, 1].
// * @param matrix The matrix to fill with random points.
// */
// void RandomData::uniform_in_cube(cpp11::writable::doubles_matrix<> &matrix) {
//   double *matrix_ptr = REAL(matrix.data());
//   int n = matrix.nrow();
//   int num_dim = matrix.ncol();
//   unif_rand_n(num_dim * n, matrix_ptr);
// }
//
// /*
//  * Generate a point in the uniform random hypersphere sphere.
//  * @param num_dim The dimension of the space.
//  * @param n The number of points to generate.
//  * @return A pointer to an array of doubles containing the points in the hypersphere.
// */
// void RandomData::uniform_in_sphere(cpp11::writable::doubles_matrix<> &matrix) {
//   int num_points = matrix.ncol();
//   int num_dim = matrix.nrow();
//   double exponent = 1.0 / static_cast<double>(num_dim);
//   double *x = REAL(matrix.data());
//   int inc_x = 1;
//
//   for (int row = 0; row < num_points; row++) {
//     // Normalize a random vector in N(0,1).
//     std::vector<double> v(num_dim);
//     norm_rand_n(num_dim, v.data());
//     double norm = dnrm2_(&num_dim, v.data(), &inc_x);
//     for (auto &val : v) {
//       val /= norm;
//     }
//
//     // Scale the point to a random radius in the unit sphere.
//     double r = unif_rand();
//     r = pow(r, exponent);
//     for (auto &val : v) {
//       val *= r;
//     }
//     std::move(v.begin(), v.end(), x + row * num_dim);
//   }
// };

/*
 * Generate a vector of U[0, 1] distributed random numbers in-place.
 * @param vec A pointer to an array of doubles where the random numbers will
 * be stored.
*/
void RandomData::unif_rand_n(cpp11::writable::doubles &vec) {
  std::generate(vec.begin(), vec.end(), unif_rand);
}

/*
* Generate a vector of N(0, 1) distributed random numbers.
* @param vec A pointer to an array of doubles where the random numbers will
* be stored.
*/
void RandomData::norm_rand_n(cpp11::writable::doubles &vec) {
  std::generate(vec.begin(), vec.end(), norm_rand);
}
