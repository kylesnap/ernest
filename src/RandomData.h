/* LICENSE NOTICE
 *
 * This is a modified version of the original code written by John Burkardt,
 * licensed under the LPGL. The original and unaltered source code for this
 * document can be found at
 * https://people.math.sc.edu/Burkardt/cpp_src/random_data/random_data.html
 */
#include <vector>

#include <R_ext/BLAS.h>
#include <R_ext/Lapack.h>
#include <R_ext/Random.h>
#include <Rmath.h>

#include "cpp11.hpp"

namespace RandomData {

// Fill R matrices with random data
void uniform_in_cube(cpp11::writable::doubles_matrix<> &matrix);
void uniform_in_sphere(cpp11::writable::doubles_matrix<> &matrix);

void unif_rand_n(int n, double *vec);
void norm_rand_n(int n, double *vec);

} // namespace RandomData
