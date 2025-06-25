/* LICENSE NOTICE
 *
 * This is a modified version of the original code written by John Burkardt,
 * licensed under the LPGL. The original and unaltered source code for this
 * document can be found at
 * https://people.math.sc.edu/Burkardt/cpp_src/random_data/random_data.html
 */

#include <R_ext/BLAS.h>
#include <R_ext/Lapack.h>
#include <R_ext/Random.h>
#include <Rmath.h>

namespace RandomData {

int dpotrf(double a[], const int lda, const int n);
void dpotrs(const double a[], const int lda, const int n, double b[]);
double *unif_rand_n(int n);
void unif_rand_n(int n, double *vec);
double *norm_rand_n(int n);
double *uniform_in_sphere(int num_dim, int n);
double *uniform_in_cube(int num_dim, int n);
double *uniform_in_ellipsoid(int dim_num, int n, double a[], double r, int &seed);

} // namespace RandomData
