/* LICENSE NOTICE
 *
 * This is a modified version of the original code written by John Burkardt,
 * licensed under the LPGL. The original and unaltered source code for this
 * document can be found at
 * https://people.math.sc.edu/Burkardt/cpp_src/random_data/random_data.html
 */

#include "RandomData.hpp"

/**
 * Calculate the cholesky factorization of a symmetric positive definite matrix.
 * @param a The matrix to be factored, stored in column-major order.
 * @param lda The leading dimension of the matrix.
 * @param n The order of the matrix.
 * @return An integer indicating the success of the factorization.
 */
int RandomData::dpotrf(double a[], const int lda, const int n) {
  const char UUPLO = 'U';
  const int &N = n;
  const int &LLDA = lda;

  int info = 0;
  F77_CALL(dpotrf) (&UUPLO, &N, a, &LLDA, &info FCONE);
  return info;
}

/**
 * Solve a system of linear equations A*X = B, knowing that A is factorized
 * by dpotrf.
 * @param a The output from dpotrf, the factorized matrix.
 * @param lda The leading dimension of the matrix A.
 * @param n The order of the matrix A.
 * @param b The right-hand side vector, which will be overwritten with the
 * solution.
 */
void RandomData::dpotrs(const double a[], const int lda, const int n, double b[]) {
  const char UUPLO = 'U';
  const int &N = n;
  const int &LLDA = lda;
  int info = 0;
  F77_NAME(dpotrs) (&UUPLO, &N, &N, a, &LLDA, b, &LLDA, &info FCONE);
}

/*
* Generate a vector of U[0, 1] distributed random numbers.
* @param n The number of random numbers to generate.
* @return A pointer to an array of doubles containing the random numbers.
*/
double *RandomData::unif_rand_n(int n) {
  double *r = new double[n];
  for (int i = 0; i < n; i++) {
    r[i] = unif_rand();
  }
  return r;
}

/*
* Generate a vector of U[0, 1] distributed random numbers in-place.
* @param n The number of random numbers to generate.
* @param vec A pointer to an array of doubles where the random numbers will
* be stored.
*/
void RandomData::unif_rand_n(int n, double *vec) {
  for (int i = 0; i < n; i++) {
    vec[i] = unif_rand();
  }
}

/*
* Generate a vector of N(0, 1) distributed random numbers.
* @param n The number of random numbers to generate.
* @return A pointer to an array of doubles containing the random numbers.
*/
double *RandomData::norm_rand_n(int n) {
  double *r = new double[n];
  for (int i = 0; i < n; i++) {
    r[i] = norm_rand();
  }
  return r;
}

/*
* Generate a point in the uniform random hypersphere sphere.
* @param num_dim The dimension of the space.
* @param n The number of points to generate.
* @return A pointer to an array of doubles containing the points in the hypersphere.
*/
double *RandomData::uniform_in_sphere(int num_dim, int n) {
  double exponent = 1.0 / static_cast<double>(num_dim);
  double *x = new double[num_dim * n];
  int inc_x = 1;

  for (int j = 0; j < n; j++) {
    // Normalize a random vector to lie on the unit sphere
    double *v = norm_rand_n(num_dim);
    double norm = dnrm2_(&num_dim, v, &inc_x);
    for (int i = 0; i < num_dim; i++) {
      v[i] = v[i] / norm;
    }

    // Scale the vector to lie within the unit sphere
    double r = unif_rand();
    r = pow(r, exponent);
    for (int i = 0; i < num_dim; i++ ) {
      x[i+j*num_dim] = r * v[i];
    }
  }
  return x;
};

/*
* Generate uniform points in the unit hypercube [0, 1].
* @param num_dim The dimension of the space.
* @param n The number of points to generate.
* @return A pointer to an array of doubles containing the points in the
* hypercube.
*/
double *RandomData::uniform_in_cube(int num_dim, int n) {
  double *x = new double[num_dim * n];
  unif_rand_n(num_dim * n, x);
  return x;
}

/*
* Map uniform points into an ellipsoid defined by a positive definite matrix A
* and a radius R.
* @param num_dim The dimension of the space.
* @param n The number of points to generate.
* @param a The matrix that describes the ellipsoid.
* @param r The right-hand side of the ellipsoid equation (i.e. the "radius").
*/
double *RandomData::uniform_in_ellipsoid(int dim_num, int n, double a[], double r, int &seed ) {
  double *u = new double[dim_num * dim_num];

  char type = 'G';
  dlacpy_(&type, &dim_num, &dim_num, a, &dim_num, u, &dim_num FCONE);
  int info = dpotrf(u, dim_num, dim_num);
  double *x = uniform_in_sphere(dim_num, n);

  double cfrom = 1.0;
  info = 0;
  F77_CALL(dlascl)(&type, &info, &info, &cfrom, &r, &dim_num, &n, x, &dim_num, &info FCONE);

  for (int j = 0; j < n; j++) {
    dpotrs(u, dim_num, dim_num, x + j * dim_num);
  }

  return x;
}
