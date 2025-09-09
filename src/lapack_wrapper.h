#pragma once
#include <R_ext/BLAS.h>
#include <R_ext/Lapack.h>
#include <Rmath.h>
#ifndef FCONE
#define FCONE
#endif

#include "cpp11.hpp"

namespace lapack_wrapper {
const int kIncX = 1;
const int kIncY = 1;
const char kNoTranspose = 'N';
const char kUpperTriangular = 'U';
const char kNonUnitDiagonal = 'N';

/**
 * @brief Get a pointer to the elements of an R object.
 *
 * @param x The R vector or matrix.
 * @return a `double*`.
 */
inline double* GetPointer(cpp11::writable::doubles& x) {
  return REAL(x.data());
};
/**
 * @brief Get a const pointer to the elements of an R object.
 *
 * @param x The R vector.
 * @return a `const double*`.
 */
inline const double* GetPointer(cpp11::doubles& x) { return REAL(x.data()); };
/**
 * @brief Get a const pointer to the elements of an R object.
 *
 * @param x The R matrix.
 * @return a `const double*`.
 */
inline const double* GetPointer(cpp11::doubles_matrix<>& x) {
  return REAL(x.data());
};

/**
 * @brief Copy elements from x to y.
 *
 * @param x Vector to copy from.
 * @param y Vector to copy to.
 */
inline void dcopy(cpp11::doubles& x, cpp11::writable::doubles& y) {
  const int n = x.size();
  dcopy_(&n, GetPointer(x), &kIncX, GetPointer(y), &kIncY);
};

/**
 * @brief Compute y = alpha * x + y.
 *
 * @param alpha Scale.
 * @param x A vector to add to `y`.
 * @param y The output vector.
 */
inline void daxpy(const double alpha, cpp11::doubles& x,
                  cpp11::writable::doubles& y) {
  const int n = x.size();
  daxpy_(&n, &alpha, GetPointer(x), &kIncX, GetPointer(y), &kIncY);
}

/**
 * @brief Compute the 2-norm.
 *
 * @param x Input vector.
 * @return The Euclidean norm.
 */
inline double dnrm2(cpp11::doubles& x) {
  const int n = x.size();
  return dnrm2_(&n, GetPointer(x), &kIncX);
};

/**
 * @brief Scale a vector by a constant.
 *
 * @param alpha Scale factor.
 * @param x Vector to scale.
 */
inline void dscal(const double alpha, cpp11::writable::doubles& x) {
  const int n = x.size();
  dscal_(&n, &alpha, GetPointer(x), &kIncX);
};

/**
 * @brief Matrix-vector multiplication for triangular matrices.
 *
 * @param A An upper triangular matrix.
 * @param x Vector to multiply.
 */
inline void dtrmv(cpp11::doubles_matrix<>& A, cpp11::writable::doubles& x) {
  const int n = A.nrow();
  const int lda = A.nrow();
  dtrmv_(&kUpperTriangular, &kNoTranspose, &kNonUnitDiagonal, &n, GetPointer(A),
         &lda, GetPointer(x), &kIncX FCONE FCONE FCONE);
}

/**
 * @brief Symmetric matrix-vector multiplication.
 *
 * @param alpha Scalar multiplier for A*x.
 * @param A Symmetric matrix.
 * @param x Input vector.
 * @param beta Scalar multiplier for y.
 * @param y Output vector.
 */
inline void dsymv(const double alpha, cpp11::doubles_matrix<>& A,
                  cpp11::doubles& x, const double beta,
                  cpp11::writable::doubles& y) {
  const int n = A.nrow();
  const int lda = A.nrow();
  dsymv_(&kUpperTriangular, &n, &alpha, GetPointer(A), &lda, GetPointer(x),
         &kIncX, &beta, GetPointer(y), &kIncY FCONE);
};
}  // namespace lapack_wrapper