#pragma once
#include <R_ext/BLAS.h>
#include <R_ext/Lapack.h>
#include <Rmath.h>
#ifndef FCONE
  #define FCONE
#endif

#include "cpp11.hpp"

namespace cpp11_blas {
  const int inc_x = 1;
  const int inc_y = 1;
  const char trans = 'N'; // No transpose
  const char uplo = 'U'; // Upper-unit triangular
  const char diag = 'N'; // Non-unit diagonal

  inline double* get_pointer(cpp11::writable::doubles &x) {
    return REAL(x.data());
  };
  inline const double* get_pointer(cpp11::doubles &x) {
    return REAL(x.data());
  };
  inline const double* get_pointer(cpp11::doubles_matrix<> &x) {
    return REAL(x.data());
  };

  inline void dcopy(cpp11::doubles &x,
                    cpp11::writable::doubles &y) {
    const int n = x.size();
    dcopy_(&n, get_pointer(x), &inc_x, get_pointer(y), &inc_y);
  };

  inline void dsymv(const double alpha, cpp11::doubles_matrix<> &A, cpp11::doubles &x, const double beta, cpp11::writable::doubles &y) {
    const int n = A.nrow();
    const int lda = A.nrow();
    dsymv_(&uplo, &n, &alpha, get_pointer(A), &lda, get_pointer(x), &inc_x, &beta, get_pointer(y), &inc_y FCONE);
  };
}