#pragma once
#include <cpp11.hpp>
#include <cpp11eigen.hpp>

/**
 * @brief We don't use the built-in wrappers from cpp11eigen mostly because we
 * want to treat R vectors as row vectors, rather than columns. These are
 * also namespaced to avoid conflicts with cpp11.
 */
namespace wrap {

/**
 * Generic template that throws an error for unsupported conversions.
 * @param x Input of unsupported type.
 * @return Never returns, throws error.
 */
template <typename T>
inline Eigen::Matrix<double, Eigen::Dynamic, Eigen::Dynamic> as_eigen(
    const T &x) {
  cpp11::stop("Cannot wrap as Eigen::Matrix.");
};

/**
 * Converts R doubles vector to an Eigen row vector.
 * @param x R doubles vector.
 * @return Eigen row vector mapped to the input data.
 */
inline Eigen::RowVectorXd as_eigen(const cpp11::doubles &x) {
  return Eigen::Map<Eigen::RowVectorXd>(REAL(x.data()), x.size());
};

/**
 * Converts R doubles matrix to an Eigen matrix.
 * @param x R doubles matrix.
 * @return Eigen matrix mapped to the input data.
 */
inline Eigen::MatrixXd as_eigen(const cpp11::doubles_matrix<> &x) {
  return Eigen::Map<Eigen::MatrixXd>(REAL(x.data()), x.nrow(), x.ncol());
};

/**
 * Generic template that throws an error for unsupported conversions.
 * @param x Input of unsupported type.
 * @return Never returns, throws error.
 */
template <typename T, typename S>
inline T as_cpp(const S &x) {
  cpp11::stop("Cannot wrap as SEXP.");
};

/**
 * Converts Eigen row vector to an R doubles vector.
 * @param x Eigen row vector.
 * @return R doubles vector.
 */
inline cpp11::doubles as_doubles(
    const Eigen::Ref<const Eigen::RowVectorXd> &x) {
  return cpp11::as_doubles(cpp11::as_sexp(x));
}

}  // namespace wrap