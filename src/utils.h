/**
 * @file utils.h
 * @brief Utility functions for type conversions and testing.
 *
 * Utility functions for converting between R and Eigen data types, as well as
 * testing utilities for numerical comparisons.
 */

#pragma once
#include <Rmath.h>

#include <cpp11.hpp>
#include <cpp11eigen.hpp>

// Reference to nonmutable Eigen object.
template <class T>
using ConstRef = Eigen::Ref<const T>;
// Reference to mutable Eigen object.
template <class T>
using Ref = Eigen::Ref<T>;

// Shorthand names for Eigen objects
using RowVector = Eigen::RowVectorXd;
using ColVector = Eigen::VectorXd;
using Vector = Eigen::VectorXd;
using Matrix = Eigen::MatrixXd;

namespace cpp11 {

/**
 * @brief Converts R doubles vector to an Eigen row vector.
 *
 * @param x R doubles vector to convert.
 * @return Eigen row vector mapped to the input data.
 */
inline RowVector as_row_vector(const cpp11::doubles &x) {
  return Eigen::Map<RowVector>(REAL(x.data()), x.size());
}

/**
 * @brief Converts Eigen row vector to an R doubles vector.
 *
 * @param x Eigen row vector to convert.
 * @return R doubles vector containing the data from x.
 */
inline cpp11::doubles as_row_doubles(const ConstRef<RowVector> &x) {
  return cpp11::as_doubles(cpp11::as_sexp(x));
}

}  // namespace cpp11

namespace test {

/**
 * @brief Compares two double values for approximate equality.
 *
 * @param a First double value to compare.
 * @param b Second double value to compare.
 * @return true if the values are approximately equal within tolerance,
 *         false otherwise.
 */
inline bool almost_equal(double a, double b) {
  const double rel_diff = 0.0001;
  double greater = std::max(std::abs(a), std::abs(b));
  double diff = std::abs(a - b);

  return diff < rel_diff * greater;
}

}  // namespace test