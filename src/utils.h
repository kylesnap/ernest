// File: /Users/ksnap/Projects/ernest/src/utils.h
// Created Date: Tuesday, October 14th 2025
// Author: Kyle Dewsnap
//
// Copyright (c) 2025 Kyle Dewsnap
// GNU General Public License v3.0 or later
// https://www.gnu.org/licenses/gpl-3.0-standalone.html
//
// Utility functions and type definitions for the Ernest project.
#pragma once
#include <Rmath.h>

#include <cpp11.hpp>
#include <cpp11eigen.hpp>

namespace ern {
constexpr double kPrecision = Eigen::NumTraits<double>::dummy_precision();
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

// Fuzzy compare `v` == `w`.
inline bool WithinRel(
    const double v, const double w,
    const double prec = 100 * Eigen::NumTraits<double>::dummy_precision()) {
  return abs(v - w) <= (prec * fmin2(abs(v), abs(w)));
}

// Fuzzy compare `v` to zero.
inline bool isZero(
    const double v,
    const double prec = Eigen::NumTraits<double>::dummy_precision()) {
  return abs(v) <= prec;
}

}  // namespace ern

namespace cpp11 {

// Convert the R doubles vector `x` to an Eigen row vector.
inline ern::RowVector as_row_vector(const cpp11::doubles& x) {
  return Eigen::Map<ern::RowVector>(REAL(x.data()), x.size());
}

// Convert the Eigen row vector `x` to an R doubles vector.
inline cpp11::doubles as_row_doubles(const ern::ConstRef<ern::RowVector>& x) {
  return cpp11::as_doubles(cpp11::as_sexp(x));
}

}  // namespace cpp11