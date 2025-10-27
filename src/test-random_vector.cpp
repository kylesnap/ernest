// File: /Users/ksnap/Projects/ernest/src/test-random_vector.cpp
// Created Date: Tuesday, October 14th 2025
// Author: Kyle Dewsnap
//
// Copyright (c) 2025 Kyle Dewsnap
// GNU General Public License v3.0 or later
// https://www.gnu.org/licenses/gpl-3.0-standalone.html
//
// Test cases for random vector generation and reflection functions.
#include <limits>

#include "random_generator.h"
#include "testthat.h"
#include "utils.h"

context("Reflect functionality") {
  test_that("All values transform to 0.5") {
    Eigen::RowVectorXd test(6);
    test << -2.5, -1.5, -0.5, 0.5, 1.5, 2.5;
    ern::ReflectWithinUnitCube(test);
    expect_true((test.array() - 0.5).abs().maxCoeff() < 1e-4);
  }

  test_that("Negative values reflect") {
    Eigen::RowVectorXd test(3);
    test << -0.9, -0.1, -1.9;
    Eigen::RowVectorXd expected(3);
    expected << 0.9, 0.1, 0.9;
    ern::ReflectWithinUnitCube(test);

    expect_true(test.isApprox(expected));
  }
}

context("Point generators") {
  ern::RandomEngine rng;
  int n_points = 50;
  constexpr int kDimMax = 20;

  test_that("UniformOnSphere") {
    for (int n_dim = 1; n_dim <= kDimMax; n_dim++) {
      Eigen::RowVectorXd test(n_dim);
      int n_fail = 0;
      for (int i = 0; i < n_points; i++) {
        ern::UniformOnSphere(test);
        if (!ern::WithinRel(test.norm(), 1)) {
          n_fail++;
        }
      }
      expect_true(n_fail == 0);
    }
  }

  test_that("UniformInBall") {
    for (int n_dim = 1; n_dim <= kDimMax; n_dim++) {
      Eigen::RowVectorXd test(n_dim);
      int n_fail = 0;
      for (int i = 0; i < n_points; i++) {
        ern::UniformInBall(test);
        if (test.norm() > 1.0) {
          n_fail++;
        }
      }
      expect_true(n_fail == 0);
    }
  }
}