#include <iostream>
#include <limits>

#include "random_vector.h"
#include "testthat.h"

/**
 * @brief Compares two double values for approximate equality.
 *
 * @param a First double value to compare.
 * @param b Second double value to compare.
 * @return true if the values are approximately equal, false otherwise.
 */
bool almost_equal(double a, double b) {
  const double rel_diff = 0.0001;
  double greater = std::max(std::abs(a), std::abs(b));
  double diff = std::abs(a - b);

  return diff < rel_diff * greater;
};

context("Reflect functionality") {
  test_that("All values transform to 0.5") {
    Eigen::RowVectorXd test(6);
    test << -2.5, -1.5, -0.5, 0.5, 1.5, 2.5;
    random_vector::ReflectWithinUnitCube(test);
    expect_true((test.array() - 0.5).abs().maxCoeff() < 1e-4);
  }

  test_that("Negative values reflect") {
    Eigen::RowVectorXd test(3);
    test << -0.9, -0.1, -1.9;
    Eigen::RowVectorXd expected(3);
    expected << 0.9, 0.1, 0.9;
    random_vector::ReflectWithinUnitCube(test);

    expect_true(almost_equal(test[0], expected[0]));
    expect_true(almost_equal(test[1], expected[1]));
    expect_true(almost_equal(test[2], expected[2]));
  }
}

context("Point generators") {
  random_vector::RandomEngine rng;
  int n_points = 50;
  constexpr int kDimMax = 20;

  test_that("UniformOnSphere") {
    for (int n_dim = 1; n_dim <= kDimMax; n_dim++) {
      Eigen::RowVectorXd test(n_dim);
      int n_fail = 0;
      for (int i = 0; i < n_points; i++) {
        random_vector::UniformOnSphere(test);
        if (!almost_equal(test.norm(), 1.0)) {
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
        random_vector::UniformInBall(test);
        if (test.norm() > 1.0) {
          n_fail++;
        }
      }
      expect_true(n_fail == 0);
    }
  }

  test_that("UniformInEllipsoid") {
    for (int n_dim = 1; n_dim <= kDimMax; n_dim++) {
      // Random positive definite precision/covariance matrix
      Eigen::MatrixXd A = Eigen::MatrixXd::Random(n_dim, n_dim);
      Eigen::MatrixXd prec =
          A.transpose() * A + 0.1 * Eigen::MatrixXd::Identity(n_dim, n_dim);
      Eigen::MatrixXd cov = prec.inverse();

      double d2 = 2.0;
      Eigen::RowVectorXd loc(n_dim), test(n_dim);
      loc.fill(0.5);

      int n_fail = 0;
      for (int i = 0; i < n_points; i++) {
        random_vector::UniformInEllipsoid(cov, d2, loc, test);
        Eigen::RowVectorXd centered = test - loc;
        double mahalanobis_sq = centered * prec * centered.transpose();
        if (mahalanobis_sq > d2) {
          n_fail++;
        }
      }
      expect_true(n_fail == 0);
    }
  }
}