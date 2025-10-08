#include <limits>

#include "random_generator.h"
#include "testthat.h"
#include "utils.h"

using test::almost_equal;

context("Reflect functionality") {
  test_that("All values transform to 0.5") {
    Eigen::RowVectorXd test(6);
    test << -2.5, -1.5, -0.5, 0.5, 1.5, 2.5;
    random_generator::ReflectWithinUnitCube(test);
    expect_true((test.array() - 0.5).abs().maxCoeff() < 1e-4);
  }

  test_that("Negative values reflect") {
    Eigen::RowVectorXd test(3);
    test << -0.9, -0.1, -1.9;
    Eigen::RowVectorXd expected(3);
    expected << 0.9, 0.1, 0.9;
    random_generator::ReflectWithinUnitCube(test);

    expect_true(almost_equal(test[0], expected[0]));
    expect_true(almost_equal(test[1], expected[1]));
    expect_true(almost_equal(test[2], expected[2]));
  }
}

context("Point generators") {
  random_generator::RandomEngine rng;
  int n_points = 50;
  constexpr int kDimMax = 20;

  test_that("UniformOnSphere") {
    for (int n_dim = 1; n_dim <= kDimMax; n_dim++) {
      Eigen::RowVectorXd test(n_dim);
      int n_fail = 0;
      for (int i = 0; i < n_points; i++) {
        random_generator::UniformOnSphere(test);
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
        random_generator::UniformInBall(test);
        if (test.norm() > 1.0) {
          n_fail++;
        }
      }
      expect_true(n_fail == 0);
    }
  }

  test_that("UniformInEllipsoid") {
    for (int n_dim = 1; n_dim <= kDimMax; n_dim++) {
      // Random transformation matrix
      Eigen::MatrixXd trans(n_dim, n_dim);
      for (auto &a : trans.reshaped()) {
        a = unif_rand();
      }

      double scale = 2.0;
      Eigen::RowVectorXd loc(n_dim), test(n_dim);
      loc.fill(0.5);

      int n_fail = 0;
      for (int i = 0; i < n_points; i++) {
        random_generator::UniformInEllipsoid(trans, scale, loc, test);
        Eigen::RowVectorXd centered = (test - loc) / sqrt(scale);
        Eigen::RowVectorXd unit_space = centered * trans.inverse();
        if (unit_space.norm() > 1.0) {
          n_fail++;
        }
      }
      expect_true(n_fail == 0);
    }
  }
}