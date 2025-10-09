#include <limits>

#include "bounding.h"
#include "testthat.h"
#include "utils.h"

using test::almost_equal;

// Test utility functions
context("LogVolume function") {
  test_that("LogVolume calculates correct volume for unit sphere") {
    Eigen::VectorXd e_values = Eigen::VectorXd::Ones(2);
    double scale = 1.0;
    double log_vol = bounding::LogVolume(e_values, scale);
    double expected = log(M_PI);
    expect_true(almost_equal(log_vol, expected));
  }

  test_that("LogVolume scales correctly") {
    Eigen::VectorXd e_values = Eigen::VectorXd::Ones(3);
    double scale = 2.0;
    double log_vol = bounding::LogVolume(e_values, scale);
    double expected = log(4.0 / 3.0 * M_PI) + 1.5 * log(scale);
    expect_true(almost_equal(log_vol, expected));
  }
}

context("Ellipsoid function - basic functionality") {
  test_that("Ellipsoid handles insufficient points") {
    Eigen::MatrixXd X(1, 2);
    X << 0, 0;
    Eigen::RowVectorXd loc(2);
    Eigen::MatrixXd A(2, 2);
    Eigen::MatrixXd scaledInvSqrtA(2, 2);
    double scale, log_volume;

    bounding::Status result =
        bounding::Ellipsoid(X, loc, A, scaledInvSqrtA, scale, log_volume);
    expect_true(log_volume == R_NegInf);
    expect_true(result == bounding::Status::kFatal);
  }

  test_that("Ellipsoid computes correct center") {
    Eigen::MatrixXd X(3, 2);
    X << 0, 0, 2, 0, 0, 2;
    Eigen::RowVectorXd loc(2);
    Eigen::MatrixXd A(2, 2);
    Eigen::MatrixXd scaledInvSqrtA(2, 2);
    double scale, log_volume;

    bounding::Ellipsoid(X, loc, A, scaledInvSqrtA, scale, log_volume);

    expect_true(almost_equal(loc(0), 2.0 / 3.0));
    expect_true(almost_equal(loc(1), 2.0 / 3.0));
    expect_true(log_volume != R_NegInf);
  }
}

context("Ellipsoid function - bounding property") {
  test_that("Ellipsoid bounds all input points") {
    // Create a set of random points
    Eigen::MatrixXd X(10, 2);
    X << 1, 1, -1, 1, 1, -1, -1, -1, 0.5, 0.5, -0.5, 0.5, 0.5, -0.5, -0.5, -0.5,
        0, 1.5, 1.5, 0;

    Eigen::RowVectorXd loc(2);
    Eigen::MatrixXd A(2, 2);
    Eigen::MatrixXd scaledInvSqrtA(2, 2);
    double scale, log_volume;

    bounding::Ellipsoid(X, loc, A, scaledInvSqrtA, scale, log_volume);
    expect_true(log_volume != R_NegInf);

    // Verify all points are within the ellipsoid
    // Use the precision matrix A directly
    double max_dist = R_NegInf;
    bool non_pos_dist = FALSE;
    for (int i = 0; i < X.rows(); ++i) {
      Eigen::RowVectorXd diff = X.row(i) - loc;
      double dist = diff * A * diff.transpose();
      max_dist = Rf_fmax2(max_dist, dist);
      non_pos_dist = non_pos_dist || !(almost_equal(1.0, sign(dist)));
    }
    expect_true(max_dist <= scale + 0.0001);
    expect_false(non_pos_dist);
  }
}

context("Ellipsoid function - geometric cases") {
  test_that("Ellipsoid handles collinear points") {
    // Points on a line
    Eigen::MatrixXd X(4, 2);
    X << 0, 0, 1, 1, 2, 2, 3, 3;

    Eigen::RowVectorXd loc(2);
    Eigen::MatrixXd A(2, 2);
    Eigen::MatrixXd scaledInvSqrtA(2, 2);
    double scale, log_volume;

    bounding::Status result =
        bounding::Ellipsoid(X, loc, A, scaledInvSqrtA, scale, log_volume);
    expect_true(log_volume != R_NegInf);
    expect_true(result == bounding::Status::kNonInvertible);
  }

  test_that("Ellipsoid handles underconstrained case") {
    // Fewer points than dimensions + 1
    Eigen::MatrixXd X(2, 3);  // 2 points in 3D
    X << 0, 0, 0, 1, 0, 0;

    Eigen::RowVectorXd loc(3);
    Eigen::MatrixXd A(3, 3);
    Eigen::MatrixXd scaledInvSqrtA(3, 3);
    double scale, log_volume;

    bounding::Status result =
        bounding::Ellipsoid(X, loc, A, scaledInvSqrtA, scale, log_volume);
    expect_true(log_volume != R_NegInf);
    expect_true(result == bounding::Status::kUnderdetermined);
  }
}