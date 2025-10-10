#include <limits>

#include "bounding.h"
#include "testthat.h"
#include "utils.h"

using test::almost_equal;

// Test utility functions
context("Ellipsoid class - basic functionality") {
  test_that("Ellipsoid handles insufficient points") {
    Eigen::MatrixXd X(1, 2);
    X << 0, 0;

    bounding::Ellipsoid ell(2);
    ell.Fit(X);

    expect_true(ell.log_volume() == R_NegInf);
    expect_true(ell.code() == bounding::Status::kFatal);
  }

  test_that("Ellipsoid computes correct center") {
    Eigen::MatrixXd X(3, 2);
    X << 0, 0, 2, 0, 0, 2;

    bounding::Ellipsoid ell(2);
    ell.Fit(X);

    expect_true(almost_equal(ell.loc()(0), 2.0 / 3.0));
    expect_true(almost_equal(ell.loc()(1), 2.0 / 3.0));
    expect_true(ell.log_volume() != R_NegInf);
  }
}

context("Ellipsoid class - bounding property") {
  test_that("Ellipsoid bounds all input points") {
    // Create a set of random points
    Eigen::MatrixXd X(10, 2);
    X << 1, 1, -1, 1, 1, -1, -1, -1, 0.5, 0.5, -0.5, 0.5, 0.5, -0.5, -0.5, -0.5,
        0, 1.5, 1.5, 0;

    bounding::Ellipsoid ell(2);
    ell.Fit(X);
    expect_true(ell.log_volume() != R_NegInf);

    // Verify all points are within the ellipsoid
    // Use the precision matrix A directly
    double max_dist = R_NegInf;
    bool non_pos_dist = FALSE;
    for (int i = 0; i < X.rows(); ++i) {
      Eigen::RowVectorXd diff = X.row(i) - ell.loc();
      double dist = diff * ell.A() * diff.transpose();
      max_dist = Rf_fmax2(max_dist, dist);
      non_pos_dist = non_pos_dist || !(almost_equal(1.0, sign(dist)));
    }
    expect_true(max_dist <= ell.scale() + 0.0001);
    expect_false(non_pos_dist);
  }
}

context("Ellipsoid class - geometric cases") {
  test_that("Ellipsoid handles collinear points") {
    // Points on a line
    Eigen::MatrixXd X(4, 2);
    X << 0, 0, 1, 1, 2, 2, 3, 3;

    bounding::Ellipsoid ell(2);
    ell.Fit(X);

    expect_true(ell.log_volume() != R_NegInf);
    expect_true(ell.code() == bounding::Status::kNonInvertible);
  }

  test_that("Ellipsoid handles underconstrained case") {
    // Fewer points than dimensions + 1
    Eigen::MatrixXd X(2, 3);  // 2 points in 3D
    X << 0, 0, 0, 1, 0, 0;

    bounding::Ellipsoid ell(3);
    ell.Fit(X);

    expect_true(ell.log_volume() != R_NegInf);
    expect_true(ell.code() == bounding::Status::kUnderdetermined);
  }
}