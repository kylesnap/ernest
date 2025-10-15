#include <limits>

#include "bounding.h"
#include "testthat.h"
#include "utils.h"

using test::almost_equal;

// Test utility functions
context("Ellipsoid class - basic functionality") {
  test_that("Ellipsoid handles insufficient points") {
    std::cout << "[TEST] Running: Ellipsoid handles insufficient points"
              << std::endl;
    Eigen::MatrixXd X(1, 2);
    X << 0, 0;

    bounding::Ellipsoid ell(2);
    ell.Fit(X);

    expect_true(ell.log_volume() == R_NegInf);
    expect_true(ell.error() == bounding::Status::kFatal);
    std::cout << "[TEST] Completed: Ellipsoid handles insufficient points"
              << std::endl;
  }

  test_that("Ellipsoid replicates a known shape") {
    Eigen::Matrix2d shape{{3, 1}, {1, 2}};
    Eigen::Matrix2d inv_shape{{0.4, -0.2}, {-0.2, 0.6}};
    Eigen::Matrix2d sqrt_shape{{1.7013016, 0.3249197}, {0.3249197, 1.3763819}};

    Eigen::RowVector2d center{0, 0};
    std::cout << "[TEST] Running: Ellipsoid replicates a known shape"
              << std::endl;
    bounding::Ellipsoid ell(center, shape);
    expect_true(ell.n_dim() == 2);
    expect_true(ell.error() == bounding::Status::kOk);

    expect_true(shape.isApprox(ell.shape()));
    expect_true(ell.center().isApproxToConstant(0));
    expect_true(inv_shape.isApprox(ell.inverse_shape()));
    expect_true(sqrt_shape.isApprox(ell.sqrt_shape(), 0.0001));

    expect_true(almost_equal(ell.log_volume(), 1.949449));
    std::cout << "[TEST] Completed: Ellipsoid replicates a known shape"
              << std::endl;
  }
}

context("Ellipsoid class - bounding property") {
  test_that("Ellipsoid bounds all input points") {
    std::cout << "[TEST] Running: Ellipsoid bounds all input points"
              << std::endl;
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
      Eigen::RowVectorXd diff = X.row(i) - ell.center();
      double dist = diff * ell.inverse_shape() * diff.transpose();
      max_dist = Rf_fmax2(max_dist, dist);
      non_pos_dist = non_pos_dist || !(almost_equal(1.0, sign(dist)));
    }
    std::cout << "MAX DISTANCE: " << max_dist << std::endl;
    expect_true(max_dist <= 1.0001);
    expect_false(non_pos_dist);
    std::cout << "[TEST] Completed: Ellipsoid bounds all input points"
              << std::endl;
  }
}

context("Ellipsoid class - geometric cases") {
  test_that("Ellipsoid handles collinear points") {
    std::cout << "[TEST] Running: Ellipsoid handles collinear points"
              << std::endl;
    // Points on a line
    Eigen::MatrixXd X(4, 2);
    X << 0, 0, 1, 1, 2, 2, 3, 3;

    bounding::Ellipsoid ell(2);
    ell.Fit(X);

    expect_true(ell.log_volume() != R_NegInf);
    expect_true(ell.error() == bounding::Status::kDegenerate);
    std::cout << "[TEST] Completed: Ellipsoid handles collinear points"
              << std::endl;
  }

  test_that("Ellipsoid handles underconstrained case") {
    std::cout << "[TEST] Running: Ellipsoid handles underconstrained case"
              << std::endl;
    // Fewer points than dimensions + 1
    Eigen::MatrixXd X(2, 3);  // 2 points in 3D
    X << 0, 0, 0, 1, 0, 0;

    bounding::Ellipsoid ell(3);
    ell.Fit(X);

    expect_true(ell.log_volume() != R_NegInf);
    expect_true(ell.error() == bounding::Status::kUnderdetermined);
    std::cout << "[TEST] Completed: Ellipsoid handles underconstrained case"
              << std::endl;
  }
}