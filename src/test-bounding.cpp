#include <limits>

#include "bounding.h"
#include "testthat.h"
#include "utils.h"

using test::almost_equal;

// Test utility functions
context("Ellipsoid class - basic functionality") {
  test_that("Ellipsoid replicates a known shape") {
    Eigen::Matrix2d shape{{2, 1}, {1, 1}};
    Eigen::Matrix2d inv_sqrt_shape{{0.8944272, -0.4472136},
                                   {-0.4472136, 1.3416408}};
    Eigen::RowVector2d center{0, 0};
    bounding::Ellipsoid ell(center, shape);
    expect_true(ell.n_dim() == 2);
    expect_true(ell.error() == bounding::Status::kOk);

    expect_true(ell.center().isApproxToConstant(0));
    expect_true(ell.shape().isApprox(shape, 1e-3));
    expect_true(almost_equal(ell.log_volume(), 1.14473));
    expect_true(ell.inv_sqrt_shape().isApprox(inv_sqrt_shape, 1e-3));

    Eigen::Vector2d oob{-0.5, -0.5}, inb{-0.5, 0.5};
    expect_false(ell.Covered(oob));
    expect_true(ell.Covered(inb));
    expect_true(ell.Covered(center));
  }

  test_that("Ellipsoid bounds all input points") {
    std::cout << "[TEST] Running: Ellipsoid bounds all input points"
              << std::endl;
    // Create a set of random points
    Eigen::MatrixXd X(10, 2);
    X << 0.934681506741446, -1.10934386692773, -0.0927026432043534,
        -0.165253071681541, 0.945919217628179, -1.17643629595198,
        0.779882729044357, -1.39204790238083, 0.668308362725138,
        -0.363965942433027, -0.970956925958582, 0.910219868950651,
        -0.41972737345736, -0.345625239953043, 0.800538882935828,
        -0.307907407476938, 0.0658319173280825, -0.95753533700075,
        0.936704248735548, -0.881101537651297;

    bounding::Ellipsoid ell(2);
    ell.Fit(X);
    std::cout << "Fit Sucessful" << std::endl;
    std::cout << ell.center() << std::endl;
    std::cout << ell.shape() << std::endl;
    std::cout << ell.inv_sqrt_shape() << std::endl;

    // Verify all points are within the ellipsoid
    // Use the precision matrix A directly
    X.transposeInPlace();
    for (auto col : X.colwise()) {
      expect_true(ell.Covered(col));
    }
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