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

  test_that("Distance works") {
    int n_dim = 10;
    int n_test = 1000;  // Reduce number of tests for faster runtime
    double err_max = 0.0;
    random_generator::RandomEngine gen;

    // Preallocate matrices/vectors to avoid repeated allocations
    Vector center(n_dim);
    Matrix u(n_dim, n_dim);
    Vector p(n_dim);

    for (int i_test = 0; i_test < n_test; i_test++) {
      // Random ellipsoid center and SPD matrix
      random_generator::RUnif(center);
      std::generate(u.reshaped().begin(), u.reshaped().end(), unif_rand);
      Matrix a = u * u.transpose();

      bounding::Ellipsoid ell(center, a);
      Matrix L = ell.matrixL();

      // Random unit direction
      random_generator::RUnif(p);
      p.array() -= 0.5;
      p.normalize();

      // transform p using Cholesky factor
      L.transpose().triangularView<Eigen::Upper>().solveInPlace(p);
      double sin = 2 * unif_rand();

      // position point at xc + (1/sin) * direction
      p = (p / sin) + center;

      // Compute distance from p to ellipsoid defined by (xc, g)
      double s = ell.Distance(p);
      double err = abs(s - sin);
      err_max = Rf_fmax2(err_max, err);
    }
    expect_true(err_max < 1e-3);
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
  }

  test_that("Ellipsoid handles underconstrained case") {
    std::cout << "[TEST] Running: Ellipsoid handles underconstrained case"
              << std::endl;
    Eigen::MatrixXd X(2, 3);
    X << 0, 0, 0, 1, 0, 0;

    bounding::Ellipsoid ell(3);
    ell.Fit(X);

    expect_true(ell.log_volume() != R_NegInf);
    expect_true(ell.error() == bounding::Status::kUnderdetermined);
    std::cout << "[TEST] Completed: Ellipsoid handles underconstrained case"
              << std::endl;
  }
}