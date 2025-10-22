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
    expect_true(almost_equal(ell.log_volume(), 1.14473));
  }

  test_that("Distance to point works") {
    int n_dim = 10;
    int n_test = 100;  // Reduce number of tests for faster runtime
    double err_dist = 0.0, err_out = 0.0, err_in = 0.0;
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
      err_dist = Rf_fmax2(err_dist, err);

      // Compute closest point
      Vector close = ell.Closest(p);
      double dist_closest = ell.Distance(close);
      if (s <= 1.0) {
        // If o is outside Ell, the dist(close) should be 1.
        err_out = Rf_fmax2(err_out, std::abs(dist_closest - 1.0));
      } else {
        // If p is inside Ell, then dist(close) should be equal to dist(p).
        err_in = Rf_fmax2(err_in, std::abs(dist_closest - s));
      }
    }
    expect_true(err_dist < 1e-3);
    expect_true(err_in < 1e-3);
    expect_true(err_out < 1e-3);
  }

  Eigen::Matrix2d shape = Eigen::Matrix2d::Identity();

  test_that("Two barely touching ellipsoids") {
    Eigen::RowVector2d center1{0, 0};
    Eigen::RowVector2d center2{2, 0};

    bounding::Ellipsoid ell1(center1, shape);
    bounding::Ellipsoid ell2(center2, shape);

    double dist = ell1.Distance(ell2);

    // Distance should be approximately R_NegInf (touching)
    expect_true(dist == R_NegInf);
  }

  test_that("Two overlapping ellipsoids") {
    Eigen::RowVector2d center1{0, 0};
    Eigen::RowVector2d center2{1, 0};

    bounding::Ellipsoid ell1(center1, shape);
    bounding::Ellipsoid ell2(center2, shape);

    double dist = ell1.Distance(ell2);

    // Distance should be R_NegInf (overlapping)
    expect_true(dist == R_NegInf);
  }

  test_that("Two non-overlapping ellipsoids") {
    Eigen::RowVector2d center1{0, 0};
    Eigen::RowVector2d center2{2.01, 0};

    bounding::Ellipsoid ell1(center1, shape);
    bounding::Ellipsoid ell2(center2, shape);

    double dist = ell1.Distance(ell2);
    expect_true(almost_equal(dist, 0.01));
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
    expect_true(ell.error() == bounding::Status::kDegenerate);
  }

  test_that("Ellipsoid handles underconstrained case") {
    Eigen::MatrixXd X(2, 3);
    X << 0, 0, 0, 1, 0, 0;

    bounding::Ellipsoid ell(3);
    ell.Fit(X);

    expect_true(ell.log_volume() != R_NegInf);
    expect_true(ell.error() == bounding::Status::kUnderdetermined);
  }
}