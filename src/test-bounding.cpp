// File: /Users/ksnap/Projects/ernest/src/test-bounding.cpp
// Created Date: Monday, October 20th 2025
// Author: Kyle Dewsnap
//
// Copyright (c) 2025 Kyle Dewsnap
// GNU General Public License v3.0 or later
// https://www.gnu.org/licenses/gpl-3.0-standalone.html
//
// Test cases for ellipsoids and rectangles.
#include <limits>

#include "ellipsoid.h"
#include "rectangle.h"
#include "testthat.h"
#include "utils.h"

// Test utility functions
context("Ellipsoid class - basic functionality") {
  test_that("Ellipsoid replicates a known shape") {
    Eigen::Matrix2d shape{{2, 1}, {1, 1}};
    Eigen::Matrix2d inv_sqrt_shape{{0.8944272, -0.4472136}, {-0.4472136, 1.3416408}};
    Eigen::RowVector2d center{0, 0};
    vol::Ellipsoid ell(center, shape);
    expect_true(ell.n_dim() == 2);
    expect_true(ell.error() == vol::Status::kOk);

    expect_true(ell.center().isApproxToConstant(0));
    expect_true(ell.shape().isApprox(shape, 1e-3));
    expect_true(ern::WithinRel(ell.log_volume(), 1.14473, 1e-5));
    expect_true(ell.inv_sqrt_shape().isApprox(inv_sqrt_shape, 1e-3));

    Eigen::Vector2d oob{-0.5, -0.5}, inb{-0.5, 0.5};
    expect_false(ell.Covered(oob));
    expect_true(ell.Covered(inb));
    expect_true(ell.Covered(center));
    expect_true(ern::WithinRel(ell.log_volume(), 1.14473, 1e-5));
  }

  test_that("Distance to point works") {
    int n_dim = 10;
    int n_test = 100;  // Reduce number of tests for faster runtime
    double err_dist = 0.0, err_out = 0.0, err_in = 0.0;
    ern::RandomEngine gen;

    // Preallocate matrices/vectors to avoid repeated allocations
    Eigen::VectorXd center(n_dim);
    Eigen::MatrixXd u(n_dim, n_dim);
    Eigen::VectorXd p(n_dim);

    for (int i_test = 0; i_test < n_test; i_test++) {
      // Random ellipsoid center and SPD matrix
      ern::RUnif(center);
      std::generate(u.reshaped().begin(), u.reshaped().end(), unif_rand);
      Eigen::MatrixXd a = u * u.transpose();

      vol::Ellipsoid ell(center, a);
      Eigen::MatrixXd L = ell.matrixL();

      // Random unit direction
      ern::RUnif(p);
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
      Eigen::VectorXd close = ell.Closest(p);
      double dist_closest = ell.Distance(close);
      if (s <= 1.0) {
        // If o is outside Ell, the dist(close) should be 1.
        err_out = Rf_fmax2(err_out, std::abs(dist_closest - 1.0));
      } else {
        // If p is inside Ell, then dist(close) should be equal to dist(p).
        err_in = Rf_fmax2(err_in, std::abs(dist_closest - s));
      }
    }
    expect_true(ern::isZero(err_dist, 1e-3));
    expect_true(ern::isZero(err_in, 1e-3));
    expect_true(ern::isZero(err_out, 1e-3));
  }

  Eigen::Matrix2d shape = Eigen::Matrix2d::Identity();

  test_that("Two barely touching ellipsoids") {
    Eigen::RowVector2d center1{0, 0};
    Eigen::RowVector2d center2{2, 0};

    vol::Ellipsoid ell1(center1, shape);
    vol::Ellipsoid ell2(center2, shape);

    expect_true(ell1.Intersects(ell2));
  }

  test_that("Two overlapping ellipsoids") {
    Eigen::RowVector2d center1{0, 0};
    Eigen::RowVector2d center2{1, 0};

    vol::Ellipsoid ell1(center1, shape);
    vol::Ellipsoid ell2(center2, shape);

    expect_true(ell1.Intersects(ell2));
  }

  test_that("Two non-overlapping ellipsoids") {
    Eigen::RowVector2d center1{0, 0};
    Eigen::RowVector2d center2{2.01, 0};

    vol::Ellipsoid ell1(center1, shape);
    vol::Ellipsoid ell2(center2, shape);

    expect_false(ell1.Intersects(ell2));
  }
}

context("Ellipsoid class - geometric cases") {
  test_that("Ellipsoid handles collinear points") {
    // Points on a line
    Eigen::MatrixXd X(4, 2);
    X << 0, 0, 1, 1, 2, 2, 3, 3;

    vol::Ellipsoid ell(2);
    ell.Fit(X);

    expect_true(ell.log_volume() != R_NegInf);
    expect_true(ell.error() == vol::Status::kDegenerate);
  }

  test_that("Ellipsoid handles underconstrained case") {
    Eigen::MatrixXd X(2, 3);
    X << 0, 0, 0, 1, 0, 0;

    vol::Ellipsoid ell(3);
    ell.Fit(X);

    expect_true(ell.log_volume() != R_NegInf);
    expect_true(ell.error() == vol::Status::kUnderdetermined);
  }
}

context("Rectangle class - basic functionality") {
  test_that("Rectangle coverage detection works") {
    vol::Rectangle rect(2);

    // Points inside
    expect_true(rect.Covered(Eigen::Vector2d{0.5, 0.5}));
    expect_true(rect.Covered(Eigen::Vector2d{0.0, 0.0}));
    expect_true(rect.Covered(Eigen::Vector2d{1.0, 1.0}));

    // Points outside
    expect_false(rect.Covered(Eigen::Vector2d{-0.1, 0.5}));
    expect_false(rect.Covered(Eigen::Vector2d{0.5, 1.1}));
    expect_false(rect.Covered(Eigen::Vector2d{1.5, 1.5}));
  }

  test_that("Rectangle uniform sampling produces valid points") {
    int n_dim = 5;
    int n_samples = 100;
    vol::Rectangle rect(n_dim);

    Eigen::VectorXd sample(n_dim);
    int n_oob = 0;
    for (int i = 0; i < n_samples; i++) {
      rect.UniformSample(sample);
      n_oob += !(rect.Covered(sample));
    }
    expect_true(n_oob == 0);

    n_oob = 0;
    for (int i = 0; i < n_samples / 10; i++) {
      sample = rect.UniformSample();
      n_oob += !(rect.Covered(sample));
    }
    expect_true(n_oob == 0);
  }
}

context("Rectangle class - shrinking") {
  test_that("Rectangle shrinks correctly in two dimensions") {
    vol::Rectangle rect(2);
    Eigen::Vector2d inner{0.5, 0.5};
    Eigen::Vector2d outer{0.3, 0.9};
    expect_true(rect.Clamp(inner, outer));

    expect_true(rect.lower().isApprox(Eigen::Vector2d{0.3, 0}));
    expect_true(rect.upper().isApprox(Eigen::Vector2d{1, 0.9}));

    // Old maxima falls outside cube.
    expect_false(rect.Covered(Eigen::Vector2d{1.0, 1.0}));
    expect_true(rect.Covered(inner));
    expect_true(rect.Covered(outer));
  }

  test_that("Rectangle doesn't react to OOB arguments") {
    vol::Rectangle rect(2);
    Eigen::Vector2d inner{0.5, 0.5};
    Eigen::Vector2d outer{0.3, 0.9};
    expect_true(rect.Clamp(inner, outer));

    expect_false(rect.Clamp(inner, Eigen::Vector2d{1.0, 1.0}));
    expect_true(rect.lower().isApprox(Eigen::Vector2d{0.3, 0}));
    expect_true(rect.upper().isApprox(Eigen::Vector2d{1, 0.9}));

    expect_false(rect.Clamp(Eigen::Vector2d{1.0, 1.0}, outer));
    expect_true(rect.lower().isApprox(Eigen::Vector2d{0.3, 0}));
    expect_true(rect.upper().isApprox(Eigen::Vector2d{1, 0.9}));

    expect_true(rect.Clamp(inner, outer));
    expect_true(rect.lower().isApprox(Eigen::Vector2d{0.3, 0}));
    expect_true(rect.upper().isApprox(Eigen::Vector2d{1, 0.9}));
  };

  test_that("Rectangle can be clamped in higher dimensions") {
    typedef Eigen::Matrix<double, 5, 1> Vector5d;
    int n_dim = 5;
    vol::Rectangle rect(n_dim);

    // First clamp
    Vector5d inner = Vector5d::Constant(0.5);
    Vector5d outer{0.2, 0.8, 0.3, 0.9, 0.4};
    expect_true(rect.Clamp(inner, outer));
    expect_true(rect.lower().isApprox(Vector5d{0.2, 0, 0.3, 0, 0.4}));
    expect_true(rect.upper().isApprox(Vector5d{1, 0.8, 1, 0.9, 1}));
    expect_true(rect.Covered(outer));

    // Second clamp with different inner/outer
    inner << 0.5, 0.6, 0.5, 0.7, 0.6;
    outer << 0.3, 0.6, 0.7, 0.7, 0.85;
    expect_true(rect.Clamp(inner, outer));
    expect_true(rect.lower().isApprox(Vector5d{0.3, 0, 0.3, 0, 0.4}));
    expect_true(rect.upper().isApprox(Vector5d{1, 0.8, 0.7, 0.9, 0.85}));
    expect_true(rect.Covered(outer));
  }
}