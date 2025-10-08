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

context("ScaleFactor function") {
  test_that("ScaleFactor finds maximum distance") {
    Eigen::MatrixXd X(3, 2);
    X << 0, 0, 1, 0, 0, 1;
    Eigen::RowVectorXd loc = Eigen::RowVectorXd::Zero(2);
    Eigen::MatrixXd precision = Eigen::MatrixXd::Identity(2, 2);

    double scale = bounding::ScaleFactor(X, loc, precision);
    expect_true(almost_equal(scale, 1.0));
  }
}

context("GetTMatrix function") {
  test_that("GetTMatrix creates correct transformation") {
    Eigen::VectorXd e_values(2);
    e_values << 4.0, 1.0;
    Eigen::MatrixXd e_vectors = Eigen::MatrixXd::Identity(2, 2);

    Eigen::MatrixXd trans = bounding::GetTMatrix(e_values, e_vectors);

    expect_true(almost_equal(trans(0, 0), 2.0));  // sqrt(4)
    expect_true(almost_equal(trans(1, 1), 1.0));  // sqrt(1)
  }
}

context("Ellipsoid function - basic functionality") {
  test_that("Ellipsoid handles insufficient points") {
    Eigen::MatrixXd X(1, 2);
    X << 0, 0;
    Eigen::RowVectorXd loc(2);
    Eigen::MatrixXd cov(2, 2);
    Eigen::MatrixXd trans(2, 2);
    double scale, log_volume;

    int result = bounding::Ellipsoid(X, loc, cov, trans, scale, log_volume);
    expect_true(log_volume == R_NegInf);
    expect_true(result == bounding::Status::kFatal);
  }

  test_that("Ellipsoid computes correct center") {
    Eigen::MatrixXd X(3, 2);
    X << 0, 0, 2, 0, 0, 2;
    Eigen::RowVectorXd loc(2);
    Eigen::MatrixXd cov(2, 2);
    Eigen::MatrixXd trans(2, 2);
    double scale, log_volume;

    bounding::Ellipsoid(X, loc, cov, trans, scale, log_volume);

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
    Eigen::MatrixXd cov(2, 2);
    Eigen::MatrixXd trans(2, 2);
    double scale, log_volume;

    bounding::Ellipsoid(X, loc, cov, trans, scale, log_volume);
    expect_true(log_volume != R_NegInf);

    // Verify all points are within the ellipsoid
    // Transform to unit space and check distance <= scale
    Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> eigensystem(cov);
    Eigen::VectorXd e_values = eigensystem.eigenvalues();
    Eigen::MatrixXd e_vectors = eigensystem.eigenvectors();

    // Handle zero eigenvalues like the main function
    for (int i = 0; i < 1; i++) {
      if (e_values[i] <= 0.0) {
        e_values.head(i + 1).setConstant(e_values[i + 1] / 2.0);
      }
    }

    Eigen::MatrixXd inv_cov = e_vectors * e_values.cwiseInverse().asDiagonal() *
                              e_vectors.transpose();

    double max_dist = R_NegInf;
    bool non_pos_dist = FALSE;
    for (int i = 0; i < X.rows(); ++i) {
      Eigen::RowVectorXd diff = X.row(i) - loc;
      double dist = diff * inv_cov * diff.transpose();
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
    Eigen::MatrixXd cov(2, 2);
    Eigen::MatrixXd trans(2, 2);
    double scale, log_volume;

    int result = bounding::Ellipsoid(X, loc, cov, trans, scale, log_volume);
    expect_true(log_volume != R_NegInf);

    // Should handle the zero eigenvalue case
    Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> eigensystem(cov);
    Eigen::VectorXd e_values = eigensystem.eigenvalues();
    expect_true(e_values[0] > 0);  // After correction, should be positive
    expect_true(result == bounding::Status::kNonInvertible);
  }

  test_that("Ellipsoid handles underconstrained case") {
    // Fewer points than dimensions + 1
    Eigen::MatrixXd X(2, 3);  // 2 points in 3D
    X << 0, 0, 0, 1, 0, 0;

    Eigen::RowVectorXd loc(3);
    Eigen::MatrixXd cov(3, 3);
    Eigen::MatrixXd trans(3, 3);
    double scale, log_volume;

    int result = bounding::Ellipsoid(X, loc, cov, trans, scale, log_volume);
    expect_true(log_volume != R_NegInf);

    // Check that unconstrained eigenvalues were set
    Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> eigensystem(cov);
    Eigen::VectorXd e_values = eigensystem.eigenvalues();
    expect_true(almost_equal(e_values[0], e_values[1]));
    expect_true(result == bounding::Status::kSingular);
  }
}

context("Ellipsoid function - analytical cases") {
  test_that("Ellipsoid volume for unit square vertices") {
    // Vertices of a unit square
    Eigen::MatrixXd X(4, 2);
    X << 0, 0, 1, 0, 0, 1, 1, 1;

    Eigen::RowVectorXd loc(2);
    Eigen::MatrixXd cov(2, 2);
    Eigen::MatrixXd trans(2, 2);
    double scale, log_volume;

    bounding::Ellipsoid(X, loc, cov, trans, scale, log_volume);
    expect_true(log_volume != R_NegInf);

    // Center should be at (0.5, 0.5)
    expect_true(almost_equal(loc(0), 0.5));
    expect_true(almost_equal(loc(1), 0.5));

    // Volume should be reasonable (positive, finite)
    expect_true(R_FINITE(log_volume));
  }
}
