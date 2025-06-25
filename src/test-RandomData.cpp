#include "RandomData.hpp"
#include "testthat.h"

context("RandomData basic functionality") {

test_that("unif_rand_n returns random numbers") {
  for (int i = 0; i < 10; ++i) {
    double *u = RandomData::unif_rand_n(i);
    expect_true(
      std::all_of(u, u + i, [](double x) { return x >= 0.0 && x < 1.0; })
    );
    delete[] u;
  }
}

test_that("unif_rand_n (in-place) fills array") {
  for (int i = 0; i < 10; ++i) {
    double uvec[i];
    RandomData::unif_rand_n(i, uvec);
    expect_true(
      std::all_of(uvec, uvec + i, [](double x) { return x >= 0.0 && x < 1.0; })
    );
  }
}

test_that("norm_rand_n returns normal random numbers") {
  for (int i = 0; i < 10; ++i) {
    double *z = RandomData::norm_rand_n(i);
    expect_true(
      std::all_of(z, z + i, [](double x) { return x >= -3.0 && x <= 3.0; })
    );
    delete[] z;
  }
}

test_that("uniform_in_sphere returns points in sphere") {
  for (int dim = 1; dim <= 10; ++dim) {
    int num_points = 5;
    double *sphere_pts = RandomData::uniform_in_sphere(dim, num_points);
    for (int j = 0; j < num_points; ++j) {
      double norm = 0.0;
      for (int i = 0; i < dim; ++i) {
        norm += sphere_pts[i + j * dim] * sphere_pts[i + j * dim];
      }
      norm = std::sqrt(norm);
      expect_true(norm <= 1.0); // Points should be within the unit sphere
    }
    delete[] sphere_pts;
  }
}

test_that("uniform_in_cube returns points in cube") {
  for (int dim = 1; dim <= 10; ++dim) {
    int num_points = 5;
    double *cube_pts = RandomData::uniform_in_cube(dim, num_points);
    for (int j = 0; j < num_points; ++j) {
      for (int i = 0; i < dim; ++i) {
        expect_true((cube_pts[i + j * dim] >= 0.0 && cube_pts[i + j * dim] < 1.0));
      }
    }
    delete[] cube_pts;
  }
}

test_that("dpotrf and dpotrs solve SPD system") {
  double A[9] = {1, 0, 1, 0, 2, 0, 1, 0, 3};
  int info = RandomData::dpotrf(A, 3, 3);
  expect_true(info == 0);
  double expected_A[9] = {1, 0, 1, 0, sqrt(2), 0, 1, 0, sqrt(2)};
  expect_true(
    std::equal(A, A + 9, expected_A, [](double a, double b) { return std::abs(a - b) < 1e-6; })
  );
  double b[3] = {1, 2, 3};
  double expected_sol[3] = {0, 1, 1};
  RandomData::dpotrs(A, 3, 3, b);
  expect_true(
    std::equal(b, b + 2, expected_sol, [](double a, double b) { return std::abs(a - b) < 1e-6; })
  );
}

test_that("uniform_in_ellipsoid returns points in ellipsoid") {
  double ellipsoid_A[4] = {3.0, 1.0, 1.0, 2.0};
  double ellipsoid_r = 1.0;
  int seed = 123;
  double *ellipsoid_pts = RandomData::uniform_in_ellipsoid(2, 3, ellipsoid_A, ellipsoid_r, seed);
  for (int j = 0; j < 3; ++j) {
      double norm = 0.0;
      for (int i = 0; i < 2; ++i) {
          norm += ellipsoid_pts[i + j * 2] * ellipsoid_pts[i + j * 2] / ellipsoid_A[i + i * 2];
      }
      expect_true(norm <= ellipsoid_r * ellipsoid_r); // Points should be within the ellipsoid
  }
}

} // context
