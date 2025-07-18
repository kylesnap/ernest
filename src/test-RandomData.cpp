#include "RandomData.h"
#include <iostream>
#include "testthat.h"

context("RandomData basic functionality") {

test_that("uniform_in_cube returns points in cube") {
  for (int dim = 1; dim <= 10; ++dim) {
    cpp11::writable::doubles_matrix<> cube_pts(dim, 5);
    RandomData::uniform_in_cube(cube_pts);
    bool over = false;
    for (auto i : cube_pts) {
      for (auto &&j : i) {
        over = over || (j < 0.0 || j > 1.0);
      }
    }
    expect_false(over);
  }
}

test_that("uniform_in_sphere returns points in sphere") {
  for (int dim = 1; dim <= 10; ++dim) {
    cpp11::writable::doubles_matrix<> sphere_pts(dim, 5);
    RandomData::uniform_in_sphere(sphere_pts);
    for (auto i : sphere_pts) {
      double norm = 0.0;
      for (auto &&j : i) {
        norm += j * j;
      }
      expect_true(norm <= 1.0);
    }
  }
}

}
