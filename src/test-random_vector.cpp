#include <limits>

#include "random_vector.h"
#include "testthat.h"

/**
 * @brief Compares two double values for approximate equality.
 *
 * @param a First double value to compare.
 * @param b Second double value to compare.
 * @return true if the values are approximately equal, false otherwise.
 */
bool almost_equal(double a, double b) {
  const double rel_diff = 0.0001;
  double greater = std::max(std::abs(a), std::abs(b));
  double diff = std::abs(a - b);

  return diff < rel_diff * greater;
};

context("Reflect functionality") {
  test_that("All values transform to 0.5") {
    cpp11::writable::doubles test = {-2.5, -1.5, -0.5, 0.5, 1.5, 2.5};
    random_vector::ReflectWithinUnitCube(test);
    expect_true(std::all_of(test.cbegin(), test.cend(),
                            [](double i) { return almost_equal(i, 0.5); }));
  }

  test_that("Negative values reflect") {
    cpp11::writable::doubles test = {-0.9, -0.1, -1.9};
    cpp11::writable::doubles expected = {0.9, 0.1, 0.9};
    random_vector::ReflectWithinUnitCube(test);

    expect_true(almost_equal(test[0], expected[0]));
    expect_true(almost_equal(test[1], expected[1]));
    expect_true(almost_equal(test[2], expected[2]));
  }
}

context("Point generators") {
  random_vector::RandomEngine rng;
  int n_points = 1000;
  constexpr int DIM_MAX = 20;

  test_that("random in ball") {
    for (int n_dim = 1; n_dim < DIM_MAX; n_dim++) {
      int n_fails = 0;
      cpp11::writable::doubles rand(n_dim);
      for (int p = 0; p < n_points; p++) {
        random_vector::UniformInBall(rand);
        double norm = lapack_wrapper::dnrm2(rand);
        if (norm >= 1.0) {
          n_fails++;
        }
      }
      expect_true(n_fails == 0);
    }
  }
}

/**
 * A simple helper function for generating points from an ellipsoid.
 */
[[cpp11::register]]
cpp11::doubles test_ellipsoid(cpp11::doubles_matrix<> chol_precision,
                              cpp11::doubles loc, double d2) {
  cpp11::writable::doubles rand(loc.size());
  random_vector::UniformInEllipsoid(chol_precision, loc, d2, rand);
  return rand;
}