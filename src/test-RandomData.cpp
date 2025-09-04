#include "RandomData.h"
#include <iostream>
#include "testthat.h"
#include <limits>

bool almost_equal(double A, double B) {
  double diff = fabs(A - B);
  A = fabs(A);
  B = fabs(B);
  // Find the largest
  double largest = (B > A) ? B : A;

  return diff <= largest * std::numeric_limits<double>::epsilon();
}

context("Reflect functionality") {
  test_that("All values transform to 0.5") {
    cpp11::writable::doubles test = {-2.5, -1.5, -0.5, 0.5, 1.5, 2.5};
    RandomData::reflect(test);
    expect_true(
      std::all_of(
        test.cbegin(),
        test.cend(),
        [](double i) { return almost_equal(i, 0.5); }
      )
    );
  }

  test_that("Negative values reflect") {
    cpp11::writable::doubles test = {-0.9, -0.1, -1.9};
    cpp11::writable::doubles expected = {0.9, 0.1, 0.9};
    RandomData::reflect(test);

    expect_true(almost_equal(test[0], expected[0]));
    expect_true(almost_equal(test[1], expected[1]));
    expect_true(almost_equal(test[2], expected[2]));
  }
}
