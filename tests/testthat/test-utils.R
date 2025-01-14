test_that("merge_transformations works correctly", {
  # Create mock prior_transform objects
  prior_transform1 <- prior_transform(
    fn = \(x) x + 1,
    name = "transform1"
  )

  prior_transform2 <- prior_transform(
    distributions3::Normal(),
    name = "transform2"
  )

  prior_transforms <- list(prior_transform1, prior_transform2)

  result <- merge_transformations(prior_transforms)

  # Check that composite_fn works correctly
  expect_equal(result$composite_fn(c(0.5, 0.5)), c(1.5, 0))

  # Check that names are correct
  expect_equal(result$names, c("transform1", "transform2"))

  # Check that supports are correct
  expect_equal(result$supports, list(c(1, 2), c(-Inf, Inf)))

  # Test with missing names
  prior_transform3 <- prior_transform(
    \(x) x - 1,
  )

  prior_transforms <- list(prior_transform1, prior_transform3)

  result <- merge_transformations(prior_transforms)

  # Check that names are generated correctly
  expect_equal(result$names, c("transform1", "V"))

  # Check that supports are correct
  expect_equal(result$supports, list(c(1, 2), c(-1, 0)))
})

test_that("merge_transformations throws error for invalid input", {
  invalid_transform <- list(
    fn = \(x) x + 1,
    name = "invalid",
    support = c(0, 1)
  )
  # Not setting class to "prior_transform"

  prior_transforms <- list(invalid_transform)

  expect_error(merge_transformations(prior_transforms),
               "All elements of prior_transforms must be of class 'prior_transform'")
})
