test_that("as_ernest_rcrd works", {
  data(example_run)
  x <- as_ernest_rcrd(example_run)
  expect_equal(
    vctrs::fields(x),
    c("unit", "log_lik", "id", "evals", "birth_lik")
  )
  expect_equal(field(x, "log_lik"), example_run$weights$log_lik)
  expect_equal(field(x, "id"), example_run$weights$id)
  expect_equal(field(x, "evals"), example_run$weights$evaluations)
  expect_equal(field(x, "birth_lik"), example_run$weights$birth_lik)
  expect_equal(field(x, "unit"), example_run$samples$unit_cube)
  expect_equal(attr(x, "variables"), c("x", "y", "z"))
})

test_that("as.list ernest_rcrd works", {
  data(example_run)
  x <- as_ernest_rcrd(example_run)
  list_x <- as.list(x)
  expect_named(list_x, c("unit", "log_lik", "id", "evals", "birth_lik"))
  expect_equal(list_x$log_lik, example_run$weights$log_lik)
  expect_equal(colnames(list_x$unit), c("x", "y", "z"))
})

test_that("ernest_rcrd orders draws", {
  dead <- as_ernest_rcrd(example_run, keep_live = FALSE)
  live <- tail(as_ernest_rcrd(example_run), 1000)
  scrambled_d <- sample(dead, size = length(dead))
  scrambled_l <- sample(live, size = length(live))
  all <- c(scrambled_d, scrambled_l)
  all <- sort(all)
  expect_equal(field(all, "log_lik"), example_run$weights$log_lik)
  expect_equal(field(all, "unit"), example_run$samples$unit_cube)
  expect_equal(attr(all, "variables"), c("x", "y", "z"))
})

test_that("ernest_rcrd reports dimensional mismatch", {
  run2D <- generate(
    ernest_sampler(
      log_lik = gaussian_blobs$log_lik,
      prior = gaussian_blobs$prior,
      seed = 42
    ),
    max_iterations = 1000
  )
  run3D <- generate(
    ernest_sampler(
      log_lik = gaussian_3D$log_lik,
      prior = gaussian_3D$prior,
      seed = 42
    ),
    max_iterations = 1000
  )
  expect_error(
    c(as_ernest_rcrd(run2D), as_ernest_rcrd(run3D)),
    "`variables` attribute must match."
  )
})
