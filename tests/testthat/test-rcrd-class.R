test_that("as_ernest_rcrd works", {
  data(example_run)
  x <- as_ernest_rcrd(example_run)
  ref <- as_ernest_rcrd(example_run)
  expect_equal(
    vctrs::fields(x),
    c("unit", "log_lik", "id", "nlive", "evals", "birth_lik")
  )
  expect_equal(field(x, "log_lik"), field(ref, "log_lik"))
  expect_equal(field(x, "id"), field(ref, "id"))
  expect_equal(field(x, "evals"), field(ref, "evals"))
  expect_equal(field(x, "birth_lik"), field(ref, "birth_lik"))
  unit <- example_run$samples$unit_cube
  dimnames(unit) <- NULL
  expect_equal(field(x, "unit"), unit)
  expect_identical(attr(x, "nvariables"), 3L)
  expect_snapshot(x)
})

test_that("as.list ernest_rcrd works", {
  data(example_run)
  x <- as_ernest_rcrd(example_run)
  list_x <- as.list(x)
  expect_named(
    list_x,
    c("unit", "log_lik", "id", "nlive", "evals", "birth_lik")
  )
  expect_equal(list_x$log_lik, field(as_ernest_rcrd(example_run), "log_lik"))
})

test_that("ernest_rcrd orders draws", {
  dead <- as_ernest_rcrd(example_run, keep_live = FALSE)
  live <- tail(as_ernest_rcrd(example_run), 1000)
  scrambled_d <- sample(dead, size = length(dead))
  scrambled_l <- sample(live, size = length(live))
  all <- c(scrambled_d, scrambled_l)
  all <- sort(all)
  expect_equal(
    field(all, "log_lik"),
    field(as_ernest_rcrd(example_run), "log_lik")
  )
  expect_equal(field(all, "unit"), field(as_ernest_rcrd(example_run), "unit"))
  expect_identical(attr(all, "nvariables"), 3L)
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
