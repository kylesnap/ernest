test_that("ernest_rcrd stores the expected information", {
  data(example_run)
  x <- example_run$rcrd
  ref <- example_run$rcrd
  expect_equal(
    vctrs::fields(x),
    c("unit", "log_lik", "id", "nlive", "neval", "birth_lik")
  )
  expect_equal(field(x, "log_lik"), field(ref, "log_lik"))
  expect_equal(field(x, "id"), field(ref, "id"))
  expect_equal(field(x, "neval"), field(ref, "neval"))
  expect_equal(field(x, "birth_lik"), field(ref, "birth_lik"))
  unit <- field(example_run$rcrd, "unit")
  expect_equal(field(x, "unit"), unit)
  expect_identical(attr(x, "nvar"), 3L)
  expect_snapshot(glance(example_run$rcrd))
  expect_snapshot(x)
})

test_that("as.list ernest_rcrd works", {
  data(example_run)
  x <- example_run$rcrd
  list_x <- as.list(x)
  expect_named(
    list_x,
    c("unit", "log_lik", "id", "nlive", "neval", "birth_lik")
  )
  expect_equal(list_x$log_lik, field(example_run$rcrd, "log_lik"))
})

test_that("ernest_rcrd orders draws", {
  dead <- head(example_run$rcrd, -1000)
  live <- tail(example_run$rcrd, 1000)
  scrambled_d <- sample(dead, size = length(dead))
  scrambled_l <- sample(live, size = length(live))
  all <- c(scrambled_d, scrambled_l)
  all <- sort(all)
  expect_equal(
    field(all, "log_lik"),
    field(example_run$rcrd, "log_lik")
  )
  expect_equal(field(all, "unit"), field(example_run$rcrd, "unit"))
  expect_identical(attr(all, "nvar"), 3L)
})

test_that("ernest_rcrd reports dimensional mismatch", {
  skip_extended()
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
    c(run2D$rcrd, run3D$rcrd),
    "`nvar` attribute must match."
  )
})

describe("rcrd_is_run", {
  it("returns TRUE on a valid run", {
    expect_true(rcrd_is_run(example_run$rcrd))
  })

  res <- c()
  it("warns when unique IDs don't match nlive", {
    rcrd <- example_run$rcrd
    expect_warning(
      res <<- c(res, rcrd_is_run(rcrd, nlive = 500)),
      "should contain 500 unique IDs, but has 1000."
    )
  })

  it("warns when `x` is scrambled", {
    withr::local_seed(42)
    rcrd <- example_run$rcrd
    rcrd[1:1000] <- rev(rcrd[1:1000])
    expect_warning(
      res <<- c(res, rcrd_is_run(rcrd)),
      "should be sorted in ascending order of log-likelihood."
    )
  })
  expect_all_false(res)
})
