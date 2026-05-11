withr::local_seed(42)

describe("new_live_set", {
  sampler <- ernest_sampler(
    log_lik = gaussian_blobs$log_lik,
    prior = gaussian_blobs$prior,
    seed = 42
  )

  it("generates the live set correctly", {
    result <- new_live_set(sampler$lrps, 10)
    expect_equal(dim(result$unit), c(10, 2))
    expect_equal(
      apply(
        apply(result$unit, 1, gaussian_blobs$prior$fn),
        2,
        gaussian_blobs$log_lik
      ),
      result$log_lik
    )
  })

  it("gives informative errors when unit_log_fn fails", {
    bad_lik <- new_rwmh_cube(
      unit_log_fn = \(x) stop("Bad Likelihood!"),
      nvar = 2L
    )
    expect_error(new_live_set(bad_lik, 10), "Bad Likelihood!")
  })
})

#' @srrstats {BS2.1a} write_live_set() validates live set components and ensures
#' that log_lik produces expected output given prior transformation, ensuring
#' commensurate quantities. It is called by compile and by ernest_sampler.
describe("write_live_set", {
  sampler <- ernest_sampler(
    log_lik = gaussian_blobs$log_lik,
    prior = gaussian_blobs$prior,
    seed = 42,
    nlive = 500
  )

  make_live <- function(
    unit = matrix(runif(1000), nrow = 500, ncol = 2),
    log_lik = seq(-10, -1, length.out = 500),
    birth_lik = rep(-Inf, 500)
  ) {
    list(unit = unit, log_lik = log_lik, birth_lik = birth_lik)
  }

  it("accepts valid live set and binds to live_env", {
    live <- make_live()
    result <- write_live_set(live, sampler)
    expect_true(exists("unit", envir = result))
    expect_true(exists("log_lik", envir = result))
    expect_true(exists("birth_lik", envir = result))
    expect_equal(nrow(result$unit), 500)
    expect_equal(length(result$log_lik), 500)
    expect_equal(length(result$birth_lik), 500)
  })

  it("errors if unit doesn't match nvar", {
    live <- make_live(unit = matrix(runif(1500), nrow = 500, ncol = 3))
    expect_error(
      write_live_set(live, sampler),
      "Non-recyclable dimensions."
    )
  })

  it("errors if unit contains values outside [0,1]", {
    live <- make_live(
      unit = matrix(runif(1000, -0.5, 1.5), nrow = 500, ncol = 2)
    )
    expect_error(
      write_live_set(live, sampler),
      "`unit` must contain only finite values between 0 and 1"
    )
  })

  it("errors if unit contains non-finite values", {
    live <- make_live()
    live$unit[5, 2] <- NaN
    expect_error(
      write_live_set(live, sampler),
      "`unit` must contain only finite values between 0 and 1"
    )
  })

  it("errors if components have mismatched sizes", {
    live <- make_live(log_lik = seq(-10, -1, length.out = 499))
    expect_error(
      write_live_set(live, sampler),
      "must have size 500"
    )
  })

  it("errors if log_lik contains Inf", {
    live <- make_live()
    live$log_lik[5] <- Inf
    expect_error(
      write_live_set(live, sampler),
      "`log_lik` must contain only finite values or `-Inf`"
    )
  })

  it("errors if log_lik contains NaN", {
    live <- make_live()
    live$log_lik[5] <- NaN
    expect_error(
      write_live_set(live, sampler),
      "`log_lik` must contain only finite values or `-Inf`"
    )
  })

  it("allows log_lik to contain -Inf", {
    live <- make_live()
    live$log_lik[5] <- -Inf
    expect_no_error(write_live_set(live, sampler))
  })

  it("errors if log_lik is a perfect plateau (all values identical)", {
    live <- make_live(log_lik = rep(-10, 500L))
    expect_error(
      write_live_set(live, sampler),
      "`log_lik` currently contains one unique value"
    )
  })

  it("warns if log_lik has too many repeated values", {
    live <- make_live()
    live$log_lik[250:500] <- live$log_lik[250]
    expect_warning(
      write_live_set(live, sampler),
      "Only 250/500 likelihood values are unique"
    )
  })

  it("errors if birth_lik has wrong size", {
    live <- make_live(birth_lik = rep(-Inf, 10))
    expect_error(
      write_live_set(live, sampler),
      "must have size 500"
    )
  })

  it("errors if birth_lik contains non-finite values", {
    live <- make_live()
    live$birth_lik[5] <- NA
    expect_error(
      write_live_set(live, sampler),
      "`birth_lik` must contain only finite values or `-Inf`"
    )
  })
})

test_that("compile initializes the live set", {
  sampler <- ernest_sampler(
    log_lik = gaussian_blobs$log_lik,
    prior = gaussian_blobs$prior,
    seed = 42,
    nlive = 500
  )
  sampler <- compile(sampler)
  orig_units <- sampler$live_env$unit
  orig_log_lik <- sampler$live_env$log_lik

  expect_equal(dim(orig_units), c(500, 2))
  expected_log_lik <- apply(
    t(apply(orig_units, 1, gaussian_blobs$prior$fn)),
    1,
    gaussian_blobs$log_lik
  )

  expect_equal(orig_log_lik, expected_log_lik)
  expect_equal(sampler$live_env$birth_lik, rep(-Inf, 500))
})
