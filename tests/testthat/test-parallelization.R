parallel_lik <- NULL
parallel_pt <- NULL

test_that("parallel ernest_likelihood works", {
  parallel_lik <<- parallel_likelihood(
    function(x) {
      sum(x) + c
    },
    c = 0.5
  )
  expect_s3_class(parallel_lik, c("ernest_likelihood", "crate"))
})


test_that("parallel ernest_prior works", {
  parallel_pt <<- parallel_prior(
    function(x) {
      cumsum(x) + c
    },
    .names = c("a", "b", "c"),
    c = 0.5
  )
  expect_s3_class(parallel_pt, c("crated_prior", "ernest_prior"))
  expect_s3_class(attr(parallel_pt, "body"), "crate")
})

describe("check_parallel_enabled", {
  it("fails when log_lik is not a crate", {
    sampler <- ernest_sampler(\(x) sum(x), parallel_pt)
    expect_error(check_parallel_enabled(sampler), "portable `log_lik` function")
  })

  it("fails when prior is not a crate", {
    sampler <- ernest_sampler(
      parallel_lik,
      create_prior(\(x) cumsum(x), names = LETTERS[1:3])
    )
    expect_error(check_parallel_enabled(sampler), "portable `prior` function")
  })

  it("fails when daemons are not set", {
    sampler <- ernest_sampler(parallel_lik, parallel_pt)
    expect_error(check_parallel_enabled(sampler), "No daemons set.")
  })
})

# Set up CRAN-compliant daemons
mirai::daemons(1, dispatcher = FALSE)
on.exit(mirai::daemons(0), add = TRUE)
