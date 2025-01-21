test_that("Basic Test Log Likelihood", {
  r <- 2  # radius
  w <- 0.1  # width
  c1 <- c(-3.5, 0)  # center of shell 1
  c2 <- c(3.5, 0)  # center of shell 2
  const <- log(1 / sqrt(2 * pi * w^2))  # normalization constant

  # Log-likelihood of a single shell
  logcirc <- function(theta, c) {
    d <- sqrt(sum((theta - c)^2))
    const - (d - r)^2 / (2 * w^2)
  }

  # Log-likelihood of two shells
  loglike <- function(theta) {
    log_add_exp(logcirc(theta, c1), logcirc(theta, c2))
  }

  # Prior transform
  prior_transform <- set_prior_transform(
    distributions3::Uniform(-6, 6),
    names = c("X", "Y")
  )

  nested_sampling(loglike, prior_transform, control = list(num_points = 100, max_iter = 100)) |>
    print()
})
