run_cpp_tests("ernest")

test_that("uniform_in_ellipsoid works", {
  set.seed(42)
  DIM_MAX <- 20L

  for (n_dim in seq(DIM_MAX)) {
    cov <- diag(n_dim)
    c(l, v) %<-% eigen(cov, symmetric = FALSE)
    axlens <- sqrt(l)
    axes <- v %*% diag(axlens)
    precision <- v %*% diag(1 / l) %*% t(v)
    n_fails <- 0
    for (i in seq(1000)) {
      res <- test_ellipsoid(axes)
      if (sqrt(drop(crossprod(crossprod(precision, res), res))) > 1.0) {
        n_fails <- n_fails + 1L
      }
    }
    expect_equal(n_fails, 0L)
  }
})
