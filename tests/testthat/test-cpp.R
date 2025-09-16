run_cpp_tests("ernest")

test_that("Test ellipsoids", {
  set.seed(42)
  N_MAX <- 10

  for (n_dim in seq(2, N_MAX)) {
    det <- 0
    while (abs(det) < 1.e-10) {
      A <- matrix(runif(n_dim^2), nrow = n_dim)
      det <- det(A)
    }
    cor <- cov2cor(crossprod(A))
    pts <- matrix(runif(1000 * n_dim, min = 0, max = 1), ncol = n_dim)
    pts <- pts %*% cor

    ell <- cluster::ellipsoidhull(pts)
    if (ell$ierr != 0) {
      cli::cli_warn("Skipped test due to non-convergence.")
      next
    }
    precision <- solve(ell$cov)
    chol_prec <- chol(precision)

    new_pts <- t(replicate(1000, test_ellipsoid(chol_prec, ell$loc, ell$d2)))
    dists <- apply(new_pts, 1, \(x) {
      d <- x - ell$loc
      drop(d %*% precision %*% d)
    })
    n_oob <- sum(dists > ell$d2)
    expect_equal(n_oob, 0)
  }
})
