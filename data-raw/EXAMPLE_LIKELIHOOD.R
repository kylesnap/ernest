library(mvtnorm)

# Multivariate Normal Distribution
m <- 3
mean <- rep(0, m)
sigma <- diag(m)
sigma[2, 1] <- sigma[1, 2] <- 3 / 5
sigma[3, 1] <- sigma[1, 3] <- 1 / 3
sigma[3, 2] <- sigma[2, 3] <- 11 / 15
prec <- solve(sigma)
log_det <- -sum(log(diag(chol(sigma))))

# Provide a Scalar Log-Likelihood Function:
log_lik <- function(x) {
  log_det - 0.5 * m * log(2 * pi) - 0.5 * (t(x) %*% prec %*% x)
}
log_lik(c(0, 0, 0))

# `create_likelihood` allows scalar fns. to accept matrix inputs:
try(log_lik(matrix(rep(0, m * 2), nrow = 2)))
scalar_ll <- create_likelihood(scalar_fn = log_lik)
scalar_ll(matrix(rep(0, m * 2), nrow = 2))

# Provide a Vectorized Log-Likelihood Function:
v_log_lik <- function(x) {
  dmvnorm(x, mean = mean, sigma = sigma, log = TRUE)
}
v_log_lik(c(0, 0, 0))
v_log_lik(matrix(rep(0, m * 2), nrow = 2))

vector_ll <- create_likelihood(vectorized_fn = v_log_lik)
vector_ll

# Control Behaviour when Nonfinite Likelihood Values are Encountered
# Default: Warn and replace with `-Inf`
vector_ll(c(0, 0, NA))

# Signal an error
abort_ll <- create_likelihood(log_lik, on_nonfinite = "abort")
try(abort_ll(c(0, 0, NA)))

# Silently replace all non-finite values
quiet_ll <- create_likelihood(vectorized_fn = v_log_lik, on_nonfinite = "quiet")
quiet_ll(c(0, 0, NA))
