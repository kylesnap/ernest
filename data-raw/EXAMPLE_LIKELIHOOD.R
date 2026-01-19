library(mvtnorm)

# Example data and model
data("PlantGrowth")
mod <- lm(weight ~ group, data = PlantGrowth)
frame <- model.frame(mod)
outcome <- model.response(frame)
predictors <- model.matrix(weight ~ group, frame)

# Log-likelihood function for a linear model (vector input)
simple_log_lik <- function(predictors, outcome) {
  function(theta) {
    mu <- predictors %*% theta[1:3]
    sigma <- theta[4]
    sum(stats::dnorm(outcome, mean = mu, sd = sigma, log = TRUE))
  }
}

# Create likelihood using a vector-based function
simple_ll <- create_likelihood(fn = simple_log_lik(predictors, outcome))

# Log-likelihood function for a linear model (matrix input)
fast_log_lik <- function(predictors, outcome) {
  n_obs <- length(outcome)
  function(theta) {
    if (!is.matrix(theta)) {
      dim(theta) <- c(1, 4)
    }
    apply(
      theta,
      1,
      \(row) {
        dmvnorm(
          outcome,
          mean = predictors %*% row[1:3],
          sigma = diag(row[4]^2, nrow = n_obs),
          log = TRUE
        )
      }
    )
  }
}

# Create likelihood using a matrix-based function
fast_ll <- create_likelihood(matrix_fn = fast_log_lik(predictors, outcome))

# Example: Evaluate at MLE
params <- c(mod$coefficients, summary(mod)$sigma)
simple_ll(params)
fast_ll(params)

# Example: Evaluate a matrix of parameters
param_mat <- matrix(
  rep(params, 2),
  ncol = 4,
  byrow = TRUE
)
param_mat[2, 4] <- param_mat[2, 4] / 2
simple_ll(param_mat)
fast_ll(param_mat)

# Handling non-finite values (default: warn and replace with -Inf)
simple_ll(c(mod$coefficients, NA))

# Control non-finite handling with on_nonfinite
abort_ll <- create_likelihood(
  fn = simple_log_lik(predictors, outcome),
  on_nonfinite = "abort"
)
try(abort_ll(c(mod$coefficients, NA)))

quiet_ll <- create_likelihood(
  fn = simple_log_lik(predictors, outcome),
  on_nonfinite = "quiet"
)
quiet_ll(c(mod$coefficients, NA))
