data(epilepsy, package = "brms")

# Pooled Model of Seizure Count via. Treatment and Baseline Seizure Count:
# glm(Count ~ zBase * Trt, family = poisson(), data = epilepsy)
#
# Building a Likelihood Function
mod <- stats::glm(count ~ zBase * Trt, family = poisson(), data = epilepsy)
mle <- stats::logLik(mod)

new_x <- model.matrix(
  object = mod$formula,
  data = mod$model,
  terms = mod$terms,
  contrasts.arg = attr(model.matrix(mod), "contrasts")
)
new_y <- model.response(mod$model)

linkinv <- stats::poisson()$linkinv

poisson_glm_ll <- function(x) {
  eta <- new_x %*% x
  mu <- linkinv(eta)
  sum(stats::dpois(new_y, lambda = mu, log = TRUE))
}
#' Custom function returns the expected log-lik. at the MLE.
all.equal(
  as.double(mle),
  poisson_glm_ll(mod$coefficients),
  check.attributes = FALSE
)

ll <- create_likelihood(poisson_glm_ll)
ll

# Controlling Missing Data Behaviour
new_x_na <- new_x
new_x_na[50:60, "zBase"] <- NA
na_poisson_glm_ll <- function(x) {
  eta <- new_x_na %*% x
  mu <- linkinv(eta)
  sum(stats::dpois(new_y, lambda = mu, log = TRUE))
}

# By default, ernest warns users and replaces NAs with `-Inf`
ll <- create_likelihood(na_poisson_glm_ll)
ll(mod$coefficients)

# Use `on_nonfinite` to specify how NAs should be treated
ll <- create_likelihood(na_poisson_glm_ll, on_nonfinite = "abort")
try(ll(mod$coefficients))

ll <- create_likelihood(na_poisson_glm_ll, on_nonfinite = "quiet")
ll(mod$coefficients)
