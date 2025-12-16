data(epilepsy, package = "brms")

# Pooled Model of Seizure Count via. Treatment and Baseline Seizure Count:
# glm(Count ~ zBase * Trt, family = poisson(), data = epilepsy)
#
# Using a weakly-informative prior
# All coefficients (b0, b1, b2, b1_2) follow N(0, 10)
pooled_prior <- create_normal_prior(
  names = c("b0", "b1", "b2", "b1_2"),
  mean = 0,
  sd = 10
)
pooled_prior
pooled_prior$fn(c(0.5, 0.5, 0.5, 0.5))

# Random-Intercept Model of Seizure-Treatment Relationship:
# brm(
#   count ~ zBase * Trt + (1 | patient),
#   data = epilepsy,
#   family = poisson(),
#   prior = prior(normal(0, 10), class = b) + prior(cauchy(0, 2), class = sd)
# )
#
# Random Intercepts follow N(mu, sd)
# Remaining coefficients (b1, b2, b1_2) follow N(0, 10)
patients <- as.character(unique(epilepsy$patient))
n_groups <- length(patients)
intercept_transform <- function(x) {
  mu <- stats::qnorm(x[1], mean = 0, sd = 10) # Hyperparameter `mu`
  sd <- extraDistr::qtnorm(x[2], mean = 0, sd = 10, a = 0) # Hyperparameter `sd`
  intercept <- stats::qnorm(x[3:(n_groups + 2)], mean = mu, sd = sd)
  c(mu, sd, intercept)
}

intercept_prior <- create_prior(
  intercept_transform,
  names = c("mu", "sd", patients)
)
intercept_prior
intercept_prior$fn(rep(0.5, n_groups + 2))

population_prior <- create_normal_prior(
  names = c("b1", "b2", "b1_2"),
  mean = 0,
  sd = 10
)

# Combine priors using `c()`
full_prior <- c(intercept_prior, population_prior)
print(full_prior)
