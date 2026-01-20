data("PlantGrowth")

# Linear model: weight ~ group
mod <- lm(weight ~ group, data = PlantGrowth)
frame <- model.frame(mod)
predictors <- model.matrix(weight ~ group, frame)

# Prior for regression coefficients (Normal(0, 10))
coef_prior <- create_normal_prior(
  names = colnames(predictors),
  mean = 0,
  sd = 10
)
coef_prior
coef_prior$fn(rep(0.5, length(coef_prior$names)))

# Prior for standard deviation (Truncated Normal(0, 10), lower bound 0)
sd_prior <- create_prior(
  fn = function(x) extraDistr::qtnorm(x, mean = 0, sd = 10, a = 0),
  names = "sigma"
)
sd_prior
sd_prior$fn(0.5)

# Combine priors for full parameter vector (coefficients + sigma)
full_prior <- coef_prior + sd_prior
full_prior
full_prior$fn(rep(0.5, 4))
