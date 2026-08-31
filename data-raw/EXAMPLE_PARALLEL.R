# Step 1: Create a portable likelihood function
likelihood <- parallel_likelihood(
  vectorized_fn = function(x) {
    apply(x, 1, \(params) {
      -0.5 * sum(params^2)
    })
  }
)

# Step 2: Create a portable prior function
prior <- create_uniform_prior(lower = c(-5, -5), upper = c(5, 5))

# Step 3: Set up daemons (this is usually done once per session)
mirai::daemons(1, dispatcher = FALSE)

# Step 4: Build the sampler and call generate
sampler <- ernest_sampler(
  log_lik = likelihood,
  prior = prior,
  nlive = 400
)
s <- compile(sampler)
run <- generate(sampler, min_logz = 0.1, parallel = 4)
run

# View the results of each sub-run
run$.parallel

# Safely terminate daemons (for CRAN)
mirai::daemons(0)
Sys.sleep(1)
