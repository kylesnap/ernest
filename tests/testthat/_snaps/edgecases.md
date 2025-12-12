# Zero-length likelihood fails

    Code
      ernest_sampler(ll, prior, seed = 42)
    Condition
      Error in `ernest_sampler()`:
      ! <ernest_sampler> cannot compile.
      Caused by error in `ernest_sampler()`:
      ! log-lik. values must be single scalars, not vectors of size 0.

# Zero-length prior fails

    Code
      create_prior(prior_fn, .n_dim = 0)
    Condition
      Error in `create_prior()`:
      ! `n_dim` must be a whole number larger than or equal to 1 or `NULL`, not the number 0.

---

    Code
      create_prior(prior_fn, .n_dim = 1)
    Condition
      Error in `create_prior()`:
      ! `fn` must return a vector of length 1, not one of length 0.

# Fails on character types

    Code
      create_prior(prior_fn, .n_dim = 2)
    Condition
      Error in `create_prior()`:
      ! `fn` must return a numeric vector, not a character vector.

---

    <ernest_sampler> cannot compile.
    Caused by error in `ernest_sampler()`:
    ! Can't convert `log-lik.` <character> to <double>.

# Fails on complex types

    Code
      create_prior(prior_fn, .n_dim = 2)
    Condition
      Error in `create_prior()`:
      ! `fn` must return a numeric vector, not a complex vector.

---

    <ernest_sampler> cannot compile.
    Caused by error in `ernest_sampler()`:
    ! Can't convert `log-lik.` <complex> to <double>.

# Missing values in the log-likelihood

    <ernest_sampler> cannot compile.
    Caused by error in `ernest_sampler()`:
    ! log-lik. values must be either finite or `-Inf`, not NA.

---

    Code
      ernest_sampler(create_likelihood(ll_fn_missing, on_nonfinite = "warn"),
      gaussian_blobs$prior, seed = 42)
    Condition
      Warning:
      <ernest_sampler> threw a warning during compilation
      Caused by warning:
      ! Replacing `NA` with `-Inf`.
    Message
      nested sampling specification <ernest_sampler>
      * No. Points: 500
      * LRPS Method: rwmh_cube
    Output
      
    Message
      ernest LRPS method <rwmh_cube/ernest_lrps>
      * Dimensions: 2
      * No. Log-Lik Calls: 0
      * No. Accepted Proposals: 0
      * No. Steps: 25
      * Target Acceptance: 0.5
      * Step Size: 1.000

# Ernest fails when ll is flat to begin with

    Code
      ernest_sampler(ll, create_uniform_prior(2), seed = 42)
    Condition
      Error in `create_uniform_prior()`:
      ! `lower` must be strictly smaller than `upper`.

