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
      create_prior(prior_fn, names = character())
    Condition
      Error in `new_ernest_prior()`:
      ! `names` must be at least length one, not length 0.

---

    Code
      create_prior(prior_fn, names = LETTERS[1])
    Condition
      Error in `new_ernest_prior()`:
      ! Error while validating the prior.
      Caused by error in `check_prior()`:
      ! `fn` must return a numeric vector of length 1, not one of length 0.

# Fails on character types

    Code
      create_prior(prior_fn, names = LETTERS[1:2])
    Condition
      Error in `new_ernest_prior()`:
      ! Error while validating the prior.
      Caused by error:
      ! Can't convert `out` <character> to <double>.

---

    <ernest_sampler> cannot compile.
    Caused by error in `ernest_sampler()`:
    ! Can't convert `log-lik.` <character> to <double>.

# Fails on complex types

    Code
      create_prior(prior_fn, names = LETTERS[1:2])
    Condition
      Error in `new_ernest_prior()`:
      ! Error while validating the prior.
      Caused by error:
      ! Can't convert `out` <complex> to <double>.

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
      ernest_sampler(ll, create_uniform_prior(names = LETTERS[1:2]), seed = 42)
    Condition
      Error in `ernest_sampler()`:
      ! <ernest_sampler> cannot compile.
      Caused by error in `ernest_sampler()`:
      ! `log_lik` must contain a range of likelihood values.
      x `log_lik` currently contains one unique value (0).

