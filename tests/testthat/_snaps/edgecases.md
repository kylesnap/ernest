# Zero-length likelihood fails

    Code
      ernest_sampler(ll, prior, seed = 42)
    Condition
      Error in `ernest_sampler()`:
      ! <ernest_sampler> cannot compile.
      Caused by error in `compile()`:
      ! Error when creating the live set.
      Caused by error in `if (is.finite(log_lik) && log_lik > criterion) ...`:
      ! missing value where TRUE/FALSE needed

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
      ! `rowwise_fn` must have 1000 rows, not 0.

# Fails on character types

    Code
      create_prior(prior_fn, names = LETTERS[1:2])
    Condition
      Error:
      ! Can't convert `out` <character> to <double>.

---

    Code
      ernest_sampler(ll, create_uniform_prior(names = LETTERS[1:2]), seed = 42)
    Condition
      Error in `ernest_sampler()`:
      ! <ernest_sampler> cannot compile.
      Caused by error in `compile()`:
      ! Error when creating the live set.
      Caused by error:
      ! Couldn't calculate the log. lik of #.# and #.#.
      Caused by error:
      ! Can't convert `log_lik(x)` <character> to <double>.

# Fails on complex types

    Code
      create_prior(prior_fn, names = LETTERS[1:2])
    Condition
      Error:
      ! Can't convert `out` <complex> to <double>.

---

    Code
      ernest_sampler(ll, create_uniform_prior(names = LETTERS[1:2]), seed = 42)
    Condition
      Error in `ernest_sampler()`:
      ! <ernest_sampler> cannot compile.
      Caused by error in `compile()`:
      ! Error when creating the live set.
      Caused by error:
      ! Couldn't calculate the log. lik of #.# and #.#.
      Caused by error:
      ! Can't convert `log_lik(x)` <complex> to <double>.

# Missing values in the log-likelihood

    Code
      ernest_sampler(log_lik = create_likelihood(ll_fn_missing, on_nonfinite = "abort"),
      prior = gaussian_blobs$prior, seed = 42)
    Condition
      Error in `ernest_sampler()`:
      ! <ernest_sampler> cannot compile.
      Caused by error in `compile()`:
      ! Error when creating the live set.
      Caused by error:
      ! Couldn't calculate the log. lik of #.# and #.#.
      Caused by error:
      ! log-lik. values must be either finite or `-Inf`.
      x Detected non-viable value: `NA`.

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
      Nested sampling run specification:
      * No. points: 500
      * Sampling method: 25-step random walk sampling (acceptance target = #.#%)
      * Prior: uniform prior distribution with 2 dimensions (A and B)

# Ernest fails when ll is flat to begin with

    Code
      ernest_sampler(ll, create_uniform_prior(names = LETTERS[1:2]), seed = 42)
    Condition
      Error in `ernest_sampler()`:
      ! <ernest_sampler> cannot compile.
      Caused by error in `compile()`:
      ! `log_lik` must contain a range of likelihood values.
      x `log_lik` currently contains one unique value (0).

