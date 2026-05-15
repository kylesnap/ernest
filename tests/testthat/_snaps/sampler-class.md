# ernest_sampler initializes correctly

    Code
      sampler
    Message
      Nested sampling run specification:
      * No. points: 500
      * Sampling method: 25-step random walk sampling (acceptance target = 50%)
      * Prior: uniform prior distribution with 2 dimensions (A and B)

# Zero-length likelihood fails

    Code
      ernest_sampler(ll, prior, seed = 42)
    Condition
      Error in `ernest_sampler()`:
      ! <ernest_sampler> cannot compile.
      Caused by error in `compile()`:
      ! Error when creating the live set.
      Caused by error in `vctrs::df_list()`:
      ! Can't recycle `unit` (size 500) to match `log_lik` (size 0).

# Fails on character types

    Code
      create_prior(prior_fn, names = LETTERS[1:2])
    Condition
      Error:
      ! Can't convert `prior$fn(x)` <character[,2]> to <double[,2]>.

---

    Code
      ernest_sampler(ll, create_uniform_prior(names = LETTERS[1:2]), seed = 42)
    Condition
      Error in `ernest_sampler()`:
      ! <ernest_sampler> cannot compile.
      Caused by error in `compile()`:
      ! Error when creating the live set.
      Caused by error:
      ! Couldn't calculate the log-lik of #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#, ..., #.#, and #.#.
      Caused by error:
      ! Can't convert `log_lik(x)` <character> to <double>.

# Fails on complex types

    Code
      create_prior(prior_fn, names = LETTERS[1:2])
    Condition
      Error:
      ! Can't convert `prior$fn(x)` <complex[,2]> to <double[,2]>.

---

    Code
      ernest_sampler(ll, create_uniform_prior(names = LETTERS[1:2]), seed = 42)
    Condition
      Error in `ernest_sampler()`:
      ! <ernest_sampler> cannot compile.
      Caused by error in `compile()`:
      ! Error when creating the live set.
      Caused by error:
      ! Couldn't calculate the log-lik of #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#, #.#, ..., #.#, and #.#.
      Caused by error:
      ! Can't convert `log_lik(x)` <complex> to <double>.

# Ernest fails when ll is flat to begin with

    Code
      ernest_sampler(ll, create_uniform_prior(names = LETTERS[1:2]), seed = 42)
    Condition
      Error in `ernest_sampler()`:
      ! <ernest_sampler> cannot compile.
      Caused by error in `compile()`:
      ! `log_lik` must contain a range of likelihood values.
      x `log_lik` currently contains one unique value (0).

