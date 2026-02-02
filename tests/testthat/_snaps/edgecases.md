# Zero-length likelihood fails

    Code
      ernest_sampler(ll, prior, seed = 42)
    Condition
      Error in `ernest_sampler()`:
      ! <ernest_sampler> cannot compile.
      Caused by error in `compile()`:
      ! `unit` must have 500 rows, not 0.

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
      Error:
      ! Can't convert `prior$fn(x)` <double[,0]> to <double[,1]>.
      Non-recyclable dimensions.

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

