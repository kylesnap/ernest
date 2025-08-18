# Zero-length likelihood fails

    <ernest_sampler> cannot compile.
    Caused by error in `ernest_sampler()`:
    ! Can't create live points.
    Caused by error in `_fn`:
    ! `log_lik(...)` must have size 1, not size 0.

# Zero-length prior fails

    `n_dim` must be a whole number larger than or equal to 1, not the number 0.

---

    Can't validate `fn` as a valid prior.
    Caused by error in `fn()`:
    ! `y` must have size 1, not size 0.

# Fails on character types

    Can't validate `fn` as a valid prior.
    Caused by error in `fn()`:
    ! `prior(unit)` must always return a vector or matrix of doubles.
    x Instead, it returned a character vector.

---

    <ernest_sampler> cannot compile.
    Caused by error in `ernest_sampler()`:
    ! Can't create live points.
    Caused by error in `_fn`:
    ! Can't convert `log_lik(...)` <character> to <double>.

# Fails on complex types

    Can't validate `fn` as a valid prior.
    Caused by error in `fn()`:
    ! `prior(unit)` must always return a vector or matrix of doubles.
    x Instead, it returned a complex vector.

---

    <ernest_sampler> cannot compile.
    Caused by error in `ernest_sampler()`:
    ! Can't create live points.
    Caused by error in `_fn`:
    ! Can't convert `log_lik(...)` <complex> to <double>.

# Missing values in the prior

    Can't validate `fn` as a valid prior.
    Caused by error in `fn()`:
    ! `prior(unit)` must never return `NA` or `NaN` values.

# Missing values in the log-likelihood

    <ernest_sampler> threw a warning during compilation
    Caused by warning:
    ! Replacing `NA` with `-Inf`.

---

    <ernest_sampler> cannot compile.
    Caused by error in `ernest_sampler()`:
    ! Can't create live points.
    Caused by error in `_fn`:
    ! `lik(x)` must always return finite double values or `-Inf`.
    x `lik(x)` returned NA.
    i Did you set `.nonfinite_action` with `create_likelihood()`)?

# Ernest fails when ll is flat to begin with

    <ernest_sampler> cannot compile.
    Caused by error in `ernest_sampler()`:
    ! `log_lik` must contain a range of likelihood values.
    x `log_lik` currently contains one unique value (0).

# Ernest halts and warns when ll becomes flat during a run

    Code
      generate(sampler, seed = 42L)
    Condition
      Warning in `compile()`:
      `log_lik` may contain a likelihood plateau; proceed with caution.
      ! Only 135/500 likelihood values are unique.
      Warning:
      Stopping run due to a likelihood plateau at 0.
    Output
      
      -- Nested sampling run: 
      No. Iterations: 151
      No. Calls: 172
      Log. Evidence: -0.303 (± 0.064)

