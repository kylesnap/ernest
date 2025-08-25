# Zero-length likelihood fails

    <ernest_sampler> cannot compile.
    Caused by error in `compile()`:
    ! `log_lik(...)` must have size 1, not size 0.

# Zero-length prior fails

    `n_dim` must be a whole number larger than or equal to 1 or `NULL`, not the number 0.

---

    Can't validate `fn` as a valid prior.
    Caused by error in `prior$fn()`:
    ! `y` must have size 1, not size 0.

# Fails on character types

    Can't validate `fn` as a valid prior.
    Caused by error in `prior$fn()`:
    ! `prior(unit)` must always return a vector or matrix of doubles.
    x Instead, it returned a character vector.

---

    <ernest_sampler> cannot compile.
    Caused by error in `compile()`:
    ! Can't convert `log_lik(...)` <character> to <double>.

# Fails on complex types

    Can't validate `fn` as a valid prior.
    Caused by error in `prior$fn()`:
    ! `prior(unit)` must always return a vector or matrix of doubles.
    x Instead, it returned a complex vector.

---

    <ernest_sampler> cannot compile.
    Caused by error in `compile()`:
    ! Can't convert `log_lik(...)` <complex> to <double>.

# Missing values in the prior

    Can't validate `fn` as a valid prior.
    Caused by error in `prior$fn()`:
    ! `prior(unit)` must never return `NA` or `NaN` values.

# Missing values in the log-likelihood

    <ernest_sampler> cannot compile.
    Caused by error in `compile()`:
    ! `lik(x)` must always return finite double values or `-Inf`.

---

    <ernest_sampler> threw a warning during compilation
    Caused by warning in `compile()`:
    ! Replacing `NA` with `-Inf`.

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
      ! Only 142/500 likelihood values are unique.
      Warning:
      Stopping run due to a likelihood plateau at 0.
    Output
      Nested sampling run <ernest_run/ernest_sampler>
      No. Points: 500
      
      -- Sampling Method 
      * Random Walk in Unit Cube LRPS <rwmh_cube/ernest_lrps>
      * No. Dimensions: 2
      * No. Calls Since Update: 0
      * No. Accepted Since Update: 0
      * Current Step Size: 1
      
      -- Results 
      No. Iterations: 163
      No. Calls: 196
      Log. Evidence: -0.3244 (± 0.0664)

