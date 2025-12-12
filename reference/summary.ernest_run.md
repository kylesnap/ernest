# Summarize a nested sampling run

Provides a summary of an `ernest_run` object, including key statistics
and a tibble of results for each iteration.

## Usage

``` r
# S3 method for class 'ernest_run'
summary(object, ...)
```

## Arguments

- object:

  An `ernest_run` object.

- ...:

  These dots are for future extensions and must be empty.

## Value

A list of class `summary.ernest_run` with the following components:

- `n_iter`: Integer. Number of iterations performed.

- `n_points`: Integer. Number of live points used in the run.

- `n_calls`: Integer. Total number of likelihood function calls.

- `log_volume`: Double. Final estimated log-prior volume.

- `log_evidence`: Double. Final log-evidence estimate.

- `log_evidence_err`: Double. Standard deviation of the log-evidence
  estimate.

- `draws`: Posterior draws as returned by
  [`as_draws()`](https://mc-stan.org/posterior/reference/draws.html).

- `run` A
  [tibble::tibble](https://tibble.tidyverse.org/reference/tibble.html).

`run` stores the state of the run at each iteration with these columns:

- `call`: Cumulative number of likelihood calls.

- `log_lik`: Log-likelihood for each sample.

- `log_volume`: Estimated log-prior volume.

- `log_weight`: Unnormalized log-weights (relative to evidence).

- `log_evidence`: Cumulative log-evidence.

- `log_evidence_err`: Standard deviation of log-evidence.

- `information`: Estimated KL divergence at each iteration.

## See also

- [`generate()`](https://generics.r-lib.org/reference/generate.html) for
  details on the `ernest_run` object.

- [`as_draws()`](https://mc-stan.org/posterior/reference/draws.html) for
  more information on `draws` objects.

## Examples

``` r
# Load an example run
data(example_run)

# Summarize the run and view a tibble of its results.
run_sm <- summary(example_run)
run_sm
#> nested sampling result summary <summary.ernest_run>
#> • No. Points: 1000
#> • No. Iterations: 9384
#> ────────────────────────────────────────────────────────────────────────────────
#> • No. Calls: 205329
#> • Log. Volume: -16.87
#> • Log. Evidence: -9.045 (± 0.08309)
run_sm$run
#> # A tibble: 10,384 × 7
#>     call log_lik log_volume log_weight log_evidence log_evidence_err information
#>    <int>   <dbl>      <dbl>      <dbl>        <dbl>            <dbl>       <dbl>
#>  1     1   -147.     -0.001      -145.        -154.         7.02e-33   -4.92e-62
#>  2     2   -141.     -0.002      -139.        -148.         1.81e-31   -3.29e-59
#>  3     3   -136.     -0.003      -134.        -143.         1.85e-30   -3.43e-57
#>  4     4   -136.     -0.004      -133.        -142.         3.74e-30   -1.40e-56
#>  5     5   -134.     -0.005      -131.        -140.         8.30e-30   -6.90e-56
#>  6     6   -130.     -0.006      -128.        -137.         3.86e-29   -1.49e-54
#>  7     7   -130.     -0.007      -128.        -136.         6.84e-29   -4.68e-54
#>  8     8   -129.     -0.008      -127.        -135.         1.09e-28   -1.18e-53
#>  9     9   -129.     -0.009      -127.        -135.         1.55e-28   -2.40e-53
#> 10    10   -126.     -0.01       -123.        -132.         4.02e-28   -1.62e-52
#> # ℹ 10,374 more rows
```
