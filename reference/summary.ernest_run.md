# Summarize a nested sampling run

Returns a concise summary of an `ernest_run` object, including key
statistics and a description of the posterior distribution.

## Usage

``` r
# S3 method for class 'ernest_run'
summary(object, ...)
```

## Arguments

- object:

  \[[ernest_run](https://kylesnap.github.io/ernest/reference/generate-ernest.md)\]  
  Results from a nested sampling run.

- ...:

  These dots are for future extensions and must be empty.

## Value

A named list, containing:

- `nlive`: `[integer(1)]` Number of points in the live set.

- `niter`: `[integer(1)]` Number of iterations performed.

- `neval`: `[integer(1)]` Number of times the likelihood function was
  evaluated.

- `log_evidence`: `[numeric(1)]` Log-evidence estimate.

- `log_evidence_err`: `[numeric(1)]` Standard error of log-evidence.

- `information`: `[numeric(1)]` Estimated Kullback-Leibler divergence
  between the prior and posterior.

- `reweighted_samples`:
  \[[posterior::draws_matrix](https://mc-stan.org/posterior/reference/draws_matrix.html)\]
  Posterior samples, resampled by normalized weights.

- `mle`: `[list]` Maximum likelihood estimate extracted during the run,
  stored in a list with the elements:

  - `log_lik`: `[double(1)]` The maximum log-likelihood value.

  - `original`, `unit_cube`: `[double(nvar)]` The parameter values at
    the MLE, expressed in the original parameter space and within the
    unit cube.

- `posterior`: \[[data.frame](https://rdrr.io/r/base/data.frame.html)\]
  with columns for the posterior mean, sd, median, and the 15th and 85th
  percentiles for each parameter.

- `seed`: The RNG seed used.

## See also

[`generate.ernest_run()`](https://kylesnap.github.io/ernest/reference/generate-ernest.md)
[`as_draws.ernest_run()`](https://kylesnap.github.io/ernest/reference/as_draws.ernest_run.md)

## Examples

``` r
data(example_run)
run_sm <- summary(example_run)
run_sm
#> Summary of nested sampling run:
#> ── Run Information ─────────────────────────────────────────────────────────────
#> * No. points: 1000
#> * Iterations: 9353
#> * Likelihood evals.: 204731
#> * Log-evidence: -9.0165 (± 0.0824)
#> * Information: 4.82
#> * RNG seed: 42
#> ── Posterior Summary ───────────────────────────────────────────────────────────
#> # A tibble: 3 × 6
#>   variable     mean    sd    median   q15   q85
#>   <chr>       <dbl> <dbl>     <dbl> <dbl> <dbl>
#> 1 x         0.00123  2.80 -0.00110  -1.88  1.93
#> 2 y        -0.00750  2.83  0.000403 -1.98  1.97
#> 3 z        -0.0158   2.80 -0.0233   -1.98  1.91
#> ── Maximum Likelihood Estimate (MLE) ───────────────────────────────────────────
#> * Log-likelihood: -2.6825
#> * Original parameters: -0.0425, 0.0561, and -0.006
run_sm$posterior
#> # A tibble: 3 × 6
#>   variable     mean    sd    median   q15   q85
#>   <chr>       <dbl> <dbl>     <dbl> <dbl> <dbl>
#> 1 x         0.00123  2.80 -0.00110  -1.88  1.93
#> 2 y        -0.00750  2.83  0.000403 -1.98  1.97
#> 3 z        -0.0158   2.80 -0.0233   -1.98  1.91
```
