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

  \[[ernest_run](https://kylesnap.github.io/ernest/reference/generate.ernest_sampler.md)\]  
  Results from a nested sampling run.

- ...:

  These dots are for future extensions and must be empty.

## Value

`[summary.ernest_run]` A named list, containing:

- `nlive`: `[integer(1)]` Number of points in the live set.

- `niter`: `[integer(1)]` Number of iterations.

- `neval`: `[integer(1)]` Number of likelihood evaluations.

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

  - `original`, `unit_cube`: `[double(n_dim)]` The parameter values at
    the MLE, expressed in the original parameter space and within the
    unit cube.

- `posterior`: \[tibble\] with columns for the posterior mean, sd,
  median, and the 15th and 85th percentiles for each parameter.

- `seed`: The RNG seed used.

## See also

- [`generate()`](https://generics.r-lib.org/reference/generate.html) for
  details on the `ernest_run` object.

- [`as_draws()`](https://mc-stan.org/posterior/reference/draws.html) for
  details on how posterior samples are extracted.

## Examples

``` r
data(example_run)
run_sm <- summary(example_run)
run_sm
#> Summary of nested sampling run:
#> ── Run Information ─────────────────────────────────────────────────────────────
#> * No. points: 1000
#> * Iterations: 9456
#> * Likelihood evals.: 207001
#> * Log-evidence: -9.1176 (± 0.0833)
#> * Information: 4.930
#> * RNG seed: 42
#> ── Posterior Summary ───────────────────────────────────────────────────────────
#> # A tibble: 3 × 6
#>   variable     mean    sd  median   q15   q85
#>   <chr>       <dbl> <dbl>   <dbl> <dbl> <dbl>
#> 1 x        -0.00572  2.79 -0.0174 -1.94  2.00
#> 2 y         0.0302   2.80  0.0165 -1.91  2.04
#> 3 z        -0.00378  2.82  0.0192 -1.96  1.97
#> ── Maximum Likelihood Estimate (MLE) ───────────────────────────────────────────
#> * Log-likelihood: -2.6803
#> * Original parameters: 0.0097, -0.0096, and -0.0231
run_sm$posterior
#> # A tibble: 3 × 6
#>   variable     mean    sd  median   q15   q85
#>   <chr>       <dbl> <dbl>   <dbl> <dbl> <dbl>
#> 1 x        -0.00572  2.79 -0.0174 -1.94  2.00
#> 2 y         0.0302   2.80  0.0165 -1.91  2.04
#> 3 z        -0.00378  2.82  0.0192 -1.96  1.97
```
