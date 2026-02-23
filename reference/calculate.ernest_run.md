# Estimate Evidence using a Nested Sampling Run

Computes evidence and related quantities from a nested sampling run,
optionally by simulating the volumes of each nested likelihood shell.

## Usage

``` r
# S3 method for class 'ernest_run'
calculate(x, ndraws = 1000L, ...)
```

## Arguments

- x:

  \[[ernest_run](https://kylesnap.github.io/ernest/reference/generate.ernest_sampler.md)\]  
  Results from a nested sampling run.

- ndraws:

  `[integer(1)]`  
  The number of log-volume sequences to simulate. If equal to zero, no
  simulations will be made, and a one draw vector of log-volumes are
  produced from the estimates contained in `x`.

- ...:

  These dots are for future extensions and must be empty.

## Value

\[[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)\]
with class `ernest_estimate`.

The iterative estimates from the nested sampling run. Contains the
following columns:

- `log_lik`:
  \[[rvar](https://mc-stan.org/posterior/reference/rvar.html)\] The
  log-likelihood of the model.

- `log_volume`:
  \[[rvar](https://mc-stan.org/posterior/reference/rvar.html)\] The
  log-volume of the prior space.

- `log_weight`:
  \[[rvar](https://mc-stan.org/posterior/reference/rvar.html)\] The log
  weights of the points in the live set.

- `log_evidence`:
  \[[rvar](https://mc-stan.org/posterior/reference/rvar.html)\] The
  log-evidence of the model.

If `ndraws = 0`, an additional column is included:

- `log_evidence_err`:
  \[[rvar](https://mc-stan.org/posterior/reference/rvar.html)\] The
  standard error of the log-evidence.

## References

Higson, E., Handley, W., Hobson, M., & Lasenby, A. (2019). Nestcheck:
Diagnostic Tests for Nested Sampling Calculations. Monthly Notices of
the Royal Astronomical Society, 483(2), 2044–2056.
[doi:10.1093/mnras/sty3090](https://doi.org/10.1093/mnras/sty3090)

## Examples

``` r
# Load an example run
data(example_run)

# View results as a tibble with `ndraws = 0`.
calculate(example_run, ndraws = 0)
#> Nested sampling uncertainty estimates:
#> # of Simulated Draws: 0
#> Log-volume: -17 ± NA
#> Log-evidence: -9.1 ± NA

# Generate 100 simulated log-volume values for each iteration.
calculate(example_run, ndraws = 100)
#> Nested sampling uncertainty estimates:
#> # of Simulated Draws: 100
#> Log-volume: -17 ± 1.3
#> Log-evidence: -9.1 ± 0.07
```
