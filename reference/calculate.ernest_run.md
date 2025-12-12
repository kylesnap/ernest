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

  An `ernest_run` object.

- ndraws:

  A positive integer. The number of log-volume sequences to simulate. If
  equal to zero, no simulations will be made, and a one draw vector of
  log-volumes are produced from the estimates contained in `x`.

- ...:

  These dots are for future extensions and must be empty.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html),
containing `n_iter + n_points` rows and several columns:

- `log_lik`: The log-likelihood of the model.

- `log_volume`: The log-volume of the prior space.

- `log_weight`: The log weights of the live points.

- `log_evidence`: The log-evidence of the model.

- `log_evidence_err`: The standard error of the log-evidence (only
  available when `ndraws = 0`).

The tibble has the additional class `ernest_estimate`, which has its own
[plot](https://kylesnap.github.io/ernest/reference/plot.ernest.md)
method.

Each column is returned as an
[`posterior::rvar()`](https://mc-stan.org/posterior/reference/rvar.html)
vector.

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
#> evidence estimates <ernest_estimate>
#> 
#> Log. Volume: -17 ± NA
#> Log. Evidence: -9 ± NA
#> # A tibble: 10,384 × 5
#>       log_lik   log_volume log_weight log_evidence log_evidence_err
#>    <rvar[1d]>   <rvar[1d]> <rvar[1d]>   <rvar[1d]>       <rvar[1d]>
#>  1  -147 ± NA  -0.001 ± NA  -154 ± NA    -154 ± NA     7.0e-33 ± NA
#>  2  -141 ± NA  -0.002 ± NA  -148 ± NA    -148 ± NA     1.8e-31 ± NA
#>  3  -136 ± NA  -0.003 ± NA  -143 ± NA    -143 ± NA     1.9e-30 ± NA
#>  4  -136 ± NA  -0.004 ± NA  -142 ± NA    -142 ± NA     3.7e-30 ± NA
#>  5  -134 ± NA  -0.005 ± NA  -140 ± NA    -140 ± NA     8.3e-30 ± NA
#>  6  -130 ± NA  -0.006 ± NA  -137 ± NA    -137 ± NA     3.9e-29 ± NA
#>  7  -130 ± NA  -0.007 ± NA  -137 ± NA    -136 ± NA     6.8e-29 ± NA
#>  8  -129 ± NA  -0.008 ± NA  -136 ± NA    -135 ± NA     1.1e-28 ± NA
#>  9  -129 ± NA  -0.009 ± NA  -136 ± NA    -135 ± NA     1.5e-28 ± NA
#> 10  -126 ± NA  -0.010 ± NA  -132 ± NA    -132 ± NA     4.0e-28 ± NA
#> # ℹ 10,374 more rows

# Generate 100 simulated log-volume values for each iteration.
calculate(example_run, ndraws = 100)
#> evidence estimates <ernest_estimate>
#> 
#> No. of Simulated Draws: 100
#> Log. Volume: -17 ± 1.4
#> Log. Evidence: -9 ± 0.07
#> # A tibble: 10,384 × 4
#>       log_lik          log_volume   log_weight log_evidence
#>    <rvar[1d]>          <rvar[1d]>   <rvar[1d]>   <rvar[1d]>
#>  1  -147 ± NA  -0.00089 ± 0.00098  -154 ± 0.90  -154 ± 0.90
#>  2  -141 ± NA  -0.00184 ± 0.00140  -148 ± 0.82  -148 ± 0.82
#>  3  -136 ± NA  -0.00291 ± 0.00192  -143 ± 0.83  -143 ± 0.83
#>  4  -136 ± NA  -0.00377 ± 0.00220  -143 ± 0.72  -142 ± 0.60
#>  5  -134 ± NA  -0.00487 ± 0.00241  -141 ± 0.76  -140 ± 0.62
#>  6  -130 ± NA  -0.00587 ± 0.00262  -137 ± 0.72  -137 ± 0.69
#>  7  -130 ± NA  -0.00688 ± 0.00269  -137 ± 0.83  -136 ± 0.63
#>  8  -129 ± NA  -0.00803 ± 0.00285  -136 ± 0.75  -135 ± 0.59
#>  9  -129 ± NA  -0.00882 ± 0.00293  -136 ± 0.73  -135 ± 0.51
#> 10  -126 ± NA  -0.00989 ± 0.00312  -133 ± 0.79  -133 ± 0.70
#> # ℹ 10,374 more rows
```
