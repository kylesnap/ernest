# Estimate Evidence using a Nested Sampling Run

Computes evidence and related quantities from a nested sampling run,
optionally by simulating the volumes of each nested likelihood shell.

## Usage

``` r
# S3 method for class 'ernest_run'
calculate(x, ..., ndraws = NULL)
```

## Arguments

- x:

  An `ernest_run` object.

- ...:

  These dots are for future extensions and must be empty.

- ndraws:

  An optional positive integer. The number of log-volume sequences to
  simulate. If equal to zero, no simulations will be made, and a one
  draw vector of log-volumes are produced from the estimates contained
  in `x`. If `NULL`, `getOption("posterior.rvar_ndraws")` is used
  (default 4000).

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

# View results as a tibble with `ndraws = FALSE` (the default).
calculate(example_run)
#> evidence estimates <ernest_estimate>
#> 
#> No. of Simulated Draws: 4000
#> Log. Volume: -17 ± 1.3
#> Log. Evidence: -9.1 ± 0.069
#> # A tibble: 10,398 × 4
#>       log_lik         log_volume   log_weight log_evidence
#>    <rvar[1d]>         <rvar[1d]>   <rvar[1d]>   <rvar[1d]>
#>  1  -147 ± NA  -0.00099 ± 0.0010  -154 ± 0.81  -154 ± 0.81
#>  2  -141 ± NA  -0.00199 ± 0.0014  -148 ± 0.80  -148 ± 0.80
#>  3  -136 ± NA  -0.00301 ± 0.0017  -143 ± 0.81  -143 ± 0.80
#>  4  -136 ± NA  -0.00402 ± 0.0020  -143 ± 0.82  -142 ± 0.67
#>  5  -134 ± NA  -0.00504 ± 0.0022  -141 ± 0.79  -140 ± 0.66
#>  6  -130 ± NA  -0.00604 ± 0.0025  -137 ± 0.79  -137 ± 0.75
#>  7  -130 ± NA  -0.00701 ± 0.0027  -137 ± 0.81  -136 ± 0.63
#>  8  -129 ± NA  -0.00804 ± 0.0028  -136 ± 0.81  -135 ± 0.58
#>  9  -129 ± NA  -0.00902 ± 0.0030  -136 ± 0.81  -135 ± 0.53
#> 10  -126 ± NA  -0.01000 ± 0.0031  -133 ± 0.80  -133 ± 0.69
#> # ℹ 10,388 more rows

# Generate 100 simulated log-volume values for each iteration.
calculate(example_run, ndraws = 100)
#> evidence estimates <ernest_estimate>
#> 
#> No. of Simulated Draws: 100
#> Log. Volume: -17 ± 1.2
#> Log. Evidence: -9.1 ± 0.076
#> # A tibble: 10,398 × 4
#>       log_lik        log_volume   log_weight log_evidence
#>    <rvar[1d]>        <rvar[1d]>   <rvar[1d]>   <rvar[1d]>
#>  1  -147 ± NA  -0.0013 ± 0.0013  -154 ± 0.92  -154 ± 0.92
#>  2  -141 ± NA  -0.0022 ± 0.0017  -148 ± 0.77  -148 ± 0.77
#>  3  -136 ± NA  -0.0032 ± 0.0019  -143 ± 0.65  -143 ± 0.64
#>  4  -136 ± NA  -0.0041 ± 0.0020  -143 ± 0.95  -142 ± 0.62
#>  5  -134 ± NA  -0.0050 ± 0.0023  -141 ± 0.89  -141 ± 0.72
#>  6  -130 ± NA  -0.0060 ± 0.0027  -137 ± 0.80  -137 ± 0.78
#>  7  -130 ± NA  -0.0070 ± 0.0030  -137 ± 0.79  -136 ± 0.67
#>  8  -129 ± NA  -0.0081 ± 0.0031  -136 ± 0.76  -135 ± 0.60
#>  9  -129 ± NA  -0.0092 ± 0.0031  -136 ± 0.84  -135 ± 0.53
#> 10  -126 ± NA  -0.0104 ± 0.0033  -133 ± 0.81  -133 ± 0.72
#> # ℹ 10,388 more rows
```
