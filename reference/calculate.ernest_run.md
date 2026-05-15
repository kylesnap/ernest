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

  \[[ernest_run](https://kylesnap.github.io/ernest/reference/generate-ernest.md)\]  
  Results from a nested sampling run.

- ndraws:

  `[integer(1)]`  
  The number of log-volume sequences to simulate. If equal to zero,
  log-volume simulation is skipped and error in the log-evidence
  estimates is approximated with analytical error estimates.

- ...:

  These dots are for future extensions and must be empty.

## Value

An `ernest_estimate` object, which inherits from `tbl_df`, `tbl`, and
`data.frame`.

The iterative estimates from the nested sampling run. Contains the
following columns for each point:

- `log_lik`: `[[double()]]` The log-likelihood of the point.

- `log_volume`: `[[posterior::rvar()]]` The log-volume of the prior
  space associated with the point.

- `log_weight`: `[[posterior::rvar()]]` The estimated contribution of
  the point to the log-evidence estimate (i.e., the unnormalized
  posterior log-weight).

- `log_evidence`: `[[posterior::rvar()]]` The current log-evidence
  estimate.

If `ndraws > 0`, `log_volume`, `log_weight`, and `log_evidence` each
contain `ndraws` simulated draws per iteration.

If `ndraws = 0`, `log_volume` and `log_weight` contain a single
deterministic draw per iteration, and `log_evidence` contains draws from
a normal approximation based on analytical variance estimates (see the
package vignetttes for more information). The number of draws is
controlled with getOption("posterior.rvar_ndraws"), with a default of
1000.

## References

Higson, E., Handley, W., Hobson, M., & Lasenby, A. (2019). Nestcheck:
Diagnostic Tests for Nested Sampling Calculations. Monthly Notices of
the Royal Astronomical Society, 483(2), 2044–2056.
[doi:10.1093/mnras/sty3090](https://doi.org/10.1093/mnras/sty3090)

## See also

[`weights.ernest_run()`](https://kylesnap.github.io/ernest/reference/weights.ernest_run.md)

## Examples

``` r
# Load an example run
data(example_run)

# View results and analytical evidence errors.
calculate(example_run, ndraws = 0)
#> # A tibble: 10,353 × 4
#>    log_lik   log_volume log_weight log_evidence
#>      <dbl>   <rvar[1d]> <rvar[1d]>   <rvar[1d]>
#>  1   -137.  -0.001 ± NA  -143 ± NA     -143 ± 0
#>  2   -132.  -0.002 ± NA  -139 ± NA     -139 ± 0
#>  3   -130.  -0.003 ± NA  -137 ± NA     -137 ± 0
#>  4   -130.  -0.004 ± NA  -137 ± NA     -136 ± 0
#>  5   -129.  -0.005 ± NA  -136 ± NA     -136 ± 0
#>  6   -127.  -0.006 ± NA  -134 ± NA     -134 ± 0
#>  7   -124.  -0.007 ± NA  -131 ± NA     -131 ± 0
#>  8   -123.  -0.008 ± NA  -130 ± NA     -130 ± 0
#>  9   -123.  -0.009 ± NA  -130 ± NA     -129 ± 0
#> 10   -122.  -0.010 ± NA  -129 ± NA     -128 ± 0
#> # ℹ 10,343 more rows

# Simulate 100 log-volume shrinkage sequences across the run.
calculate(example_run, ndraws = 100)
#> # A tibble: 10,353 × 4
#>    log_lik          log_volume   log_weight log_evidence
#>      <dbl>          <rvar[1d]>   <rvar[1d]>   <rvar[1d]>
#>  1   -137.  -0.00087 ± 0.00094  -144 ± 0.92  -144 ± 0.92
#>  2   -132.  -0.00192 ± 0.00146  -139 ± 0.81  -139 ± 0.80
#>  3   -130.  -0.00284 ± 0.00175  -138 ± 0.81  -137 ± 0.72
#>  4   -130.  -0.00383 ± 0.00221  -137 ± 0.74  -136 ± 0.63
#>  5   -129.  -0.00484 ± 0.00241  -136 ± 0.73  -136 ± 0.51
#>  6   -127.  -0.00586 ± 0.00261  -134 ± 0.77  -134 ± 0.61
#>  7   -124.  -0.00680 ± 0.00280  -132 ± 0.81  -131 ± 0.74
#>  8   -123.  -0.00795 ± 0.00293  -130 ± 0.79  -130 ± 0.71
#>  9   -123.  -0.00887 ± 0.00304  -130 ± 0.70  -129 ± 0.56
#> 10   -122.  -0.00980 ± 0.00305  -129 ± 0.74  -128 ± 0.54
#> # ℹ 10,343 more rows
```
