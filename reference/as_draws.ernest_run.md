# Transform nested sampling runs to `draws` objects

Access the posterior sample and weights from a nested sampling run as an
object supported by the
[posterior](https://mc-stan.org/posterior/reference/posterior-package.html)
package.

## Usage

``` r
# S3 method for class 'ernest_run'
as_draws(x, units = c("original", "unit_cube"), ...)

# S3 method for class 'ernest_run'
as_draws_rvars(x, units = c("original", "unit_cube"), ...)

# S3 method for class 'ernest_run'
as_draws_matrix(x, units = c("original", "unit_cube"), ...)
```

## Arguments

- x:

  [`[ernest_run]`](https://kylesnap.github.io/ernest/reference/generate-ernest.md)  
  Results from a nested sampling run.

- units:

  `[character(1)]`  
  The scale of the sampled points:

  - `"original"`: Points are on the scale of the prior space.

  - `"unit_cube"`: Points are on the (0, 1) unit hypercube scale.

- ...:

  These dots are for future extensions and must be empty.

## Value

[`posterior::draws_matrix()`](https://mc-stan.org/posterior/reference/draws_matrix.html)
or
[`posterior::draws_rvars()`](https://mc-stan.org/posterior/reference/draws_rvars.html)  
A object containing the posterior samples from the nested sampling run,
with a hidden `.weights` column containing the importance weights for
each sample.

## Note

To produce a weighted posterior sample, use
[`posterior::resample_draws()`](https://mc-stan.org/posterior/reference/resample_draws.html)
to reweigh an object from `as_draws` using its importance weights.

## See also

[`posterior::as_draws()`](https://mc-stan.org/posterior/reference/draws.html)

## Examples

``` r
library(posterior)
#> This is posterior version 1.7.0
#> 
#> Attaching package: ‘posterior’
#> The following objects are masked from ‘package:stats’:
#> 
#>     mad, sd, var
#> The following objects are masked from ‘package:base’:
#> 
#>     %in%, match
data(example_run)

# View importance weights
dm <- as_draws(example_run)
str(dm)
#>  'draws_matrix' num [1:10359, 1:4] -8.05 -7.27 9.1 -8.81 -9.44 ...
#>  - attr(*, "dimnames")=List of 2
#>   ..$ draw    : chr [1:10359] "1" "2" "3" "4" ...
#>   ..$ variable: chr [1:4] "x" "y" "z" ".log_weight"
#>  - attr(*, "nchains")= int 1
weights(dm) |> head()
#> [1] 4.661608e-59 4.677800e-57 2.152569e-56 3.433589e-56 5.397510e-56
#> [6] 5.756538e-55

# Summarise points after resampling
dm |>
  resample_draws() |>
  summarize_draws()
#> # A tibble: 3 × 10
#>   variable      mean    median    sd   mad    q5   q95  rhat ess_bulk ess_tail
#>   <chr>        <dbl>     <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
#> 1 x        -0.00681   0.00245  0.974 0.977 -1.62  1.58  1.17    4600.     12.7
#> 2 y        -0.00957  -0.00775  0.990 1.01  -1.62  1.63  1.20    4221.     12.9
#> 3 z         0.000411  0.000927 0.977 0.981 -1.58  1.60  1.17    4400.     12.9

# Extract the same coordinates in the unit space coordinates
dm_unit <- as_draws_rvars(example_run, units = "unit_cube")
str(dm_unit)
#> List of 4
#>  $ x          : rvar<10359>[1]  0.5 ± 0.14
#>  $ y          : rvar<10359>[1]  0.5 ± 0.14
#>  $ z          : rvar<10359>[1]  0.5 ± 0.14
#>  $ .log_weight: rvar<10359>[1]  -18 ± 18
#>  - attr(*, "class")= chr [1:3] "draws_rvars" "draws" "list"
```
