# Transform nested sampling runs to `draws` objects

Convert an
[ernest_run](https://kylesnap.github.io/ernest/reference/generate.ernest_sampler.md)
to a format supported by the
[posterior](https://mc-stan.org/posterior/reference/posterior-package.html)
package.

## Usage

``` r
# S3 method for class 'ernest_run'
as_draws(x, ..., units = c("original", "unit_cube"), radial = FALSE)

# S3 method for class 'ernest_run'
as_draws_matrix(x, ..., units = c("original", "unit_cube"), radial = FALSE)

# S3 method for class 'ernest_run'
as_draws_rvars(x, ..., units = c("original", "unit_cube"), radial = FALSE)
```

## Arguments

- x:

  An
  [ernest_run](https://kylesnap.github.io/ernest/reference/generate.ernest_sampler.md)
  object.

- ...:

  These dots are for future extensions and must be empty.

- units:

  Case-sensitive string. The scale for the sampled points:

  - `"original"`: Points are on the scale of the prior space.

  - `"unit_cube"`: Points are on the (0, 1) unit hypercube scale.

- radial:

  Logical. If `TRUE`, returns an additional column `.radial` containing
  the radial coordinate (i.e., the Euclidean norm) for each sampled
  point.

## Value

A [draws](https://mc-stan.org/posterior/reference/draws.html) object
containing posterior samples from the nested sampling run, with
importance weights (in log units).

The returned object type depends on the function used:

- For `as_draws` and `as_draws_matrix`, a
  [`posterior::draws_matrix()`](https://mc-stan.org/posterior/reference/draws_matrix.html)
  object (class `c("draws_matrix", "draws", "matrix")`).

- For `as_draws_rvars`, a
  [`posterior::draws_rvars()`](https://mc-stan.org/posterior/reference/draws_rvars.html)
  object (class `c("draws_rvars", "draws", "list")`).

## See also

- [`posterior::as_draws()`](https://mc-stan.org/posterior/reference/draws.html)
  for details on the `draws` object.

- [`posterior::resample_draws()`](https://mc-stan.org/posterior/reference/resample_draws.html)
  uses the log weights from ernest's output to produce a weighted
  posterior sample.

## Examples

``` r
# Load example run
library(posterior)
#> This is posterior version 1.6.1
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
weights(dm) |> head()
#> [1] 1.227464e-63 5.334820e-61 5.680337e-59 1.135948e-58 8.184433e-58
#> [6] 2.437934e-56

# Summarise points after resampling
dm |>
  resample_draws() |>
  summarize_draws()
#> # A tibble: 3 × 10
#>   variable     mean  median    sd   mad    q5   q95  rhat ess_bulk ess_tail
#>   <chr>       <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>    <dbl>
#> 1 x         0.00242  0.0197 0.957 0.938 -1.61  1.56  1.19    4441.     13.0
#> 2 y        -0.00349  0.0220 0.955 0.950 -1.61  1.53  1.18    4308.     12.8
#> 3 z        -0.0193  -0.0216 0.965 0.972 -1.62  1.55  1.20    4212.     13.1

# View the radial coordinate in unit space over the run
dm_rad <- as_draws_rvars(
  example_run,
  units = "unit_cube",
  radial = TRUE
)
plot(
  x = example_run$log_volume,
  y = draws_of(dm_rad$.radial),
  xlab = "Log-volume",
  ylab = "Radial coordinate"
)
```
