# Compile a set of live points for nested sampling

Prepares an object for nested sampling by validating and (re)generating
its set of live points. This ensures the sampler is viable before new
live points are generated during the nested sampling algorithm.

## Usage

``` r
# S3 method for class 'ernest_sampler'
compile(object, ..., seed = deprecated())

# S3 method for class 'ernest_run'
compile(object, ..., seed = deprecated(), clear = FALSE)
```

## Arguments

- object:

  An
  [ernest_sampler](https://kylesnap.github.io/ernest/reference/ernest_sampler.md)
  or
  [ernest_run](https://kylesnap.github.io/ernest/reference/generate.ernest_sampler.md)
  object.

  - For `ernest_sampler`: Prepares a new sampler with a fresh set of
    live points.

  - For `ernest_run`: Regenerates live points from previous results,
    unless `clear = TRUE`.

- ...:

  These dots are for future extensions and must be empty.

- seed:

  **\[deprecated\]** `seed` is no longer supported; set RNG with the
  `seed` argument of
  [`ernest_sampler()`](https://kylesnap.github.io/ernest/reference/ernest_sampler.md).

- clear:

  Logical. If `TRUE`, clears results from previous runs before
  compiling. If `FALSE`, retains previous results and validates live
  points.

## Value

A validated `object`, with a valid set of live points stored in its
`run_env` environment.

## Details

[`compile()`](https://generics.r-lib.org/reference/compile.html)
validates the set of live points in the sampler or run, ensuring:

- Each live point is within the unit hypercube.

- The likelihood function returns valid values (finite double or `-Inf`)
  for each point.

- The set of live points is not a perfect plateau (all points sharing
  the same likelihood). A warning is issued if more than 25% of points
  share the same likelihood value.

If validation fails, the set of live points is removed, preventing
further sampling until the issue is resolved.

## See also

- [`ernest_sampler()`](https://kylesnap.github.io/ernest/reference/ernest_sampler.md)
  for creating an `ernest_sampler` object.

- [`generate()`](https://generics.r-lib.org/reference/generate.html) for
  running nested sampling and details on the `ernest_run` object.

## Examples

``` r
prior <- create_uniform_prior(lower = c(-1, -1), upper = 1)
#> New names:
#> • `Uniform` -> `Uniform...1`
#> • `Uniform` -> `Uniform...2`
ll_fn <- function(x) -sum(x^2)
sampler <- ernest_sampler(ll_fn, prior, n_points = 100)

# Compile the sampler to add live points
compile(sampler)
#> ℹ Created 100 live points.
#> nested sampling specification <ernest_sampler>
#> • No. Points: 100
#> • LRPS Method: rwmh_cube
#> 
#> ernest LRPS method <rwmh_cube/ernest_lrps>
#> • Dimensions: 2
#> • No. Log-Lik Calls: 0
#> • No. Accepted Proposals: 0
#> • No. Steps: 25
#> • Target Acceptance: 0.5
#> • Step Size: 1.000
head(sampler$run_env$unit)
#>            [,1]       [,2]
#> [1,] 0.02310297 0.01008702
#> [2,] 0.03245055 0.98445262
#> [3,] 0.07092365 0.01616990
#> [4,] 0.98744005 0.92027590
#> [5,] 0.92899679 0.04063944
#> [6,] 0.06389842 0.95245616

# Continue a previous run
# run <- data(example_run)
# sampler_2 <- compile(example_run)
# sampler_2

# Make a new sampler from a previous run
sampler_3 <- compile(example_run, clear = TRUE)
#> ℹ Created 1000 live points.
sampler_3
#> nested sampling specification <ernest_sampler>
#> • No. Points: 1000
#> • LRPS Method: rwmh_cube
#> 
#> ernest LRPS method <rwmh_cube/ernest_lrps>
#> • Dimensions: 3
#> • No. Log-Lik Calls: 0
#> • No. Accepted Proposals: 0
#> • No. Steps: 25
#> • Target Acceptance: 0.5
#> • Step Size: 1.000
```
