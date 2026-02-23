# Compile the live set of points for nested sampling

Prepares an object for nested sampling by validating and (re)generating
its live set. This ensures the sampler is viable before new points are
drawn during the nested sampling algorithm.

## Usage

``` r
# S3 method for class 'ernest_sampler'
compile(object, ...)

# S3 method for class 'ernest_run'
compile(object, clear = FALSE, ...)
```

## Arguments

- object:

  [`[ernest_run]`](https://kylesnap.github.io/ernest/reference/generate.ernest_sampler.md)
  or
  [`[ernest_sampler]`](https://kylesnap.github.io/ernest/reference/ernest_sampler.md)  
  Results from a nested sampling run.

- ...:

  These dots are for future extensions and must be empty.

- clear:

  `[logical(1)]`  
  If `TRUE`, clears results from previous runs before compiling. If
  `FALSE`, retains previous results and validates the live set.

## Value

A copy of `[object]`.

The copy is guaranteed to have a valid live set, created according to
the class of `object` and the value of `clear`:

- If `object` is an `ernest_sampler`, or if `clear = TRUE`, a new live
  set is created from scratch.

- If `object` is an `ernest_run`, the live set is regenerated from
  previous results.

## Details

[`compile()`](https://generics.r-lib.org/reference/compile.html)
validates the live set bound to `object`, ensuring that:

- Each point in the set is within the unit hypercube.

- The likelihood function returns valid values (finite double or `-Inf`)
  for each point.

- The live set does not represent a perfect likelihood plateau (i.e.,
  that all points share the same likelihood). A warning is issued if
  more than 25% of points share the same likelihood value.

If validation fails, the live set is removed from `object`, preventing
further sampling until the issue is resolved.

## See also

- [`generate()`](https://generics.r-lib.org/reference/generate.html) for
  running samplers with compiled live sets.

## Examples

``` r
prior <- create_uniform_prior(lower = c(-1, -1), upper = 1)
ll_fn <- function(x) -sum(x^2)
sampler <- ernest_sampler(ll_fn, prior, nlive = 100)

# Compile the sampler to add a live set
compile(sampler)
#> Nested sampling run specification:
#> * No. points: 100
#> * Sampling method: 25-step random walk sampling (acceptance target = 50.0%)
#> * Prior: uniform prior distribution with 2 dimensions (Uniform_1 and Uniform_2)
head(sampler$run_env$unit)
#>            [,1]         [,2]
#> [1,] 0.03694823 0.9618287729
#> [2,] 0.06791685 0.9332903884
#> [3,] 0.14663727 0.0009992414
#> [4,] 0.85656860 0.0280483754
#> [5,] 0.06376257 0.8783412096
#> [6,] 0.05198476 0.8541909819

# Continue a previous run
# run <- data(example_run)
# sampler_2 <- compile(example_run)
# sampler_2

# Make a new sampler from a previous run
sampler_3 <- compile(example_run, clear = TRUE)
sampler_3
#> Nested sampling run specification:
#> * No. points: 1000
#> * Sampling method: 25-step random walk sampling (acceptance target = 50.0%)
#> * Prior: uniform prior distribution with 3 dimensions (x, y, and z)
```
