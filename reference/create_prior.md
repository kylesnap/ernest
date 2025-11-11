# Specify a prior distribution for nested sampling

Use an R function to specify the prior distribution of parameters for a
nested sampling run.

## Usage

``` r
create_prior(
  fn,
  names = NULL,
  lower = -Inf,
  upper = Inf,
  .n_dim = NULL,
  .name_repair = c("unique", "universal", "check_unique")
)
```

## Arguments

- fn:

  A function. Takes a vector of unit cube coordinates and returns a
  vector of parameters of the same length.

- names:

  An optional character vector. Names for the variables in the prior
  distribution.

- lower, upper:

  Numeric vectors. Expected bounds for the parameter vectors after
  hypercube transformation.

- .n_dim:

  An optional positive integer. The number of dimensions of the prior
  distribution.

- .name_repair:

  An optional, case-sensitive string. How to repair `names`. Options are
  `"unique"` (default), `"universal"`, or `"check_unique"`. See
  [`vctrs::vec_as_names()`](https://vctrs.r-lib.org/reference/vec_as_names.html)
  for details.

## Value

A list with with class `ernest_prior`, containing `fn`, `lower`,
`upper`, and `names`. The vector-valued parameters are guaranteed to be
of length `n_dim` if provided, or share a common length if otherwise
left `NULL`.

## Details

The unit hypercube transformation encodes points in the parameter space
as independent and identically distributed points within a unit
hypercube. Nested sampling implementations, including ernest, use this
transformation to simplify likelihood-restricted prior sampling and
avoid unnecessary rejection steps.

`create_prior` allows you to specify your own prior distribution by
providing a transformation function. For factorisable priors, this
function can simply transform each value in (0, 1) using the inverse
cumulative distribution function (CDF) for each parameter. For more
complex cases, you can specify hierarchical or conditionally dependent
priors (see Examples).

`create_prior` performs regularity checks on your prior function to
catch basic errors that may affect nested sampling. To pass these
checks, `fn` must be able to take in a vector of points (each between 0
and 1) and return a vector of the same length which contains only finite
values.

## Note

See
[vctrs::vector_recycling_rules](https://vctrs.r-lib.org/reference/theory-faq-recycling.html)
for additional information on how parameters are recycled to a common
length.

## Examples

``` r
# 3D uniform prior in the range [-10, 10]
unif <- function(x) {
   -10 + x * 20
}

prior <- create_prior(unif, lower = -10, upper = 10, .n_dim = 3)
#> New names:
#> • `` -> `...1`
#> • `` -> `...2`
#> • `` -> `...3`
prior$fn(c(0.25, 0.5, 0.75))
#> [1] -5  0  5

# A normal prior with parameterised mean and standard deviation
hier_f <- function(theta) {
  mu <- qnorm(theta[1], mean = 5) # mu ~ N(5, 1)
  sigma <- 10 ^ qunif(theta[2], min = -1, max = 1) # log10(sigma) ~ U[-1, 1]
  x <- qnorm(theta[3], mu, sigma) # X ~ N(mu, sigma)
  c(mu, sigma, x)
}
create_prior(
  hier_f,
  names = c("mu", "sigma", "x"),
  lower = c(-Inf, 0, -Inf)
)
#> custom prior distribution <ernest_prior>
#> 
#> # A tibble: 3 × 3
#>   names lower upper
#>   <chr> <dbl> <dbl>
#> 1 mu     -Inf   Inf
#> 2 sigma     0   Inf
#> 3 x      -Inf   Inf
```
