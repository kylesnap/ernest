# Specify a prior with uniformly distributed marginals

A specialisation of
[`create_prior()`](https://kylesnap.github.io/ernest/reference/create_prior.md)
where the parameter space is described by independent uniform marginals.

## Usage

``` r
create_uniform_prior(
  lower = 0,
  upper = 1,
  names = "Uniform",
  .n_dim = NULL,
  .name_repair = c("unique", "universal", "check_unique")
)
```

## Arguments

- lower, upper:

  Numeric vector of bounds for the uniform distribution.

- names:

  An optional character vector. Names for the variables in the prior
  distribution.

- .n_dim:

  An optional positive integer. The number of dimensions of the prior
  distribution.

- .name_repair:

  An optional, case-sensitive string. How to repair `names`. Options are
  `"unique"` (default), `"universal"`, or `"check_unique"`. See
  [`vctrs::vec_as_names()`](https://vctrs.r-lib.org/reference/vec_as_names.html)
  for details.

## Value

A `uniform_prior`, a subclass of `ernest_prior` with an efficient
implementation of the unit hypercube transformation.

## Note

See
[vctrs::vector_recycling_rules](https://vctrs.r-lib.org/reference/theory-faq-recycling.html)
for additional information on how parameters are recycled to a common
length.

## See also

Other special_priors:
[`create_normal_prior()`](https://kylesnap.github.io/ernest/reference/create_normal_prior.md)

## Examples

``` r
prior <- create_uniform_prior(lower = c(3, -2), upper = c(5, 4))
#> New names:
#> • `Uniform` -> `Uniform...1`
#> • `Uniform` -> `Uniform...2`
prior$fn(c(0.33, 0.67))
#> [1] 3.66 2.02
```
