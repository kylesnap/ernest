# Specify a prior with normally distributed marginals

A specialisation of
[`create_prior()`](https://kylesnap.github.io/ernest/reference/create_prior.md)
where the parameter space is described by independent normal variables,
possibly truncated.

## Usage

``` r
create_normal_prior(
  mean = 0,
  sd = 1,
  names = "Normal",
  lower = -Inf,
  upper = Inf,
  .n_dim = NULL,
  .name_repair = c("unique", "universal", "check_unique")
)
```

## Arguments

- mean:

  Numeric vector of means.

- sd:

  Numeric vector of standard deviations (must be strictly positive.)

- names:

  An optional character vector. Names for the variables in the prior
  distribution.

- lower, upper:

  Numeric vector of bounds for a truncated normal distribution.

- .n_dim:

  An optional positive integer. The number of dimensions of the prior
  distribution.

- .name_repair:

  An optional, case-sensitive string. How to repair `names`. Options are
  `"unique"` (default), `"universal"`, or `"check_unique"`. See
  [`vctrs::vec_as_names()`](https://vctrs.r-lib.org/reference/vec_as_names.html)
  for details.

## Value

A `normal_prior`, a subclass of `ernest_prior` with an efficient
implementation of the unit hypercube transformation.

## Note

See
[vctrs::vector_recycling_rules](https://vctrs.r-lib.org/reference/theory-faq-recycling.html)
for additional information on how parameters are recycled to a common
length.

## See also

- [`create_prior()`](https://kylesnap.github.io/ernest/reference/create_prior.md)
  for more on the `ernest_prior` object.

- [`truncnorm::qtruncnorm()`](https://rdrr.io/pkg/truncnorm/man/dtruncnorm.html)
  for the truncated normal quantile function.

Other special_priors:
[`create_uniform_prior()`](https://kylesnap.github.io/ernest/reference/create_uniform_prior.md)

## Examples

``` r
prior <- create_normal_prior(.n_dim = 3)
#> New names:
#> • `Normal` -> `Normal...1`
#> • `Normal` -> `Normal...2`
#> • `Normal` -> `Normal...3`
prior$fn(c(0.25, 0.5, 0.75))
#> [1] -0.6744898  0.0000000  0.6744898
```
