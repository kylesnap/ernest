# Prepare a new nested sampling run

Initializes an `ernest_sampler` object containing the components
required to perform nested sampling. This object can then be used to
build sequences of nested samples with
[`generate()`](https://generics.r-lib.org/reference/generate.html).

## Usage

``` r
ernest_sampler(
  log_lik,
  prior,
  sampler = rwmh_cube(),
  n_points = 500,
  first_update = NULL,
  update_interval = NULL,
  seed = NULL
)
```

## Arguments

- log_lik:

  A function which takes a vector of parameters as a vector and returns
  the log-likelihood of a given model. Wrapped using
  [`create_likelihood()`](https://kylesnap.github.io/ernest/reference/create_likelihood.md)
  unless it is already of class `ernest_likelihood`.

- prior:

  An object with class
  [ernest_prior](https://kylesnap.github.io/ernest/reference/create_prior.md).
  Describes the prior space within which to generate sample parameters
  for `log_lik`.

- sampler:

  An object of class
  [ernest_lrps](https://kylesnap.github.io/ernest/reference/new_ernest_lrps.md).
  Describes the likelihood-restricted prior sampling technique to adopt
  during the run.

- n_points:

  A strictly positive integer. The number of live points to use in the
  nested sampling run.

- first_update:

  An optional positive integer. The number of likelihood calls to make
  with the default [uniform
  LRPS](https://kylesnap.github.io/ernest/reference/unif_cube.md) method
  before swapping to the technique described by `sampler`. If left
  `NULL`, this is set to `n_points * 2.5`.

- update_interval:

  An optional positive integer. The number of likelihood calls between
  updates to the `sampler` object. If `NULL`, this is set to
  `n_points * 1.5`.

- seed:

  An optional integer. Sets the random seed controlling the random
  number generator for nested sampling runs, which is stored in the
  resulting `ernest_sampler` as an attribute. If `NULL`, this is set to
  a random integer.

## Value

An object of class `ernest_sampler`, which is a list containing the
inputs used as arguments to this function, along with an environment
`run_env` which is used to store the `n_points` live particles
throughout a nested sampling run.

## Details

The `ernest_sampler` object is tested with
[`compile()`](https://generics.r-lib.org/reference/compile.html) before
it is returned. This helps to catch errors with the likelihood and prior
specifications. If this compilation step fails, review your `log_lik_fn`
and `prior` objects for their compliance.

## Verbosity

Messages from ernest can be silenced with the global options
`rlib_message_verbosity` and `rlib_warning_verbosity`. These options
take the values:

- "default": Verbose unless the `.frequency` argument is supplied.

- "verbose": Always verbose.

- "quiet": Always quiet.

When set to quiet, messages are not displayed and the condition is not
signaled. See
[`rlang::abort()`](https://rlang.r-lib.org/reference/abort.html) for
more information.

## See also

- [`create_likelihood()`](https://kylesnap.github.io/ernest/reference/create_likelihood.md)
  describes the requirements of the `log_lik_fn` parameter.

- [`create_prior()`](https://kylesnap.github.io/ernest/reference/create_prior.md)
  describes the requriements of the `prior` parameter.

- [ernest_lrps](https://kylesnap.github.io/ernest/reference/new_ernest_lrps.md)
  describes the general requirements of likelihood-restricted prior
  samplers.

## Examples

``` r
prior <- create_uniform_prior(lower = c(-1, -1), upper = 1)
#> New names:
#> • `Uniform` -> `Uniform...1`
#> • `Uniform` -> `Uniform...2`
ll_fn <- function(x) -sum(x^2)
sampler <- ernest_sampler(ll_fn, prior, n_points = 100)
sampler
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

# Use a unit-cube LRPS (not recommended in practice)
unit_sampler <- ernest_sampler(
  ll_fn,
  prior,
  n_points = 100,
  sampler = unif_cube()
)
unit_sampler
#> nested sampling specification <ernest_sampler>
#> • No. Points: 100
#> • LRPS Method: unif_cube
#> 
#> ernest LRPS method <unif_cube/ernest_lrps>
#> • Dimensions: 2
#> • No. Log-Lik Calls: 0
```
