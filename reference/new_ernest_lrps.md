# Create a new likelihood-restricted prior sampler (LRPS)

Constructs an LRPS, accepted by
[`ernest_sampler()`](https://kylesnap.github.io/ernest/reference/ernest_sampler.md)
and used to create new points during a nested sampling run.

## Usage

``` r
new_ernest_lrps(
  unit_log_fn = NULL,
  n_dim = NULL,
  max_loop = getOption("ernest.max_loop", 1000000L),
  cache = NULL,
  ...,
  .class = NULL,
  .call = caller_env()
)
```

## Arguments

- unit_log_fn:

  `[function]`  
  Takes a matrix of points in the unit cube and returns a numeric vector
  of log-likelihood values. Optional, and updated when called by
  [`ernest_sampler()`](https://kylesnap.github.io/ernest/reference/ernest_sampler.md).

- n_dim:

  `[integer(1)]`  
  Number of dimensions within the prior space. Optional, and updated
  when called by
  [`ernest_sampler()`](https://kylesnap.github.io/ernest/reference/ernest_sampler.md).

- max_loop:

  `[integer(1)]`  
  Maximum number of attempts to generate points. Inferred from the
  `ernest.max_loop` option.

- cache:

  `[environment]` Environment for caching information required for the
  specific LRPS method. Created if left `NULL`.

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Name-value pairs for additional elements for `ernest_lrps'` subclasses

- .class:

  `[character()]`  
  Names for the subclass of `ernest_lrps`.

- .call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

## Value

`[ernest_lrps]`, a named list containing the arguments provided.

## Details

Nested sampling relies on generating a series of points in the prior
space with increasing log-likelihood values. This is accomplished using
a likelihood-restricted prior sampler (LRPS), which generates
independent and identically distributed points from the prior, subject
to a hard likelihood constraint.

To create your own LRPS, subclass `new_ernest_lrps` and provide S3
methods for
[`propose()`](https://kylesnap.github.io/ernest/reference/propose.md)
and
[`update_lrps()`](https://kylesnap.github.io/ernest/reference/update_lrps.md)
for your subclass.
