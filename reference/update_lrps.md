# Update an LRPS

Update the behaviour of a likelihood-restricted prior sampler with
interim information from a nested sampling run.

## Usage

``` r
update_lrps(x, ...)

# S3 method for class 'multi_ellipsoid'
update_lrps(x, unit = NULL, log_volume = NA, ...)

# S3 method for class 'rwmh_cube'
update_lrps(x, unit = NULL, ...)

# S3 method for class 'slice_rectangle'
update_lrps(x, unit = NULL, ...)

# S3 method for class 'unif_ellipsoid'
update_lrps(x, unit = NULL, log_volume = NA, ...)
```

## Arguments

- x:

  \[[ernest_lrps](https://kylesnap.github.io/ernest/reference/new_ernest_lrps.md)\]  
  A likelihood-restricted prior sampler.

- ...:

  Presently ignored.

- unit:

  `[matrix(double(), integer(), x$n_dim)]`  
  The current live set stored within the run. Optional; if NULL no LRPS
  updates based on the state of the live set will be made.

- log_volume:

  `[double(1)]`  
  The current log-volume of the nested sampling run.

## Value

\[[ernest_lrps](https://kylesnap.github.io/ernest/reference/new_ernest_lrps.md)\],
created by reconstructing `x` with updated parameters.

## Details

During a nested sampling run, you may wish to update the internal
parameters of the LRPS based on sampler performance or other criterion.
The frequency of these updates is set by the `first_update` and
`update_interval` arguments of
[`ernest_sampler()`](https://kylesnap.github.io/ernest/reference/ernest_sampler.md).

If you are creating your own
[ernest_lrps](https://kylesnap.github.io/ernest/reference/new_ernest_lrps.md)
subclass, implement this method to specify any special update behaviour.
The default method reconstructs the LRPS with current parameters and
resets the likelihood call counter in the cache.
