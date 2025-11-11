# Update an LRPS

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

## Usage

``` r
update_lrps(x, ...)

# S3 method for class 'mini_balls'
update_lrps(x, unit = NULL, ...)

# S3 method for class 'multi_ellipsoid'
update_lrps(x, unit = NULL, log_volume = NULL, ...)

# S3 method for class 'no_underrun'
update_lrps(x, unit = NULL, ...)

# S3 method for class 'rwmh_cube'
update_lrps(x, unit = NULL, ...)

# S3 method for class 'slice_rectangle'
update_lrps(x, unit = NULL, ...)

# S3 method for class 'unif_ellipsoid'
update_lrps(x, unit = NULL, ...)
```

## Arguments

- x:

  An `ernest_lrps` object.

- unit:

  A matrix of live points within the sampler. If NULL, no LRPS updates
  based on the state of the live points will be made.

- log_volume:

  The current log-volume of the nested sampling run.

## Value

An updated `ernest_lrps` object with the same class as `x`, possibly
with updated parameters.
