# Generate samples from a p-norm ball

Propose new live points by selecting a random live point `c` and drawing
uniformly from a multidimensional p-norm ball with centre `c` and radius
`r`. `r` is set such that the ball encompassess at least one other live
point.

## Usage

``` r
mini_balls(enlarge = 1.1, method, p)
```

## Arguments

- enlarge:

  Double larger or equal to 1. Factor by which to inflate the ball's
  volume before sampling.

- method, p:

  Pick one of `method` and `p`: \* `method` Sets the distance measure to
  be used. This must be one of `"euclidean"`, `"maximum"`, or
  `"manhattan"`. \* `p` A positive number, indicating the p-norm to use.

## Value

A list with class `c("mini_ball", "ernest_lrps")`. Use with
[`ernest_sampler()`](https://kylesnap.github.io/ernest/reference/ernest_sampler.md)
to specify nested sampling behaviour.

## Details

The p-norm naturaly define a distance that makes the vector space a
metric space. For two points `x` and `y`, their distance in \\L^p\\
space is given by \$\$\|\|x-y\|\|\_p = \sigma{(x_i - y_i)^p}^{1/p}\$\$.

The p-norm ball is the set of all vectors such that the distance between
itself and the ball's centre `c` is less than the radius `r`. The
distance `r` is updated throughout a run to ensure that at least one
other live point is contained within the ball. To avoid biasing
proposals toward the center, inflating the volume of the ball before
sampling with `enlarge > 1` is recommended.

## References

Buchner, J. (2014). A Statistical Test for Nested Sampling Algorithms.
Statistics and Computing, 26(1–2), 383–392.
[doi:10.1007/s11222-014-9512-y](https://doi.org/10.1007/s11222-014-9512-y)

Buchner, J. (2019). Collaborative Nested Sampling: Big Data versus
Complex Physical Models. Publications of the Astronomical Society of the
Pacific, 131(1004), 108005.
[doi:10.1088/1538-3873/aae7fc](https://doi.org/10.1088/1538-3873/aae7fc)

## See also

Other ernest_lrps:
[`multi_ellipsoid()`](https://kylesnap.github.io/ernest/reference/multi_ellipsoid.md),
[`no_underrun()`](https://kylesnap.github.io/ernest/reference/no_underrun.md),
[`rwmh_cube()`](https://kylesnap.github.io/ernest/reference/rwmh_cube.md),
[`slice_rectangle()`](https://kylesnap.github.io/ernest/reference/slice_rectangle.md),
[`unif_cube()`](https://kylesnap.github.io/ernest/reference/unif_cube.md),
[`unif_ellipsoid()`](https://kylesnap.github.io/ernest/reference/unif_ellipsoid.md)

## Examples

``` r
data(example_run)
euclid_balls <- mini_balls(method = "euclidean")
euclid_balls <- mini_balls(p = 2)

# Suprenum balls (or L-infinity norm)
suprenum_balls <- mini_balls(method = "maximum")
suprenum_balls <- mini_balls(p = Inf, enlarge = 1.1)

ernest_sampler(
  example_run$log_lik_fn,
  example_run$prior,
  sampler = euclid_balls
)
#> nested sampling specification <ernest_sampler>
#> • No. Points: 500
#> • LRPS Method: mini_balls
#> 
#> ernest LRPS method <mini_balls/ernest_lrps>
#> • Dimensions: 3
#> • No. Log-Lik Calls: 0
#> • Distance: euclidean
#> • Radius: Undefined
#> • Enlargement: 1.1
```
