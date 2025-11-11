# Generate samples from the unconstrained prior distribution

Use rejection sampling across the entire prior distribution to create
new live points. This is highly inefficient as an LRPS, but may be
useful for testing the behaviour of a nested sampling specification.

## Usage

``` r
unif_cube()
```

## Value

A list with class `c("unif_cube", "ernest_lrps")`. Can be used with
[`ernest_sampler()`](https://kylesnap.github.io/ernest/reference/ernest_sampler.md)
to specify the sampling behaviour of a nested sampling run.

## References

Speagle, J. S. (2020). Dynesty: A Dynamic Nested Sampling Package for
Estimating Bayesian Posteriors and Evidences. Monthly Notices of the
Royal Astronomical Society, 493, 3132–3158.
[doi:10.1093/mnras/staa278](https://doi.org/10.1093/mnras/staa278)

## See also

Other ernest_lrps:
[`mini_balls()`](https://kylesnap.github.io/ernest/reference/mini_balls.md),
[`multi_ellipsoid()`](https://kylesnap.github.io/ernest/reference/multi_ellipsoid.md),
[`no_underrun()`](https://kylesnap.github.io/ernest/reference/no_underrun.md),
[`rwmh_cube()`](https://kylesnap.github.io/ernest/reference/rwmh_cube.md),
[`slice_rectangle()`](https://kylesnap.github.io/ernest/reference/slice_rectangle.md),
[`unif_ellipsoid()`](https://kylesnap.github.io/ernest/reference/unif_ellipsoid.md)

## Examples

``` r
data(example_run)
lrps <- unif_cube()

ernest_sampler(example_run$log_lik_fn, example_run$prior, sampler = lrps)
#> nested sampling specification <ernest_sampler>
#> • No. Points: 500
#> • LRPS Method: unif_cube
#> 
#> ernest LRPS method <unif_cube/ernest_lrps>
#> • Dimensions: 3
#> • No. Log-Lik Calls: 0
```
