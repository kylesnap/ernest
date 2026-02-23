# Generate new points from the unconstrained prior distribution

Use rejection sampling across the entire prior distribution to create
new samples. This is highly inefficient as an LRPS, but may be useful
for testing the behaviour of a nested sampling specification.

## Usage

``` r
unif_cube()
```

## Value

`[unif_cube]`, a named list that inherits from
\[[ernest_lrps](https://kylesnap.github.io/ernest/reference/new_ernest_lrps.md)\].

## References

Speagle, J. S. (2020). Dynesty: A Dynamic Nested Sampling Package for
Estimating Bayesian Posteriors and Evidences. Monthly Notices of the
Royal Astronomical Society, 493, 3132–3158.
[doi:10.1093/mnras/staa278](https://doi.org/10.1093/mnras/staa278)

## See also

Other ernest_lrps:
[`multi_ellipsoid()`](https://kylesnap.github.io/ernest/reference/multi_ellipsoid.md),
[`rwmh_cube()`](https://kylesnap.github.io/ernest/reference/rwmh_cube.md),
[`slice_rectangle()`](https://kylesnap.github.io/ernest/reference/slice_rectangle.md),
[`unif_ellipsoid()`](https://kylesnap.github.io/ernest/reference/unif_ellipsoid.md)

## Examples

``` r
data(example_run)
lrps <- unif_cube()

ernest_sampler(example_run$log_lik_fn, example_run$prior, sampler = lrps)
#> Nested sampling run specification:
#> * No. points: 500
#> * Sampling method: Uniform unit cube sampling
#> * Prior: uniform prior distribution with 3 dimensions (x, y, and z)
```
