# Generate samples from multiple spanning ellipsoids

Partitions the prior space into a set of ellipsoids whose union bounds
the set of live points. Samples are created by randomly selecting an
ellipsoid (weighted by their respective volumes), then using it to
generate a random point as in
[unif_ellipsoid](https://kylesnap.github.io/ernest/reference/unif_ellipsoid.md).
Effective for multimodal posteriors where a single ellipsoid would be
inefficient.

## Usage

``` r
multi_ellipsoid(enlarge = 1.25)
```

## Arguments

- enlarge:

  Double, greater than or equal to 1. Factor by which to inflate the
  bounding ellipsoid's volume before sampling (see Details).

## Value

A list with class `c("multi_ellipsoid", "ernest_lrps")`. Use with
[`ernest_sampler()`](https://kylesnap.github.io/ernest/reference/ernest_sampler.md)
to specify nested sampling behaviour.

## Details

Nested likelihood contours for multimodal distributions are poorly
represented by a single ellipsoid. This method fits multiple ellipsoids
to better capture disconnected or elongated regions.

Ellipsoids are generated using the following procedure:

1.  A single ellipsoid is fit to the set of live points, with volume
    \\V\\.

2.  The live points are clustered into two groups using k-means
    clustering.

3.  Ellipsoids are fit to each cluster.

4.  The split ellipsoids are accepted if both ellipsoids are
    non-degenerate, and if the combined volume of the split ellipsoids
    is significantly smaller than the original ellipsoid (calculated
    using Bayes' Information Criterion).

5.  Steps 2–4 are repeated recursively on each new ellipsoid until no
    further splits are accepted, updating \\V\\ to the volume of the
    currently split ellipsoid.

## References

- Feroz, F., Hobson, M. P., Bridges, M. (2009) MULTINEST: An Efficient
  and Robust Bayesian Inference Tool for Cosmology and Particle Physics.
  Monthly Notices of the Royal Astronomical Society. 398(4), 1601–1614.
  [doi:10.1111/j.1365-2966.2009.14548.x](https://doi.org/10.1111/j.1365-2966.2009.14548.x)

- Speagle, J. S. (2020). Dynesty: A Dynamic Nested Sampling Package for
  Estimating Bayesian Posteriors and Evidences. Monthly Notices of the
  Royal Astronomical Society, 493, 3132–3158.
  [doi:10.1093/mnras/staa278](https://doi.org/10.1093/mnras/staa278)

## See also

Other ernest_lrps:
[`rwmh_cube()`](https://kylesnap.github.io/ernest/reference/rwmh_cube.md),
[`slice_rectangle()`](https://kylesnap.github.io/ernest/reference/slice_rectangle.md),
[`unif_cube()`](https://kylesnap.github.io/ernest/reference/unif_cube.md),
[`unif_ellipsoid()`](https://kylesnap.github.io/ernest/reference/unif_ellipsoid.md)

## Examples

``` r
data(example_run)
lrps <- multi_ellipsoid(enlarge = 1.25)

ernest_sampler(example_run$log_lik_fn, example_run$prior, sampler = lrps)
#> nested sampling specification <ernest_sampler>
#> • No. Points: 500
#> • LRPS Method: multi_ellipsoid
#> 
#> ernest LRPS method <multi_ellipsoid/ernest_lrps>
#> • Dimensions: 3
#> • No. Log-Lik Calls: 0
#> • No. Ellipsoids: 1
#> • Total Log Volume: 1.001
#> • Enlargement: 1.25
```
