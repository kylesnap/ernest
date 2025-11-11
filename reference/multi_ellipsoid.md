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
multi_ellipsoid(enlarge = 1.25, min_reduction = 0.7, allow_contact = TRUE)
```

## Arguments

- enlarge:

  Double, greater than or equal to 1. Factor by which to inflate the
  bounding ellipsoid's volume before sampling (see Details).

- min_reduction:

  Double between 0 and 1. The minimum reduction in total volume required
  for an ellipsoid to be split in two. Lower values lead to more
  aggressive splitting.

- allow_contact:

  Logical. Whether to allow ellipsoids to overlap.

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

4.  The split ellipsoids are accepted if all of the following conditions
    are met:

    - Both ellipsoids are non-degenerate

    - The combined volume of the split ellipsoids is less than
      \\min\_{red.} \* V\\

    - (If `allow_contact` is `FALSE`) the ellipsoids do not intersect.

5.  Steps 2–4 are repeated recursively on each new ellipsoid until no
    further splits are accepted, updating \\V\\ to the volume of the
    currently split ellipsoid.

## Ellipsoids

Ellipsoids are stored in the `cache` environment of the LRPS object.
Ellipsoids are defined by their centre \\c\\ and shape matrix \\A\\. The
set of points \\x\\ contained within the ellipsoid is given by \$\$ x
\in {\bf{R}}^n \| (x-c) A (x-c)' \leq 1 \$\$

The volume of the ellipsoid is \\V = \mathrm{Vol}(S_n)
\sqrt{\det(A^{-1})}\\, where \\\mathrm{Vol}(S_n)\\ is the volume of the
unit hypersphere.

For sampling, we store the matrix \\A^{-1/2}\\, the inverse of the
positive-semidefinite square root of \\A\\. The ellipsoid can
equivalently be defined as the set of points \$\$x = A^{-1/2} y + c,\$\$
where \\y\\ are points from the unit hypersphere.

For more on ellipsoids and their operations, see [Algorithms for
Ellipsoids](http://tcg.mae.cornell.edu/pubs/Pope_FDA_08.pdf) by S.B.
Pope, Cornell University Report FDA 08-01 (2008).

## References

Feroz, F., Hobson, M. P., Bridges, M. (2009) MULTINEST: An Efficient and
Robust Bayesian Inference Tool for Cosmology and Particle Physics.
Monthly Notices of the Royal Astronomical Society. 398(4), 1601–1614.
[doi:10.1111/j.1365-2966.2009.14548.x](https://doi.org/10.1111/j.1365-2966.2009.14548.x)

For implementation, see:
https://github.com/kbarbary/nestle/blob/master/runtests.py

## See also

Other ernest_lrps:
[`mini_balls()`](https://kylesnap.github.io/ernest/reference/mini_balls.md),
[`no_underrun()`](https://kylesnap.github.io/ernest/reference/no_underrun.md),
[`rwmh_cube()`](https://kylesnap.github.io/ernest/reference/rwmh_cube.md),
[`slice_rectangle()`](https://kylesnap.github.io/ernest/reference/slice_rectangle.md),
[`unif_cube()`](https://kylesnap.github.io/ernest/reference/unif_cube.md),
[`unif_ellipsoid()`](https://kylesnap.github.io/ernest/reference/unif_ellipsoid.md)

## Examples

``` r
data(example_run)
lrps <- multi_ellipsoid(enlarge = 1.25, min_reduction = 0.5)

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
#> • Min Reduction: 0.5
#> • Allow Contact: TRUE
#> • Enlargement: 1.25
```
