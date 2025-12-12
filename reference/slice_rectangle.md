# Generate samples with slice sampling

**\[experimental\]** Create new live points by evolving a current live
point through slice sampling within a bounding hyperrectangle, shrinking
the rectangle when proposals are rejected.

## Usage

``` r
slice_rectangle(enlarge = 1)
```

## Arguments

- enlarge:

  Optional double, greater than or equal to 1. Factor by which to
  inflate the hyperrectangle's volume before sampling (see Details).

## Value

An object of class `c("slice_rectangle", "ernest_lrps")` that can be
used with
[`ernest_sampler()`](https://kylesnap.github.io/ernest/reference/ernest_sampler.md)
to specify the sampling behaviour.

## Details

The slice LRPS generates proposals by uniformly sampling within a
bounding hyperrectangle that contains regions of the parameter space
satisfying the likelihood criterion. Sampling begins by selecting a
known live point \\\theta\\ that satisfies the criterion. Each iteration
proposes a new point within this rectangle via uniform sampling and
compares it against the criterion; if rejected, a new hyperrectangle is
drawn such that the proposed point is on its boundary and \\\theta\\ is
in its interior. This continues until either a valid proposal is found
or the rectangle has shrunk to the point where no further clamping
operations can be performed.

By default, the hyperrectangle spans the extreme values of the current
set of live points in each dimension. This may risk excluding valid
regions of the parameter space, particularly where the posterior is
multimodal or highly non-Gaussian. To mitigate this, set `enlarge > 1`,
which inflates the hyperrectagle's volume by the specified factor before
sampling. Setting `enlarge` to `NA` disables this behaviour, instead
slicing from the unit hypercube at each iteration.

## References

Neal, R. M. (2000). Slice Sampling (Version 1). arXiv.
[doi:10.48550/ARXIV.PHYSICS/0009028](https://doi.org/10.48550/ARXIV.PHYSICS/0009028)

## See also

Other ernest_lrps:
[`multi_ellipsoid()`](https://kylesnap.github.io/ernest/reference/multi_ellipsoid.md),
[`rwmh_cube()`](https://kylesnap.github.io/ernest/reference/rwmh_cube.md),
[`unif_cube()`](https://kylesnap.github.io/ernest/reference/unif_cube.md),
[`unif_ellipsoid()`](https://kylesnap.github.io/ernest/reference/unif_ellipsoid.md)

## Examples

``` r
# Basic usage with default parameters
lrps <- slice_rectangle()

# More patient sampler for complex posteriors
patient_lrps <- slice_rectangle(enlarge = 1.25)
```
