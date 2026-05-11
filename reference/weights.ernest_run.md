# Extract the posterior sample weights from a nested sampling run

Return the normalised posterior importance weights for the dead points
in a nested sampling run.

## Usage

``` r
# S3 method for class 'ernest_run'
weights(object, log = FALSE, ...)
```

## Arguments

- object:

  \[[ernest_run](https://kylesnap.github.io/ernest/reference/generate-ernest.md)\]  
  A nested sampling run.

- log:

  `[logical(1)]`  
  Whether to return the weights on the log scale.

- ...:

  These dots are for future extensions and must be empty.

## Value

`[double()]` A numeric vector of normalised importance weights. When
`log = FALSE`, the values are exponentiated so they sum to one.

## Details

The log-weights in a nested sampling run are the individual
contributions of each sample to the log-evidence estimate. The
unnormalised weight of the \\i\\th sampled point is given as \$\$w_i =
\frac{L\_{i-1} + L_i}{2} \* (V\_{i-1} - V_i)\$\$ where \\L_i\\ is the
likelihood value for the point and \\V_i\\ is the prior volume at which
the point was sampled.

The posterior importance weights are obtained by normalising the
log-weights with the final log-evidence estimate. They can be used to
reweight posterior samples from the run so they approximate the
posterior distribution.

## See also

[as_draws.ernest_run](https://kylesnap.github.io/ernest/reference/as_draws.ernest_run.md)

## Examples

``` r
data(example_run)
weights(example_run) |> head()
#> [1] 4.661608e-59 4.677800e-57 2.152569e-56 3.433589e-56 5.397510e-56
#> [6] 5.756538e-55
weights(example_run, log = TRUE) |> head()
#> [1] -134.3132 -129.7045 -128.1781 -127.7112 -127.2588 -124.8918
```
