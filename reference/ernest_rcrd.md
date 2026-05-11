# Storing nested sampling results

`ernest_rcrd` is a
\[[vctrs::new_rcrd](https://vctrs.r-lib.org/reference/new_rcrd.html)\]
class that stores metadata for all samples drawn from the nested
sampling algorithm, tracking the history of replacements in the live set
and each point's contribution to evidence estimation.

## Details

This object is designed to be used internally within the `ernest_run`
class to track the history of samples generated during a nested sampling
run. Generally, users will not interact with `ernest_rcrd` objects
directly, and instead will call methods on `ernest_run` objects which,
internally, manipulate the `ernest_rcrd` object.

## Fields

- `unit`:

  `[double(nvar)]` The sample's coordinates within the unit hypercube
  (prior to transformation via the prior distribution).

- `id`:

  `[integer(1)]` A unique identifier for each sample in the sequence of
  live points.

- `nlive`:

  `[integer(1)]` The number of effective live points remaining when the
  sample was removed from the live set. Generally corresponds to the
  `nlive` of the `ernest_run`, but can be lower if the sample found
  within a plateau in the log-likelihood function (see References) or if
  the sample is still in the live set at termination.

- `neval`:

  `[integer(1)]` The number of likelihood evaluations required by the
  sampler to generate this replacement point, reflecting the difficulty
  of sampling from the likelihood-restricted prior. Equal to `0` for
  points remaining in the live set at termination.

- `log_lik`:

  `[double(1)]` The log-likelihood at the sample.

- `birth_lik`:

  `[double(1)]` The log-likelihood threshold that this sample exceeded;
  the minimum likelihood constraint applied when generating it. Equals
  `-Inf` for samples in the initial live set.

## References

Fowlie, A., Handley, W., Su, L., Nested Sampling with Plateaus, Monthly
Notices of the Royal Astronomical Society, 503(1), 1199–1205,
[doi:10.1093/mnras/stab590](https://doi.org/10.1093/mnras/stab590)
