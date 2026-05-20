# ernest_likelihood / produces scalar likelihoods

    Code
      ll
    Message
      <ernest_likelihood>
      <function(x) {
        x <- matrix(x, ncol = length(x))
        distval <- stats::mahalanobis(x, center = mean, cov = sigma)
        exp(-(3 * log(2 * pi) + logdet + distval) / 2)
      }>
      v Interface: "scalar_fn"
      v Non-finite handling: "warn"

# ernest_likelihood / produces likelihood from `vectorized_fn`

    Code
      mat_ll
    Message
      <ernest_likelihood>
      <function(x) {
        distval <- stats::mahalanobis(x, center = mean, cov = sigma)
        exp(matrix(-(3 * log(2 * pi) + logdet + distval) / 2, nrow = nrow(x)))
      }>
      v Interface: "vectorized_fn"
      v Non-finite handling: "warn"

# Missing values in the log-likelihood

    Code
      ernest_sampler(log_lik = create_likelihood(ll_fn_missing, on_nonfinite = "abort"),
      prior = gaussian_blobs$prior, seed = 42)
    Condition
      Error in `ernest_sampler()`:
      ! <ernest_sampler> cannot compile.
      Caused by error in `compile()`:
      ! Error when creating the live set.
      Caused by error:
      ! `log_lik` cannot return NA.

---

    Code
      ernest_sampler(create_likelihood(ll_fn_missing, on_nonfinite = "warn"),
      gaussian_blobs$prior, seed = 42)
    Condition
      Warning:
      <ernest_sampler> threw a warning during compilation
      Caused by warning:
      ! Replacing log-lik. values with `-Inf`: NA
    Message
      Nested sampling run specification:
      * No. points: 500
      * Sampling method: 25-step random walk sampling (acceptance target = 50%)
      * Prior: <uniform_prior/ernest_prior> (2 dims.)

