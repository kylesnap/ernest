# ernest_likelihood / produces scalar likelihoods

    Code
      ll
    Message
      Scalar Log-likelihood Function
      function (x) 
      {
          x <- matrix(x, ncol = length(x))
          distval <- stats::mahalanobis(x, center = mean, cov = sigma)
          exp(-(3 * log(2 * pi) + logdet + distval)/2)
      }

# ernest_likelihood / produces likelihood from `vectorized_fn`

    Code
      mat_ll
    Message
      Vectorized Log-likelihood Function
      function (x) 
      {
          distval <- stats::mahalanobis(x, center = mean, cov = sigma)
          exp(matrix(-(3 * log(2 * pi) + logdet + distval)/2, nrow = nrow(x)))
      }

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
      ! Couldn't calculate the log-lik of #.#, #.#, -#.#, #.#, #.#, #.#, #.#, -#.#, #.#, #.#, -#.#, #.#, #.#, -#.#, -#.#, #.#, #.#, -#.#, ..., -#.#, and -#.#.
      Caused by error:
      ! log-lik. values must be either finite or `-Inf`.
      x Detected non-viable value: `NA`.

---

    Code
      ernest_sampler(create_likelihood(ll_fn_missing, on_nonfinite = "warn"),
      gaussian_blobs$prior, seed = 42)
    Condition
      Warning:
      <ernest_sampler> threw a warning during compilation
      Caused by warning:
      ! Replacing `NA` with `-Inf`.
    Message
      Nested sampling run specification:
      * No. points: 500
      * Sampling method: 25-step random walk sampling (acceptance target = 50%)
      * Prior: uniform prior distribution with 2 dimensions (A and B)

