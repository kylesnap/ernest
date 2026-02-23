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

