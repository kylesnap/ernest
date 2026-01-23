# matrix_fn and fn args are similar / produces likelihoods from `fn`

    Code
      ll
    Message
      Log-likelihood Function (Auto-Generated Matrix Compatibility)
      function (x) 
      sum(stats::dnorm(x, mean = c(-1, 0, 1), log = TRUE))

# matrix_fn and fn args are similar / produces likelihood from `matrix_fn`

    Code
      mat_ll
    Message
      Log-likelihood Function (User-Provided Matrix Compatibility)
      function (x) 
      mvtnorm::dmvnorm(x, mean = c(-1, 0, 1), log = TRUE)

