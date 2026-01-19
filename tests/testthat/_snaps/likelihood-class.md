# create_likelihood throws errors

    Code
      create_likelihood("fn")
    Condition
      Error in `get()`:
      ! object 'fn' of mode 'function' was not found

---

    Code
      create_likelihood(fn, on_nonfinite = "blob")
    Condition
      Error in `new_ernest_likelihood()`:
      ! `on_nonfinite` must be one of "warn", "quiet", or "abort", not "blob".

---

    Code
      create_likelihood(fn, matrix_fn = matrix_fn)
    Condition
      Error in `create_likelihood()`:
      ! Exactly one of `fn` or `matrix_fn` must be supplied.

# matrix_fn and fn args are similar / produces likelihoods from `fn`

    Code
      ll
    Message
      Log-Likelihood Function (Auto-Generated Matrix Compatibility)
      function (x) 
      sum(stats::dnorm(x, mean = c(-1, 0, 1), log = TRUE))

# matrix_fn and fn args are similar / produces likelihood from `matrix_fn`

    Code
      mat_ll
    Message
      Log-Likelihood Function (User-Provided Matrix Compatibility)
      function (x) 
      mvtnorm::dmvnorm(x, mean = c(-1, 0, 1), log = TRUE)

