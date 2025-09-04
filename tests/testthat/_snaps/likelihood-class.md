# new_ernest_likelihood fails informatively

    `.nonfinite_action` must be one of "warn", "quiet", or "abort", not "loudly".

# create_likelihood with simple function

    Code
      ll
    Output
      Log-likelihood function <ernest_likelihood>
      function (x) 
      -sum((x - 1)^2)

# create_likelihood warns when rowwise_fn is used

    The `rowwise_fn` argument of `create_likelihood()` is deprecated as of ernest 1.1.0.
    i Please use the `fn` argument instead.

---

    Code
      ll
    Output
      Log-likelihood function <ernest_likelihood>
      function (x) 
      {
          LaplacesDemon::dmvn(x, mu = c(-1, 1), Sigma = diag(0.95, 
              nrow = 2))
      }

# create_likelihood throws errors

    object 'fn' of mode 'function' was not found

---

    `.nonfinite_action` must be one of "warn", "quiet", or "abort", not "blob".

# create_likelihood can forward arguments

    Code
      ll
    Output
      Log-likelihood function <ernest_likelihood>
      <partialised>
      function (...) 
      ~fn(mu = ~c(-1, 1), Sigma = c(0.95, 0, 0, 0.95), ...)
      <environment ADDRESS>

# non_finite action options

    log-lik. values must be either finite or `-Inf`, not NaN.
    i Did you set `.nonfinite_action` with `create_likelihood()`)?

---

    Replacing `NaN` with `-Inf`.

# fn fails if a non-double is returned

    log-lik. values must be scalar doubles, not the string "NA".

---

    log-lik. values must be scalar doubles, not the string "NA".

---

    log-lik. values must be scalar doubles, not the string "NA".

