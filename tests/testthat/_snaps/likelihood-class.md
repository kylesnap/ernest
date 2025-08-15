# create_likelihood with simple function

    Code
      ll
    Output
      
      -- <ernest_likelihood> 
      function (x) 
      -sum((x - 1)^2)

# create_likelihood with rowwise_fn

    Code
      ll
    Output
      
      -- <ernest_likelihood> 
      function (x) 
      {
          LaplacesDemon::dmvn(x, mu = c(-1, 1), Sigma = diag(0.95, 
              nrow = 2))
      }

# create_likelihood throws errors

    object 'fn' of mode 'function' was not found

---

    `.nonfinite_action` must be one of "warn", "quiet", or "abort", not "blob".

---

    Exactly one of `fn` or `rowwise_fn` must be supplied.

# create_likelihood can forward arguments

    Code
      ll
    Output
      
      -- <ernest_likelihood> 
      <partialised>
      function (...) 
      ~rowwise_fn(mu = ~c(-1, 1), Sigma = c(0.95, 0, 0, 0.95), ...)
      <environment ADDRESS>

# non_finite action options

    `lik(theta)` must always return finite double values or `-Inf`.
    x `lik(theta)` returned NaN.
    i Did you set `.nonfinite_action` with `create_likelihood()`)?

---

    `lik(theta)` must always return finite double values or `-Inf`.
    x `lik(theta)` returned NaN.
    i Did you set `.nonfinite_action` with `create_likelihood()`)?

---

    Replacing `NaN` with `-Inf`.

---

    Replacing `NaN` with `-Inf`.

# nonfinite_action options with auto_batch = FALSE

    `lik(theta)` must always return finite double values or `-Inf`.
    x `lik(theta)` returned Inf.
    i Did you set `.nonfinite_action` with `create_likelihood()`)?

---

    `lik(theta)` must always return finite double values or `-Inf`.
    x `lik(theta)` returned Inf.
    i Did you set `.nonfinite_action` with `create_likelihood()`)?

---

    Replacing `Inf` with `-Inf`.

---

    Replacing `Inf` with `-Inf`.

