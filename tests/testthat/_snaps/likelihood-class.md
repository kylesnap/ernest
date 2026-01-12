# new_ernest_likelihood fails informatively

    `on_nonfinite` must be one of "warn", "quiet", or "abort", not "loudly".

# create_likelihood with simple function

    Code
      ll
    Message
      likelihood function <ernest_likelihood>
      function (x) 
      -sum((x - 1)^2)

# create_likelihood throws errors

    Code
      create_likelihood("fn")
    Condition
      Error in `get()`:
      ! object 'fn' of mode 'function' was not found

---

    Code
      create_likelihood(test, on_nonfinite = "blob")
    Condition
      Error in `new_ernest_likelihood()`:
      ! `on_nonfinite` must be one of "warn", "quiet", or "abort", not "blob".

# non_finite action options

    Code
      fail_ll(c(0, 1, 2))
    Condition
      Error:
      ! log-lik. values must be either finite or `-Inf`, not NaN.

---

    Code
      warn_ll(c(0, 1, 2))
    Condition
      Warning:
      Replacing `NaN` with `-Inf`.
    Output
      [1] -Inf

# fn fails if a non-double is returned

    Code
      fail_ll(c(0, 1, 2))
    Condition
      Error:
      ! Can't convert `log_lik(x)` <character> to <double>.

---

    Code
      warn_ll(c(0, 1, 2))
    Condition
      Error:
      ! Can't convert `log_lik(x)` <character> to <double>.

---

    Code
      result <- pass_ll(c(0, 1, 2))
    Condition
      Error:
      ! Can't convert `log_lik(x)` <character> to <double>.

