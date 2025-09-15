# new_ernest_likelihood fails informatively

    `on_nonfinite` must be one of "warn", "quiet", or "abort", not "loudly".

# create_likelihood with simple function

    Code
      ll
    Output
      likelihood function <ernest_likelihood>
      
      function (x) 
      -sum((x - 1)^2)

# create_likelihood throws errors

    object 'fn' of mode 'function' was not found

---

    `on_nonfinite` must be one of "warn", "quiet", or "abort", not "blob".

# non_finite action options

    log-lik. values must be either finite or `-Inf`, not NaN.

---

    Replacing `NaN` with `-Inf`.

# fn fails if a non-double is returned

    Can't convert `log-lik.` <character> to <double>.

---

    Can't convert `log-lik.` <character> to <double>.

---

    Can't convert `log-lik.` <character> to <double>.

