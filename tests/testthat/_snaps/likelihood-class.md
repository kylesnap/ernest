# create_likelihood with auto_batch

    Code
      ll
    Output
      An <ernest_likelihood>: `nonfinite_action` = warn

# create_likelihood from an ernest_likelihood

    Code
      ll
    Output
      An <ernest_likelihood>: `nonfinite_action` = warn

# non_finite action options

    `lik(theta)` must always return finite double values or `-Inf`.
    x `lik(theta)` returned NaN.
    i Should you change `nonfinite_action` from "abort"?

---

    `lik(theta)` must always return finite double values or `-Inf`.
    x `lik(theta)` returned NaN.
    i Should you change `nonfinite_action` from "abort"?

---

    Replacing `NaN` with `-Inf`.

---

    Replacing `NaN` with `-Inf`.

# nonfinite_action options with auto_batch = FALSE

    `lik(theta)` must always return finite double values or `-Inf`.
    x `lik(theta)` returned Inf.
    i Should you change `nonfinite_action` from "abort"?

---

    `lik(theta)` must always return finite double values or `-Inf`.
    x `lik(theta)` returned Inf.
    i Should you change `nonfinite_action` from "abort"?

---

    Replacing `Inf` with `-Inf`.

---

    Replacing `Inf` with `-Inf`.

