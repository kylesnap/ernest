# Throws errors when stop criteria are already passed

    `max_iterations` must be greater than the current number of iterations.
    i Can't set `max_iterations` to 50.
    i Already performed 100 iterations.

---

    `max_calls` must be greater than the current number of calls.
    i Can't set `max_calls` to 500.
    i Already performed 1025 calls.

---

    `min_logz` must be less than the estimated contribution of the remaining prior volume to the evidence.
    i Can't set `min_logz` to 1.
    i Current est. remaining contribution log volume is 0.049.

# Early exits are repaired

    Can't calculate the log. likelihood.
    x Can't calculate the likelihood without an error. Caused by error in `fn()`: ! Early exit

---

    Sampler can't contain a half-completed run.
    i Rolling-back sampler to 0 iterations.

---

    Can't calculate the log. likelihood.
    x Can't calculate the likelihood without an error. Caused by error in `fn()`: ! Early exit

---

    Sampler can't contain a half-completed run.
    i Rolling-back sampler to 100 iterations.

