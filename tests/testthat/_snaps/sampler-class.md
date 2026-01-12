# ernest_sampler initializes correctly

    Code
      sampler
    Message
      Nested sampling run specification:
      * Live points: 500
      * Sampling method: 25-step random walk sampling (acceptance target = 50.0%)
      * Prior: uniform prior distribution with 2 dimensions (A and B)

# invalid samplers are caught

    Code
      eval(points_call)
    Condition
      Error:
      ! `n_points` must be a whole number larger than or equal to 1, not the number 0.

---

    Code
      refresh_ernest_sampler(bad_sampler)
    Condition
      Error in `refresh_ernest_sampler()`:
      ! `n_points` must be a whole number, not `Inf`.

---

    Code
      eval(first_update_call)
    Condition
      Error:
      ! `first_update` must be a whole number larger than or equal to 0, not the number -1.

---

    Code
      refresh_ernest_sampler(bad_sampler)
    Condition
      Error in `refresh_ernest_sampler()`:
      ! `n_points` must be a whole number, not `Inf`.

---

    Code
      eval(update_interval_call)
    Condition
      Error:
      ! `update_interval` must be a whole number larger than or equal to 0, not the number -1.

---

    Code
      refresh_ernest_sampler(bad_sampler)
    Condition
      Error in `refresh_ernest_sampler()`:
      ! `n_points` must be a whole number, not `Inf`.

---

    Code
      eval(loglik_call)
    Condition
      Error:
      ! `log_lik_fn` must be an object with class ernest_likelihood, not an empty list.

---

    Code
      refresh_ernest_sampler(bad_sampler)
    Condition
      Error in `refresh_ernest_sampler()`:
      ! `log_lik_fn` must be an object with class ernest_likelihood, not the string "sum".

---

    Code
      eval(prior_call)
    Condition
      Error:
      ! `prior` must be an object with class ernest_prior, not an empty list.

---

    Code
      refresh_ernest_sampler(bad_sampler)
    Condition
      Error in `refresh_ernest_sampler()`:
      ! `log_lik_fn` must be an object with class ernest_likelihood, not the string "sum".

---

    Code
      eval(lrps_call)
    Condition
      Error:
      ! `lrps` must be an object with class ernest_lrps, not an empty list.

