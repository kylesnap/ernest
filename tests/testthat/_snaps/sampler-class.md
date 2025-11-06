# ernest_sampler initializes correctly

    Code
      sampler
    Output
      nested sampling specification <ernest_sampler>
      No. Points: 500
      
      -- Sampling Method 
      <rwmh_cube> lrps:
      No. Dimensions: 2
      No. Calls Since Update: 0
      No. Accepted Proposals: 0
      No. Steps: 25
      Target Acceptance: 0.5
      Step Size: 1.000
      

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

