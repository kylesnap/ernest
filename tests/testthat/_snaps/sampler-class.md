# ernest_sampler initializes correctly

    Code
      sampler
    Output
      
      -- Nested sampling specification 
      No. Points: 500
      
      -- Sampling Method 
      * -- Random Walk in Unit Cube LRPS 
      * No. Dimensions: 2
      * No. Calls Since Update: 0
      * No. Accepted Since Update: 0
      * Current Step Size: 1

# invalid samplers are caught

    `n_points` must be a whole number larger than or equal to 1, not the number 0.

---

    `n_points` must be a whole number, not `Inf`.

---

    `first_update` must be a whole number larger than or equal to 0, not the number -1.

---

    `n_points` must be a whole number, not `Inf`.

---

    `update_interval` must be a whole number larger than or equal to 0, not the number -1.

---

    `n_points` must be a whole number, not `Inf`.

---

    `log_lik_fn` must be an object with class ernest_likelihood, not an empty list.

---

    `log_lik_fn` must be an object with class ernest_likelihood, not the string "sum".

---

    `prior` must be an object with class ernest_prior, not an empty list.

---

    `log_lik_fn` must be an object with class ernest_likelihood, not the string "sum".

---

    `lrps` must be an object with class ernest_lrps, not an empty list.

