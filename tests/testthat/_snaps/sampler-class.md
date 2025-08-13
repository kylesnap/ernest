# ernest_sampler initializes correctly

    Code
      sampler
    Output
      An <ernest_sampler>: 500 points x 2 params. * Sampler: Random-Walk in Unit Cube with Adaptive Step Size, No. Iter: 0, No.   Call: 0, No. Steps: 25, and Epsilon: 1. 

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

    `sampling` must be an object with class ernest_sampling, not an empty list.

---

    `log_lik_fn` must be an object with class ernest_likelihood, not the string "sum".

