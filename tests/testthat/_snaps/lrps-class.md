# ernest_lrps class initializes correctly

    Code
      obj
    Output
      
      -- Abstract LRPS 
      # Dimensions: 2
      # Calls Since Update: 0

# new_ernest_lrps throws errors for invalid arguments

    `unit_log_fn` must be a function or `NULL`, not the number 1.

---

    `n_dim` must be a whole number larger than or equal to 1 or `NULL`, not the number 0.

---

    `cache` must be an environment or `NULL`, not the number 1.

---

    `getOption('ernest.max_loop')` must be a whole number larger than or equal to 1, not the number 0.

---

    `getOption('ernest.max_loop')` must be a whole number, not `Inf`.

# propose.ernest_lrps errors if original is provided

    `x` must not be the abstract class <ernest_lrps>.

