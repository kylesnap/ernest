# new_ernest_lrps: fails informatively

    Code
      new_ernest_lrps(unit_log_fn = 1)
    Condition
      Error:
      ! `unit_log_fn` must be a function or `NULL`, not the number 1.

---

    Code
      new_ernest_lrps(fn, n_dim = 0)
    Condition
      Error:
      ! `n_dim` must be a whole number larger than or equal to 1 or `NULL`, not the number 0.

---

    Code
      new_ernest_lrps(fn, n_dim = 2, cache = 1)
    Condition
      Error:
      ! `cache` must be an environment or `NULL`, not the number 1.

---

    Code
      new_ernest_lrps(fn, n_dim = 2)
    Condition
      Error:
      ! `getOption('ernest.max_loop')` must be a whole number larger than or equal to 1, not the number 0.

---

    Code
      new_ernest_lrps(fn, n_dim = 2)
    Condition
      Error:
      ! `getOption('ernest.max_loop')` must be a whole number, not `Inf`.

# new_ernest_lrps: initializes correctly

    Code
      obj
    Output
      ! An abstract LRPS sampler <ernest_lrps>

# propose.ernest_lrps: errors if original is provided

    Code
      propose(lrps, original, -1)
    Condition
      Error in `propose()`:
      ! `x` must not be the abstract class <ernest_lrps>.

