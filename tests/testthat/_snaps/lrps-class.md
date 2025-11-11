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

# propose.ernest_lrps can be called

    Code
      propose.ernest_lrps(lrps, c(0.5, 0.5), -1)
    Condition
      Error in `propose.ernest_lrps()`:
      ! `x` must not be the abstract class <ernest_lrps>.

