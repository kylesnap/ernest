# adaptive_rwmh returns correct class and structure

    Code
      obj
    Output
      adaptive random walk LRPS <adaptive_rwmh/ernest_lrps>
      
      No. Dimensions: Uninitialized
      Current Step Size: 1.000
      Target Acceptance: 0.4

# new_adaptive_rwmh: errors on invalid arguments

    Code
      new_adaptive_rwmh(fn, 2L, target_acceptance = 0)
    Condition
      Error in `new_adaptive_rwmh()`:
      ! `target_acceptance` must be at least 1/25.

---

    Code
      new_adaptive_rwmh(fn, 2L, target_acceptance = 1.1)
    Condition
      Error in `new_adaptive_rwmh()`:
      ! `target_acceptance` must be a number smaller than or equal to 1, not the number 1.1.

---

    Code
      new_adaptive_rwmh(fn, 2L, steps = 1)
    Condition
      Error in `new_adaptive_rwmh()`:
      ! `steps` must be a whole number larger than or equal to 2, not the number 1.

---

    Code
      new_adaptive_rwmh(fn, 2L, target_acceptance = 0.01, steps = 25)
    Condition
      Error in `new_adaptive_rwmh()`:
      ! `target_acceptance` must be at least 1/25.

---

    Code
      new_adaptive_rwmh(fn, 2L, forgetfulness = -0.1)
    Condition
      Error in `new_adaptive_rwmh()`:
      ! `forgetfulness` must be a number between 0 and 1, not the number -0.1.

---

    Code
      new_adaptive_rwmh(fn, 2L, forgetfulness = 1.1)
    Condition
      Error in `new_adaptive_rwmh()`:
      ! `forgetfulness` must be a number between 0 and 1, not the number 1.1.

---

    Code
      new_adaptive_rwmh(fn, 2L, min_epsilon = -1)
    Condition
      Error in `new_adaptive_rwmh()`:
      ! `min_epsilon` must be a number larger than or equal to 0, not the number -1.

---

    Code
      new_adaptive_rwmh(fn, 2L, strength = -1)
    Condition
      Error in `new_adaptive_rwmh()`:
      ! `strength` must be a number larger than or equal to 0, not the number -1.

# propose.adaptive_rwmh: proposes a single new point

    Code
      adaptive_rw
    Output
      adaptive random walk LRPS <adaptive_rwmh/ernest_lrps>
      
      No. Dimensions: 2
      Current Step Size: 1.000
      Target Acceptance: 0.4

# update_lrps.adaptive_rwmh: targets the acceptance ratio

    Code
      more_strict
    Output
      adaptive random walk LRPS <adaptive_rwmh/ernest_lrps>
      
      No. Dimensions: 2
      Current Step Size: 0.3341
      Target Acceptance: 0.1

---

    Code
      less_strict
    Output
      adaptive random walk LRPS <adaptive_rwmh/ernest_lrps>
      
      No. Dimensions: 2
      Current Step Size: 0.1000
      Target Acceptance: 0.9

