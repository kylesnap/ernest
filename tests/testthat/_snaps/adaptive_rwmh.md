# adaptive_rwmh can be called by user

    Code
      adaptive_rwmh(steps = 1)
    Condition
      Error in `new_adaptive_rwmh()`:
      ! `steps` must be a whole number larger than or equal to 2, not the number 1.

---

    Code
      adaptive_rwmh(target_acceptance = 1.1)
    Condition
      Error in `new_adaptive_rwmh()`:
      ! `target_acceptance` must be a number smaller than or equal to 1, not the number 1.1.

---

    Code
      adaptive_rwmh(target_acceptance = 0.02)
    Condition
      Error in `new_adaptive_rwmh()`:
      ! `target_acceptance` must be at least 1/25.

---

    Code
      adaptive_rwmh(min_epsilon = -0.1)
    Condition
      Error in `new_adaptive_rwmh()`:
      ! `min_epsilon` must be a number larger than or equal to 0, not the number -0.1.

---

    Code
      adaptive_rwmh(strength = -5)
    Condition
      Error in `new_adaptive_rwmh()`:
      ! `strength` must be a number larger than or equal to 0, not the number -5.

---

    Code
      adaptive_rwmh(forgetfulness = -0.1)
    Condition
      Error in `new_adaptive_rwmh()`:
      ! `forgetfulness` must be a number between 0 and 1, not the number -0.1.

---

    Code
      adaptive_rwmh(forgetfulness = 1.1)
    Condition
      Error in `new_adaptive_rwmh()`:
      ! `forgetfulness` must be a number between 0 and 1, not the number 1.1.

---

    Code
      default
    Message
      <adaptive_rwmh> lrps:
      No. Dimensions: Undefined
      No. Calls Since Update: 0
      No. Accepted Proposals: 0
      No. Steps: 25
      Target Acceptance: 0.4
      Step Size: 1.000
      Min. Step Size: 0.1
      

