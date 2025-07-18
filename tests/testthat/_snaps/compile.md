# Informative error when prior or log. lik. fails completely.

    Can't create live points.
    Caused by error in `private$log_lik_fn()`:
    ! Bad Likelihood Job!

---

    Can't create live points.
    Caused by error in `private$prior_fn()`:
    ! Bad prior job!

# check_live validates live points correctly

    `unit` must be a numeric matrix, not a double vector.

---

    Live points matrix must have dim. 5 x 4.
    x Points are currently 3 x 5.

---

    Live points matrix must only contain finite values.

---

    Live points matrix must only contain values between 0 and 1.

---

    `log_lik` must be a double vector of length 5
    ! You provided a double vector.

---

    missing value where TRUE/FALSE needed

---

    Log likelihoods of the live points must not be a plateau.
    x Log likelihood of all 5 points = -10.

---

    Potential likelihood plateau; proceed with caution.
    ! 3 unique likelihoods across 5 live points.

