# Informative error when prior or log. lik. fails completely.

    Can't create live points.
    Caused by error in `private$log_lik_fn()`:
    ! Bad Likelihood Job!

---

    Can't create live points.
    Caused by error in `private$prior_fn()`:
    ! Bad prior job!

# check_live validates live points correctly

    `unit` must be of type 'matrix', not 'double'.

---

    `unit` must have exactly 5 rows, but has 3 rows.

---

    `unit` contains missing values (row 1, col 1).

---

    `unit` element 7 is not <= 1.

---

    `log_lik` must have length 5, but has length 3.

---

    `log_lik` contains missing values (element 5).

---

    Log likelihoods of the live points must not be a plateau.
    ! Log likelihood of all 5 points = -10.

---

    Potential likelihood plateau; proceed with caution.
    ! 3 unique likelihoods across 5 live points.

