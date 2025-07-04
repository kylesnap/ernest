# create_prior errors if n_dim < 1

    `n_dim` must be a whole number larger than or equal to 1, not the number 0.

# create_prior errors if prior function output length is wrong

    <ernest_prior> failed validation checks.
    x Failure at this input: 0.935637236107141 and 0.229094626381993
    x Failed with this output: 0.935637236107141, 0.229094626381993, and 4.08406452339504
    Caused by error in `validate_prior()`:
    ! `fn` must always return a double vector of length 2.

# create_prior errors if prior returns non-finite values

    <ernest_prior> failed validation checks.
    x Failure at this input: 0.647276502801105 and 0.853123272769153
    x Failed with this output: NaN and NaN
    Caused by error in `validate_prior()`:
    ! `fn` must always return finite values for every vector in [0, 1).

# create_prior errors if lower > min(prior)

    <ernest_prior> failed validation checks.
    x Failure at this input: 0.871139647904783 and 0.435637243324891
    x Failed with this output: 0 and 0
    Caused by error in `validate_prior()`:
    ! `fn` must return values greater than or equal to the lower bound.
    i Lower bounds: 1 and 1

# create_prior errors if lower >= upper

    <ernest_prior> failed validation checks.
    x Failure at this input: 0.503341236151755 and 0.361691718222573
    x Failed with this output: -1 and -1
    Caused by error in `validate_prior()`:
    ! `fn` must return values greater than or equal to the lower bound.
    i Lower bounds: 1 and 1

# Print methods

    Code
      create_normal_prior(1, mean = 0, sd = 1)
    Output
      <ernest_prior/normal_prior> with 1 dim.
      Vars.: N(0, 1)

---

    Code
      create_t_prior(1, df = 1)
    Output
      <ernest_prior/student_t_prior> with 1 dim.
      Vars.: T(1, 0, 1)

---

    Code
      create_cauchy_prior(1, location = 0, scale = 1)
    Output
      <ernest_prior/cauchy_prior> with 1 dim.
      Vars.: Cauchy(0, 1)

---

    Code
      create_uniform_prior(1, lower = 0, upper = 1)
    Output
      <ernest_prior/uniform_prior> with 1 dim.
      Vars.: Uniform(0, 1)

---

    Code
      create_prior(function(x) x, n_dim = 1, varnames = "x")
    Output
      <ernest_prior> with 1 dim.
      Vars.: x

