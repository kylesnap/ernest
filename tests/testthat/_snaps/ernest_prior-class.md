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

