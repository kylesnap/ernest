# create_normal_prior error handling

    `sd` of a normal distribution must be non-negative.

# create_normal_prior and create_uniform_prior: print methods

    Code
      create_normal_prior(1, mean = 0, sd = 1)
    Output
      Prior distribution <normal_prior/ernest_prior>
      
      Names: "Normal"

---

    Code
      create_uniform_prior(1, lower = 0, upper = 1)
    Output
      Prior distribution <uniform_prior/ernest_prior>
      
      Names: "Uniform"
      Bounds:
      > Lower: 0
      > Upper: 1

