# ernest_lrps class initializes correctly

    Code
      lrps$as_string()
    Output
      [1] "Abstract LRPS Sampler"

# Uniform subclass initalizes

    Code
      uniform$as_string()
    Output
      [1] "Uniform Sampling in Unit Cube" "No. Iter: 0"                  
      [3] "No. Call: 0"                  

# RWMH can be initialized and catches bad parameters

    Target acceptance must be at least 1/25.

---

    `target_acceptance` must be a number smaller than or equal to 1, not the number 1.1.

---

    `steps` must be a whole number larger than or equal to 2, not the number 1.

---

    Code
      new$as_string()
    Output
      [1] "Random-Walk in Unit Cube with Adaptive Step Size"
      [2] "No. Iter: 0"                                     
      [3] "No. Call: 0"                                     
      [4] "No. Steps: 25"                                   
      [5] "Epsilon: 1"                                      

