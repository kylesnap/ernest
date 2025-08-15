# rwmh_cube returns correct class and structure

    Code
      obj
    Output
      
      -- Random Walk in Unit Cube LRPS 
      # Dimensions: Uninitialized
      # Calls Since Update: 0
      # Accepted Proposals Since Update: 0
      Current Step Size: 1

# new_rwmh_cube errors on invalid arguments

    `target_acceptance` must be at least 1/25.

---

    `target_acceptance` must be smaller than 1.

---

    `steps` must be a whole number larger than or equal to 2, not the number 1.

# propose.rwmh_cube proposes a single new point

    Code
      rwcube
    Output
      
      -- Random Walk in Unit Cube LRPS 
      # Dimensions: 2
      # Calls Since Update: 0
      # Accepted Proposals Since Update: 0
      Current Step Size: 1

# propose.rwmh_cube proposes multiple new points with criteria

    Code
      rwcube
    Output
      
      -- Random Walk in Unit Cube LRPS 
      # Dimensions: 2
      # Calls Since Update: 0
      # Accepted Proposals Since Update: 0
      Current Step Size: 1

# propose.rwmh_cube works with provided original points

    Code
      rwcube
    Output
      
      -- Random Walk in Unit Cube LRPS 
      # Dimensions: 2
      # Calls Since Update: 125
      # Accepted Proposals Since Update: 0
      Current Step Size: 1

# propose.rwmh_cube returns original log_lik when criteria is Inf

    Code
      rwcube
    Output
      
      -- Random Walk in Unit Cube LRPS 
      # Dimensions: 2
      # Calls Since Update: 250
      # Accepted Proposals Since Update: 0
      Current Step Size: 1

