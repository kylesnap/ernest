# calculate works when ndraws = 0

    Code
      calc
    Output
      An <ernest_calculate>: 0 draws x 3370 iterations

---

    Code
      smry
    Output
      
      -- Analytical Evidence Estimate from <ernest_estimate> -------------------------
      No. Draws: 0
      Log. Evidence: -4.504 (± 0.07082)

# calculate works when ndraws = 1

    Code
      calc
    Output
      An <ernest_calculate>: 1 draws x 3370 iterations

---

    Code
      smry
    Output
      
      -- Simulated Evidence Estimate from <ernest_estimate> --------------------------
      No. Draws: 1
      Log. Evidence: -4.545 (± Inf)

# calculate works when ndraws = 4000 (default)

    Code
      calc
    Output
      An <ernest_calculate>: 4000 draws x 3370 iterations

---

    Code
      smry
    Output
      
      -- Simulated Evidence Estimate from <ernest_estimate> --------------------------
      No. Draws: 4000
      Log. Evidence: -4.44 (± 0.07437)

