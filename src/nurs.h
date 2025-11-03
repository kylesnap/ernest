// File: /Users/ksnap/Projects/ernest/src/nurs.h
// Created Date: Friday, October 31st 2025
// Author: Kyle Dewsnap
//
// Copyright (c) 2025 Kyle Dewsnap
// GNU General Public License v3.0 or later
// https://www.gnu.org/licenses/gpl-3.0-standalone.html
//
// Implements the no-underrun sampler for nested sampling.
#pragma once

#include <deque>
#include "random_generator.h"

namespace ern {

// Type alias for a step from the NURS sampler
struct StepResult {
  Vector unit;
  double log_lik;
};

class NURSSampler {
 public:
  inline NURSSampler(cpp11::function& unit_log_fn, cpp11::doubles& original)
      : unit_log_fn_(unit_log_fn),
        cur_({as_Matrix(original), R_NegInf}),
        rho_(original.size()) {
    cur_.log_lik = unit_log_fn_(original);
    proposed_ = StepResult{Vector::Zero(original.size()), R_NegInf};
  };

  void Run(double criterion, int steps, double h, int M);

  // Converts the sampler state to an R list representation.
  inline cpp11::list as_list() const {
    using namespace cpp11::literals;
    return cpp11::writable::list({"unit"_nm = as_row_doubles(cur_.unit),
                                  "log_lik"_nm = cur_.log_lik, "n_call"_nm = n_call_,
                                  "n_jump"_nm = n_jump_,
                                  "n_considered"_nm = n_lattice_considered_,
                                  "n_candidates"_nm = n_lattice_valid_});
  }

 private:
  cpp11::function& unit_log_fn_;      // Log-likelihood function
  StepResult cur_, proposed_;         // Current and proposed unit points + log-liks
  std::deque<StepResult> orbit_;      // Orbit of points around origin
  std::deque<StepResult> orbit_ext_;  // Proposed doubling of orbit
  Vector rho_;                        // Direction vector for slicing
  size_t n_call_ = 0;                 // Number of calls to log_fn
  size_t n_jump_ = 0;                 // Number of successful jumps in MH step
  size_t n_lattice_considered_ = 0;   // Number of lattice points built
  size_t n_lattice_valid_ = 0;        // Number of valid lattice points (> criterion)

  // Enum for the sampling direction
  enum Direction {
    kBackward = -1,   // Lattice extended leftwards of current orbit
    kForward = 1,     // Lattice extended rightwards of current orbit
    kOutOfBounds = 0  // Lattice falls outside the unit hypercube
  };

  // Perform one hit-and-run step
  void TakeOneStep(double criterion, double h, int M);

  // Shift the point at cur_ by `h`, and accept the shift if the new point has greater
  // log-lik. than `criterion`.
  void HitAndRun(double criterion, double h);

  // Build the orbit lattice: Double the points at iteration `i` so that they're
  // spaced `h` apart and place in a cleared orbit_ext_. Returns the direction of the
  // lattice extension, or `0` if the extension points falls OOB.
  Direction ExtendOrbit(double h);

  // Recursively check an orbit to see whether itself or its suborbits satisfy the
  // stoppping condition.
  bool CheckSubStop(const std::deque<StepResult>& orbits, double criterion) const;

  // Check whether an orbit satisfy this stopping condition:
  // max(orbits[left], orbits[right-1]) > criterion.
  bool CheckStop(const std::deque<StepResult>::const_iterator left,
                 const std::deque<StepResult>::const_iterator right,
                 double criterion) const;

  // Choose a point in the orbit with the categorical distribution.
  void ChooseNewPoint(double criterion);
};

}  // namespace ern