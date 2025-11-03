// File: /Users/ksnap/Projects/ernest/src/nurs.cpp
// Created Date: Friday, October 31st 2025
// Author: Kyle Dewsnap
//
// Copyright (c) 2025 Kyle Dewsnap
// GNU General Public License v3.0 or later
// https://www.gnu.org/licenses/gpl-3.0-standalone.html
//
// Implements the no-underrun sampler for nested sampling.
#include "nurs.h"

#include <numeric>
using namespace ern;

void NURSSampler::Run(double criterion, int steps, double h, int M) {
  for (size_t step = 0; step < steps; step++) {
    TakeOneStep(criterion, h, M);
  }
}

void NURSSampler::TakeOneStep(double criterion, double h, int M) {
  // Shift the first orbit by `h`, and store result in `orbit` deque.
  HitAndRun(criterion, h);

  // Build the orbit lattice
  for (int k = 0; k < M; ++k) {
    NURSSampler::Direction dir = ExtendOrbit(h);
    if (dir == kOutOfBounds || CheckSubStop(orbit_ext_, criterion)) break;
    switch (dir) {
      case kForward:
        orbit_.insert(orbit_.end(), orbit_ext_.begin(), orbit_ext_.end());
        break;
      case kBackward:
        orbit_.insert(orbit_.begin(), orbit_ext_.begin(), orbit_ext_.end());
        break;
      default:
        cpp11::stop("Internal error: Uncaught switch case.");
        break;
    }
    if (CheckStop(orbit_.cbegin(), orbit_.cend(), criterion)) break;
  }

  // Choose point from the lattice
  ChooseNewPoint(criterion);
}

void NURSSampler::HitAndRun(double criterion, double h) {
  // Hit step
  UniformOnSphere(rho_);
  // Run step
  double s = runif(-h / 2.0, h / 2.0);
  proposed_.unit = cur_.unit + (s * rho_);
  proposed_.log_lik = unit_log_fn_(as_doubles(proposed_.unit));
  n_call_++;
  orbit_.clear();
  if (proposed_.log_lik >= criterion) {
    cur_ = std::move(proposed_);
    n_jump_++;
  }
  orbit_.emplace_back(cur_);
}

NURSSampler::Direction NURSSampler::ExtendOrbit(double h) {
  orbit_ext_.clear();
  NURSSampler::Direction dir =
      static_cast<int>(rbinom(1, 0.5)) == 1 ? kForward : kBackward;
  size_t n_lattice = orbit_.size();
  for (const StepResult& point : orbit_) {
    StepResult& next = orbit_ext_.emplace_back(point);
    next.unit += (dir * h * n_lattice) * rho_;
    if (ern::IsOutsideUnitCube(next.unit)) {
      return kOutOfBounds;
    }
    next.log_lik = unit_log_fn_(as_doubles(next.unit));
    n_call_++;
  };
  return dir;
}

bool NURSSampler::CheckSubStop(const std::deque<StepResult>& orbs,
                               double criterion) const {
  if (orbs.size() < 2) {
    return false;
  }
  using Iter = std::deque<StepResult>::const_iterator;
  struct Range {
    Iter left;
    Iter right;
  };
  std::deque<Range> orbit_ends;
  orbit_ends.emplace_back(Range{orbs.cbegin(), orbs.cend()});

  while (!orbit_ends.empty()) {
    Range cur = std::move(orbit_ends.front());
    orbit_ends.pop_front();
    size_t dist = std::distance(cur.left, cur.right);
    if (dist < 2) {
      continue;
    }
    if (CheckStop(cur.left, cur.right, criterion)) return true;
    Iter mid = cur.left;
    std::advance(mid, dist / 2);
    orbit_ends.emplace_back(Range{cur.left, mid});
    orbit_ends.emplace_back(Range{mid, cur.right});
  }
  return false;
}

bool NURSSampler::CheckStop(const std::deque<StepResult>::const_iterator left,
                            const std::deque<StepResult>::const_iterator right,
                            double criterion) const {
  return fmax2(left->log_lik, (right - 1)->log_lik) < criterion;
}

void NURSSampler::ChooseNewPoint(double criterion) {
  const size_t n_lattice = orbit_.size();
  size_t n_valid = orbit_.size();
  double max_p = R_NegInf;
  size_t max_idx = 0;
  for (int idx = 0; idx < n_lattice; idx++) {
    StepResult& point = orbit_[idx];
    double log_p;
    if (point.log_lik < criterion) {
      n_valid--;
      break;
    } else {
      log_p = point.log_lik;
    }
    double gumbel = -log(-log(unif_rand()));
    if (log_p + gumbel > max_p) {
      max_p = log_p + gumbel;
      max_idx = idx;
    }
  }
  if (!R_finite(max_p)) return;
  cur_ = std::move(orbit_[max_idx]);
  n_lattice_considered_ += n_lattice;
  n_lattice_valid_ += n_valid;
}
