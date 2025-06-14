- remember to update _pkgdown.yml after adding or deleting exported functions
- after each new function or revision, unit tests an update _pkdown.yml
- run pkgdown::build_articles() after each vingette is completed
- use nz english
- write unit tests for each new function
- .Rbuildignore all planning documents

# margot.sim Package Context

## Code Coverage Status (2025-01-11)
- Overall coverage: 54.24%
- Files with 0% coverage are mainly example/documentation files (expected)
- Core functionality has good coverage (56-96%)
- Priority areas for test coverage improvement:
  - shift-interventions.R (0%) - new feature needing tests
  - scenario-library-simple.R (42%) - could use more comprehensive tests
  - monte-carlo.R (43%) - critical functionality needs better coverage
  - distributions.R (56%) - many edge cases not tested

## Overview
margot.sim is an R package for simulating longitudinal data with realistic observational challenges and evaluating causal inference methods through Monte Carlo simulation. It implements a "shadowing" framework inspired by Plato's Allegory of the Cave, where observed data are distorted versions ("shadows") of true causal processes.