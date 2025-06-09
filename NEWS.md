# NEWS

# [2025-06-10] margot.sim 0.1.0

## New

* Initial release of margot.sim package
* **Shadowing framework** for applying observational distortions to simulated data
  - Classical measurement error for continuous variables
  - Misclassification bias for binary variables (with sensitivity/specificity)
  - Differential measurement error
  - Dichotomization at thresholds
  - Correlated errors across variables
  - Item-level missingness (MCAR, MAR, MNAR mechanisms)
  - Positivity violations (filtering impossible exposures)
  - Selection bias (baseline or post-treatment)
* **Monte Carlo framework** for evaluating statistical estimators
  - Parallel processing support
  - Automatic performance metrics (bias, variance, MSE, coverage)
  - Comparison functions for multiple methods
* **Distribution specifications** for flexible data generation
  - Support for parametric and custom distributions
  - Distribution sets for multivariate outcomes
* Integration with `margot_simulate()` function via `shadows` parameter
* Comprehensive unit tests using testthat
* Package website using pkgdown
* Examples demonstrating complete workflows

## Improved

* Replaced external dependency `margot_process_longitudinal_data_wider()` with internal `margot_process_longitudinal()` function
* Split large `margot-simulate-functions.R` file into logical components:
  - `margot-simulate-core.R` - Main simulation function
  - `margot-simulate-censoring.R` - Censoring functions
  - `margot-simulate-causal.R` - Causal inference wrapper
  - `margot-simulate-utils.R` - Utility functions
  - `margot-scm-docs.R` - SCM documentation
  - `margot-simulate-examples.R` - Example functions
* Changed terminology from "masks/masking" to "shadows/shadowing" for better metaphor
* Added startup message showing package version (can be suppressed with `options(margot.sim.quiet = TRUE)`)