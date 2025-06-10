# NEWS

# margot.sim 0.1.2 (2025-06-11)

## New features

### Shadow Bias Comparison Framework
* Added comprehensive framework for comparing causal effects before and after applying observational shadows
  - `compute_causal_effects()`: Standardized function for computing ATE, ATT, and ATU from data
  - `compute_effects_from_sim()`: Wrapper for margot simulation data with proper time indexing
  - `compare_shadow_bias()`: Calculates absolute and relative bias for each estimand
  - `apply_shadows_with_truth()`: Preserves complete data alongside shadowed observations
  - `compare_shadow_effects()`: Full workflow for comparing true vs observed effects

### Transport Weights and Generalizability
* Added support for transportability analyses to generalize from samples to target populations
  - `simulate_ate_data_with_weights()`: Direct replacement for legacy function with identical interface
  - `margot_transport_analysis()`: Enhanced version integrating shadows with transport weights
  - Shows how observational distortions affect transported estimates differently in source vs target populations
  - Handles effect modification that differs between sample and population

### Enhanced Examples and Documentation
* `example_shadow_bias_analysis()`: Complete workflow demonstrating shadow bias evaluation
* `example_shadow_scenarios()`: Compares bias across multiple shadow combinations
* `example_weighted_shadow_analysis()`: Shows transport weights interacting with shadows

### New Vignettes
* **"Shift Interventions with Sampling Weights"**: Demonstrates progressive interventions with population weighting
* **"Censoring and Effect Modification"**: Explores differential censoring by severity with IPCW correction
* **"Heterogeneous Treatment Effects and Simpson's Paradox"**: Shows how aggregation can mask subgroup benefits
* **"Advanced Shift Interventions"**: Covers threshold, bounded, responsive, and combined shift patterns
* **"Misclassification Bias"**: Compares plain vs differential misclassification with model misspecification
* **"Practical Workflow"**: Complete analysis pipeline from design to reporting
* **"Transport Weights and Shadows"**: Generalizability with observational distortions

## Improvements

### Dual Data Architecture
* Modified `margot_simulate_causal()` to support dual data structure (`data_true` and `data_observed`)
* Shadows now preserve original values systematically for ground truth comparisons
* Enhanced print methods for shadow results and effect comparisons

### Sampling Weights Integration
* Sampling weights are fully integrated throughout the effect computation pipeline
* Weights preserved through shadow application process
* Support for comparing weighted (target population) vs unweighted (source population) effects

### Documentation
* Added comprehensive design documentation in CLAUDE.md
* Updated README with transport weights example
* Enhanced _pkgdown.yml structure with new function categories

## Bug fixes

* Fixed wave indexing to properly handle margot.sim structure (wave 0: baseline, wave 1: treatment, wave 2: outcome)
* Improved handling of non-binary treatments in effect computation (with appropriate warnings)
* Fixed recursive print method issue in shadow_bias_comparison

# margot.sim 0.1.1 (2025-06-10)

## Bug fixes

* Fixed vignette errors in `applying-shadows.Rmd` and `monte-carlo-simple.Rmd`
* Corrected function signatures for `create_positivity_shadow()` and `analyse_shadow_effects()`
* Fixed pkgdown accessibility warning by adding aria-label to GitHub icon

## New features

* Added temporal order validation for shadow dependencies
  - Shadows can only depend on variables from the same time point or earlier
  - Prevents future information from affecting past measurements
  - Added internal functions `get_time_index()` and `validate_temporal_order()`
  - Comprehensive tests ensure temporal causality is maintained

## Improvements

* Added GitHub Actions workflows for R CMD check and code coverage
* Added badges for lifecycle, license, R CMD check, code coverage, and GitHub stars
* Cleaned up internal function documentation


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
* Comprehensive unit tests using `testthat`
* Package website using `pkgdown`
* Examples demonstrating complete workflows

## Improved

* Replaced external dependency `margot::margot_process_longitudinal_data_wider()` with internal `margot.sim::margot_process_longitudinal()` function
* Split large `margot-simulate-functions.R` file into logical components:
  - `margot-simulate-core.R` - Main simulation function
  - `margot-simulate-censoring.R` - Censoring functions
  - `margot-simulate-causal.R` - Causal inference wrapper
  - `margot-simulate-utils.R` - Utility functions
  - `margot-scm-docs.R` - SCM documentation
  - `margot-simulate-examples.R` - Example functions
* Changed terminology from "masks/masking" to "shadows/shadowing" for better metaphor
* Added startup message showing package version (can be suppressed with `options(margot.sim.quiet = TRUE)`)
