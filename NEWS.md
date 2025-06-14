# NEWS

# margot.sim 0.1.3 (2025-01-15)

## Major Architecture Improvements

### S3 Object System
* Complete S3 class implementation for shadows and scenarios
  - `new_shadow()`: Low-level constructor with automatic validation
  - `new_scenario()`: Scenario constructor with S3 dispatch
  - `validate_shadow()`, `validate_scenario()`: S3 generic validation
  - `is_shadow()`, `is_scenario()`: Type checking functions
  - `as_shadow()`, `as_scenario()`: Coercion methods
  - Print, summary, and as.data.frame methods for all classes
  - `shadow_list` class for managing collections of shadows

### Shadow Dependency Management
* Automatic dependency resolution and reordering
  - `get_shadow_dependencies()`: Define dependency relationships
  - `check_shadow_ordering()`: Validate correct application order
  - `reorder_shadows()`: Automatic topological sorting
  - `update_shadow_params()`: Adjust parameters based on upstream effects
  - `apply_shadows_with_dependencies()`: Apply with automatic reordering
  - `visualize_shadow_dependencies()`: Display dependency graph

### Multi-Treatment Support
* Enhanced shift interventions for complex treatment interactions
  - `create_multi_treatment_shift()`: Handle multiple concurrent treatments
  - `create_interaction_shift()`: Define treatment interactions
  - `create_interaction_patterns()`: Complex interaction specifications
  - `create_lmtp_wrapper()`: LMTP-compatible intervention wrapper
  - Updated `margot_simulate_causal()` to accept treatment lists

### Memory Management 
* Efficient handling of large-scale simulations
  - Memory limit enforcement in `margot_monte_carlo()`
  - Checkpoint saving for resumable simulations
  - `create_memory_monitor()`: Track memory usage
  - `create_mc_streamer()`: Stream results to disk
  - `create_data_summarizer()`: On-the-fly summarization
  - `resume_monte_carlo()`: Resume interrupted simulations

### Positivity Diagnostics
* Comprehensive tools for detecting violations
  - `margot_check_positivity()`: Check empirical and model-based positivity
  - `create_positivity_shadow_from_diagnostic()`: Create shadows from diagnostics
  - Print and plot methods for diagnostics
  - Integration with scenario framework

### RNG and Reproducibility
* Proper seed discipline for parallel simulations
  - `create_seed_sequence()`: Generate reproducible seed sequences
  - `create_rng_streams()`: Create independent RNG streams
  - `setup_parallel_rng()`: Configure parallel backends
  - `validate_parallel_rng()`: Verify RNG independence
  - `diagnose_rng_streams()`: Debug RNG issues

## CRAN Compliance

* **Full R CMD check compliance**: 0 errors, 0 warnings, 0 notes
* Fixed all documentation issues including `%||%` operator
* Added missing parameter documentation
* Replaced non-ASCII characters
* Fixed example errors
* Added igraph to Suggests
* Cleaned up namespace imports

## Previous Features (v0.1.3 development - 2025-06-12)

## Major New Features

### Scenario Framework for Systematic Sensitivity Analysis
* **Core Infrastructure**: New framework for bundling shadows into documented research scenarios
  - `create_scenario()`: Create scenarios with shadows, descriptions, justifications, and references
  - `apply_scenario()`: Apply all shadows in a scenario while preserving dual data architecture
  - `compare_scenarios()`: Compare causal effects across multiple scenarios
  - `sensitivity_analysis()`: Monte Carlo evaluation across scenarios
* **Pre-built Scenario Library**: Common research contexts with realistic observational challenges
  - `scenario_oracle()`: Perfect measurement benchmark
  - `scenario_rct_typical()`: Well-conducted RCT with minimal biases
  - `scenario_rct_pragmatic()`: Real-world trial conditions
  - `scenario_ehr_typical()`: Electronic health records with coding errors and missingness
  - `scenario_survey_typical()`: Survey data with self-report biases
  - `scenario_registry_typical()`: Administrative data with truncation and coarsening
  - `scenario_pessimistic()`: Worst-case plausible scenario
  - `scenario_collection()`: Get standard scenario set for sensitivity analysis
* **Simple Scenario Library**: Alternative implementations that work with standard margot_simulate() output
  - `scenario_rct_simple()`, `scenario_observational_simple()`, `scenario_collection_simple()`

### Extended Shadow Types
* **Truncation Shadows** (`create_truncation_shadow()`): Model measurement limits
  - "simple" type: Values beyond thresholds become NA
  - "boundary" type: Values pile up at boundaries (e.g., "200k+" for income)
  - Handles laboratory equipment limits, survey bounds, privacy top-coding
* **Coarsening Shadows** (`create_coarsening_shadow()`): Convert continuous to categorical data
  - Multiple handling strategies: "midpoint", "lower", "upper", "random", "heaping"
  - Heaping algorithm models realistic digit preference (e.g., ages ending in 0 or 5)
  - Supports custom breaks or automatic binning
* **Mode Effects Shadows** (`create_mode_effects_shadow()`): Measurement varies by collection method
  - Define mode-specific transformation functions
  - Model phone vs web vs in-person survey differences
  - Preserves relationships while adding systematic mode effects

## New Documentation

### Vignettes
* **"Scenario-Based Sensitivity Analysis"** (`vignette("scenario-sensitivity")`): 
  - Comprehensive guide to using scenarios for systematic sensitivity analysis
  - Examples of creating custom scenarios for specific research contexts
  - Threshold analysis to find when conclusions change
* **"Truncation and Coarsening: When Data Loses Precision"** (`vignette("truncation-coarsening")`):
  - Detailed examples of truncation vs censoring
  - Impact of coarsening on causal estimates
  - Realistic heaping patterns in self-reported data

## Improvements

### Enhanced Architecture
* All new shadows integrate with existing `apply_shadow()` S3 dispatch system
* Comprehensive input validation and error messages
* Temporal order validation extended to new shadow types
* Print methods for scenario objects and comparison results
* Plot methods for scenario comparisons using ggplot2

### Testing and Quality
* Added 77 new tests for scenario framework
* Added 49 tests for extended shadow types
* All new functions have comprehensive edge case coverage
* Updated _pkgdown.yml with new function categories

### Examples
* Added examples to all major scenario functions
* Created realistic COVID vaccine effectiveness scenario example
* Demonstrated systematic sensitivity analysis workflows

## Technical Details

### S3 Methods
* `apply_shadow.truncation_shadow()`: Handles both simple and boundary truncation
* `apply_shadow.coarsening_shadow()`: Implements all coarsening strategies
* `apply_shadow.mode_effects_shadow()`: Applies mode-specific transformations
* `print.margot_scenario()`: Formatted output for scenarios
* `print.margot_scenario_result()`: Results from scenario application
* `print.margot_scenario_comparison()`: Comparison table with bias calculations
* `plot.margot_scenario_comparison()`: Visual comparison of effects
* `summary.margot_sensitivity_analysis()`: Summary of Monte Carlo sensitivity results

### Internal Functions
* Enhanced `apply_shadows_with_truth()` to work with scenario framework
* Added bracket parsing for coarsening shadows
* Implemented realistic heaping algorithm with NA handling

## Bug Fixes

* Fixed differential error function signatures to match expected interface
* Improved handling of interval parsing in coarsening shadows
* Fixed NA handling in heaping algorithm
* Removed non-implemented selection shadow from pessimistic scenario

# margot.sim 0.1.2 (2025-01-11)

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


# margot.sim 0.1.0 (2025-06-10)

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
