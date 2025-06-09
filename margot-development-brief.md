# margot Extensions Development Brief

## Project Overview

We are extending the `margot` R package, which simulates longitudinal data using Structural Causal Models (SCMs). The goal is to add sophisticated capabilities for:

1. Applying various "shadows" (censoring, measurement error, selection bias) to simulated data
2. Running Monte Carlo simulations to evaluate statistical estimators
3. Supporting flexible, non-normal distributions
4. Creating a unified framework for causal inference method evaluation

## Current State

### Existing margot Functions (Already Implemented)
- `margot_simulate()` - Main simulation function
- `margot_simulate_causal()` - Wrapper for causal inference with interventions
- `apply_censoring_post_hoc()` - Applies censoring after data generation
- Complete SCM with semi-Markovian structure

### Critical Dependencies & Technical Debt

1. **External Function Dependency**: `margot_simulate()` currently depends on `margot_process_longitudinal_data_wider()` from another package. We need to:
   - Build a replacement function from scratch
   - Preserve essential features like temporal ordering of variables (left to right in dataframe)
   - The existing function is available in the `R/to-develop` folder for reference

2. **Missing Features from Previous Versions**: Two important parameters need to be reimplemented as shadows:
   - **Item-level missingness** (`item_missing_rate`): Apply missingness to individual variables rather than whole-scale censoring
   - **Positivity violations** (`positivity` parameter): Simulate scenarios where certain exposure levels are impossible for some individuals

### New Extension Files Created

1. **margot_shadows.R** - Unified shadowing framework
2. **margot_monte_carlo.R** - Monte Carlo simulation tools
3. **margot_distributions.R** - Flexible distribution specifications
4. **margot_examples_integrated.R** - Complete workflow examples

## Architecture & Design Principles

### Core Philosophy
```
Truth Generation → Intervention → Post-hoc Shadowing → Estimation → Evaluation
```

The key insight: Generate complete, true data first, then apply distortions. This allows direct calculation of both true effects and bias.

### Causal Inference Foundations

**Positivity and Counterfactual Contrasts**: True causal effects require comparing outcomes under different exposures for the *same* population. Every individual must be able to (counter to fact) receive every exposure level. When positivity is violated:
- Some individuals cannot receive certain exposures
- Causal effects become ill-defined for those individuals
- Estimators may exhibit extreme weights or fail

**Item-Level vs Unit-Level Missingness**: Traditional censoring removes entire units from analysis. Item-level missingness is more nuanced:
- Individual variables can be missing while others are observed
- Creates partial information scenarios
- More realistic for many applications (lab values, survey items)

### Technical Architecture

```r
# 1. Generate true data under intervention
true_data <- margot_simulate_causal(
  interventions = list(treat = ..., control = ...)
)

# 2. Apply shadows (new functionality)
shadowed_data <- apply_shadows(
  true_data,
  shadows = list(
    measurement_error_shadow,
    selection_bias_shadow
  )
)

# 3. Evaluate estimators (new functionality)
mc_results <- margot_monte_carlo(
  estimator_fn = function(data) { ... },
  shadows = shadows,
  n_reps = 500
)
```

## Implementation Tasks

### Task 0: Replace External Dependencies (Priority)

Build internal replacement for `margot_process_longitudinal_data_wider()`:

```r
# margot_process_data.R
margot_process_longitudinal <- function(
    df_wide,
    exposure_vars = NULL,
    outcome_vars = NULL,
    preserve_temporal_order = TRUE
) {
  # Key requirements:
  # 1. Maintain temporal ordering (t0_var, t1_var, ..., tn_var)
  # 2. Create censoring indicators
  # 3. Handle missing data appropriately
  
  # Implementation based on existing function in R/to-develop/
}
```

### Task 1: Integrate Shadowing with margot_simulate

Currently, `apply_censoring_post_hoc()` is separate. We need to:

```r
# Modify margot_simulate to accept shadows parameter
margot_simulate <- function(
    ...,
    shadows = NULL,  # NEW
    apply_shadows_post_hoc = TRUE  # NEW
) {
  # Existing generation code...
  
  # At the end, if shadows provided:
  if (!is.null(shadows) && apply_shadows_post_hoc) {
    df <- apply_shadows(df, shadows)
  }
  
  df
}
```

### Task 2: Enhance Measurement Error Shadows

Add more sophisticated measurement error types:

```r
# Berkson error (true X affects measured X)
apply_berkson_error <- function(data, params) {
  # Implementation needed
}

# Correlated errors across time
apply_temporal_correlated_error <- function(data, params) {
  # AR(1) structure for measurement errors
}

# Heteroscedastic errors
apply_heteroscedastic_error <- function(data, params) {
  # Error variance depends on true value
}
```

### Task 3: Implement Missing Features as Shadows

Add item-level missingness and positivity violations as shadow types:

```r
# Item-level missingness shadow
create_item_missingness_shadow <- function(
    variables,
    missing_rate,
    missing_mechanism = c("MCAR", "MAR", "MNAR"),
    dependent_vars = NULL
) {
  create_shadow(
    type = "item_missingness",
    params = list(
      variables = variables,
      rate = missing_rate,
      mechanism = missing_mechanism,
      dependent_vars = dependent_vars
    )
  )
}

# Positivity violation shadow (filters impossible exposures)
create_positivity_shadow <- function(
    exposure_var,
    filter_fn,  # Function that returns TRUE for valid exposures
    name = "positivity_filter"
) {
  # This shadow removes individuals who cannot receive certain exposures
  # Critical for understanding bias when positivity is violated
  create_shadow(
    type = "positivity",
    params = list(
      exposure_var = exposure_var,
      filter_fn = filter_fn
    )
  )
}

# Implementation example
apply_shadow.positivity_shadow <- function(data, shadow, ...) {
  # Filter rows where exposure is possible
  valid_rows <- shadow$params$filter_fn(data)
  data[valid_rows, ]
}
```

### Task 4: Complete Distribution Integration

The current `margot_simulate_flex()` is a placeholder. Need to:

1. Modify the core generation loops in `margot_simulate()` to use custom distributions
2. Handle correlation structures with non-normal distributions
3. Ensure proper link functions for binary/count outcomes

```r
# Inside margot_simulate generation loop
if (!is.null(distributions)) {
  # Generate L_t with custom distribution
  l_dist <- distributions$confounder
  mu_l <- ... # existing linear predictor
  df[[paste0("t", t, "_l")]] <- generate_from_dist(l_dist, n, mu_l)
} else {
  # Existing normal generation
  df[[paste0("t", t, "_l")]] <- mu_l + rnorm(n)
}
```

### Task 4: Complete Distribution Integration

The current `margot_simulate_flex()` is a placeholder. Need to:

1. Modify the core generation loops in `margot_simulate()` to use custom distributions
2. Handle correlation structures with non-normal distributions
3. Ensure proper link functions for binary/count outcomes

```r
# Inside margot_simulate generation loop
if (!is.null(distributions)) {
  # Generate L_t with custom distribution
  l_dist <- distributions$confounder
  mu_l <- ... # existing linear predictor
  df[[paste0("t", t, "_l")]] <- generate_from_dist(l_dist, n, mu_l)
} else {
  # Existing normal generation
  df[[paste0("t", t, "_l")]] <- mu_l + rnorm(n)
}
```

### Task 5: Add Estimator Library

Create pre-built estimator functions for common methods:

```r
# margot_estimators.R

#' IPTW estimator
margot_iptw <- function(data, outcome_var, treatment_vars, baseline_vars) {
  # Implementation
}

#' G-computation estimator  
margot_gcomp <- function(data, outcome_var, treatment_vars, confounder_vars) {
  # Implementation
}

#' TMLE wrapper (requires tmle package)
margot_tmle <- function(data, ...) {
  # Implementation
}

#' LMTP wrapper (requires lmtp package)
margot_lmtp <- function(data, ...) {
  # Implementation
}
```

### Task 6: Parallel Processing Enhancements

Improve the Monte Carlo parallel processing:

```r
# Better cluster management
setup_margot_cluster <- function(n_cores = NULL, export_objects = NULL) {
  # Create and configure cluster
  # Export margot functions automatically
  # Return cluster object
}

# Memory-efficient parallel processing
margot_monte_carlo_chunked <- function(..., chunk_size = 100) {
  # Process simulations in chunks to manage memory
}
```

### Task 7: Diagnostic and Visualization Tools

```r
# margot_diagnostics.R

#' Comprehensive diagnostic report
diagnose_simulation <- function(sim_results, output_format = "html") {
  # Generate full diagnostic report
  # - DAG visualization
  # - Parameter recovery
  # - Shadowing effects
  # - Estimator performance
}

#' Interactive visualization
plot_mc_results_interactive <- function(mc_results) {
  # Use plotly for interactive plots
}
```

## Integration Points

### With Existing margot Code

1. **margot_simulate()**: Add `distributions` and `shadows` parameters
2. **margot_process_longitudinal_data_wider()**: Ensure compatibility with shadowed data
3. **Metadata**: Extend `margot_meta` attribute to track applied shadows

### With External Packages

```r
# Create wrappers for common packages
margot_to_lmtp <- function(margot_data) {
  # Convert to lmtp format
}

margot_to_grf <- function(margot_data) {
  # Convert to grf format
}

margot_from_mice <- function(mice_imputed_data) {
  # Import imputed data back to margot
}
```

## Testing Requirements

### Unit Tests
```r
# test_shadows.R
test_that("measurement error increases variance", {
  data <- margot_simulate(n = 1000, waves = 2)
  shadow <- create_shadow("measurement_error", params = list(sigma = 1))
  shadowed <- apply_shadow(data, shadow)
  expect_gt(var(shadowed$t1_l), var(data$t1_l))
})

# test_monte_carlo.R  
test_that("Monte Carlo recovers true effect with no shadowing", {
  results <- margot_monte_carlo(
    n_reps = 100,
    estimator_fn = perfect_estimator,
    shadows = NULL
  )
  expect_lt(abs(results$performance$bias), 0.01)
})
```

### Integration Tests
- Full workflow from simulation → shadowing → estimation
- Performance benchmarks
- Memory usage tests

## Documentation Requirements

### Vignettes Needed

1. **Getting Started with Shadowing** - Basic measurement error example
2. **Monte Carlo Evaluation** - Comparing estimators systematically  
3. **Advanced Distributions** - Using non-normal distributions
4. **Method Evaluation** - Complete workflow for paper/analysis

### Function Documentation

All functions need:
- Clear `@description`
- `@param` for all parameters
- `@return` description
- `@examples` (at least one)
- `@export` for user-facing functions

## Performance Considerations

1. **Memory**: Large simulations can exhaust memory
   - Implement chunked processing
   - Option to not save intermediate data
   
2. **Speed**: Monte Carlo can be slow
   - Parallel processing by default
   - Pre-compile estimator functions
   - Vectorize where possible

3. **Numerical Stability**: 
   - Check for extreme weights in IPW
   - Truncate/windsorize as needed
   - Handle edge cases gracefully

## Next Development Steps

### Phase 1: Core Integration (Priority)
1. Replace `margot_process_longitudinal_data_wider()` dependency
2. Implement item missingness and positivity shadows
3. Integrate shadowing system with `margot_simulate()`
4. Complete distribution integration
5. Add basic estimator library
6. Write essential tests

### Phase 2: Enhanced Features
1. Advanced measurement error types
2. Improved parallel processing
3. Diagnostic tools
4. External package integration

### Phase 3: Polish & Release
1. Complete documentation
2. Performance optimization
3. CRAN preparation
4. Publication examples

## Code Style Guidelines

- Use tidyverse style guide
- Lowercase function names with underscores
- Explicit `return()` statements
- Comment complex logic
- Use `@keywords internal` for non-exported functions

## Questions for Development

1. Should shadows be applied inside `margot_simulate()` or always post-hoc?
2. How deep should external package integration go?
3. What's the priority: flexibility or ease of use?
4. Should we create a separate `margot.shadows` package?
5. For positivity violations: Should we filter units completely or set their weights to 0?
6. For item missingness: Should we implement multiple imputation interfaces?
7. How should temporal ordering be enforced when new variables are created by shadows?

## Example Development Session

```r
# Load current code
source("margot_simulate_7.R")
source("margot_shadows.R")
source("margot_monte_carlo.R")

# Test basic workflow
dat <- margot_simulate(n = 100, waves = 2)
shadow <- create_shadow("measurement_error", params = list(sigma = 0.5))
shadowed <- apply_shadow(dat, shadow)

# Verify it works
diagnose_shadowing(dat, shadowed)

# Test positivity violation
# Example: Treatment is impossible for individuals with high baseline risk
positivity_shadow <- create_positivity_shadow(
  exposure_var = "t1_a",
  filter_fn = function(data) {
    # Treatment contraindicated if baseline risk score > 2
    risk_score <- data$b1 + data$b2
    return(risk_score <= 2)  # TRUE for valid exposures
  }
)

filtered_data <- apply_shadow(dat, positivity_shadow)
cat("Original n:", nrow(dat), "After positivity filter:", nrow(filtered_data))
```

---

## Summary

The margot extensions provide a powerful framework for causal inference simulation studies. The modular design allows researchers to:

1. Generate data from known SCMs
2. Apply realistic observational distortions  
3. Evaluate any statistical estimator
4. Compare methods systematically

**Key additions from these notes:**
- Item-level missingness allows more realistic missing data patterns
- Positivity violations help researchers understand when causal effects are poorly defined
- Both features are implemented as shadows, maintaining architectural consistency

Development should focus on maintaining the clean separation between truth and observation while adding flexibility for users to specify their own distributions, shadows, and estimators.
