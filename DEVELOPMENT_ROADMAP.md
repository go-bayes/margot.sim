# margot.sim Development Roadmap

## Overview

This document consolidates feedback from the recent code review with existing implementation plans, creating a unified roadmap for package development. The roadmap prioritises architectural improvements that must be completed before the API solidifies, while maintaining the package's core strengths of usability and the intuitive "shadow" metaphor.

## Immediate Priorities (Do Before API Solidifies)

### 1. Formal Object System (S3/R6)
**Status**: Not started  
**Priority**: CRITICAL - Must be done before external users adopt the package  
**Why**: Once users start writing shadows and scenarios, changing the object system will break their code

**Implementation**:
```r
# S3 approach (recommended for simplicity)
new_shadow <- function(type, params) {
  structure(
    list(type = type, params = params),
    class = c(paste0(type, "_shadow"), "margot_shadow")
  )
}

new_scenario <- function(name, description, shadows = list()) {
  structure(
    list(name = name, description = description, shadows = shadows),
    class = "margot_scenario"
  )
}

# Generic methods
print.margot_shadow <- function(x, ...) {
  cat("Shadow:", x$type, "\n")
  cat("Parameters:\n")
  print(x$params)
  invisible(x)
}

validate.margot_shadow <- function(x, ...) {
  # Shadow-specific validation
  UseMethod("validate", x)
}
```

**Files to modify**:
- `R/margot-shadows.R` - Convert shadow functions to S3
- `R/margot-scenario.R` - Convert scenario functions to S3
- Add new file: `R/margot-s3-methods.R` for generic methods

### 2. Seed Discipline & Parallel RNG
**Status**: Partially implemented (basic seed support exists)  
**Priority**: CRITICAL - Silent reproducibility failures are unacceptable  
**Why**: Current parallel implementation may not guarantee reproducibility

**Implementation**:
```r
# Option 1: future package approach
margot_monte_carlo <- function(..., seed = NULL) {
  if (!is.null(seed)) {
    future::plan(future::multisession, seed = TRUE)
    withr::with_seed(seed, {
      # Run simulations
    })
  }
}

# Option 2: parallel package approach
margot_monte_carlo <- function(..., seed = NULL) {
  if (!is.null(seed)) {
    RNGkind("L'Ecuyer-CMRG")
    set.seed(seed)
    mc.reset.stream()
  }
  # Use parallel::mclapply with mc.set.seed = TRUE
}
```

**Files to modify**:
- `R/monte-carlo.R` - Implement proper RNG stream management
- All simulation functions to accept and propagate seed parameter

### 3. Shadow Algebra/Dependencies
**Status**: Not implemented  
**Priority**: HIGH - Shadows can interact in unexpected ways  
**Why**: E.g., truncation changes variance assumptions for measurement error

**Implementation**:
```r
# Shadow dependency system
shadow_dependencies <- list(
  truncation = list(
    affects = c("variance", "mean"),
    update_fn = function(params, truncation_params) {
      # Recalculate variance after truncation
      params$variance <- calculate_truncated_variance(
        params$variance, 
        truncation_params
      )
      params
    }
  )
)

apply_shadows_with_dependencies <- function(data, shadows) {
  # Apply shadows in dependency order
  ordered_shadows <- topological_sort_shadows(shadows)
  
  for (shadow in ordered_shadows) {
    # Check for upstream effects
    upstream_params <- get_upstream_parameters(shadow, applied_shadows)
    
    # Update parameters if needed
    if (!is.null(upstream_params)) {
      shadow$params <- update_shadow_params(shadow, upstream_params)
    }
    
    # Apply shadow
    data <- apply_shadow(data, shadow)
    applied_shadows <- c(applied_shadows, list(shadow))
  }
  
  data
}
```

**Files to modify**:
- `R/margot-shadows.R` - Add dependency system
- New file: `R/margot-shadow-dependencies.R`

### 4. Positivity & Support Diagnostics
**Status**: Not implemented  
**Priority**: HIGH - Users need this immediately  
**Why**: Violated positivity is a common cause of estimator failure

**Implementation**:
```r
margot_check_positivity <- function(data, treatment_var, covariates, 
                                   threshold = 0.1, plot = TRUE) {
  # Calculate propensity scores
  formula <- as.formula(paste(treatment_var, "~", paste(covariates, collapse = " + ")))
  ps_model <- glm(formula, data = data, family = binomial())
  data$propensity <- predict(ps_model, type = "response")
  
  # Check for violations
  violations <- data %>%
    group_by(!!sym(treatment_var)) %>%
    summarise(
      min_ps = min(propensity),
      max_ps = max(propensity),
      n_extreme = sum(propensity < threshold | propensity > (1 - threshold)),
      n_eff = sum(1 / (propensity * (1 - propensity)))
    )
  
  # Plot if requested
  if (plot) {
    p <- ggplot(data, aes(x = propensity, fill = factor(!!sym(treatment_var)))) +
      geom_density(alpha = 0.5) +
      geom_vline(xintercept = c(threshold, 1 - threshold), 
                 linetype = "dashed", color = "red") +
      labs(title = "Propensity Score Distribution",
           subtitle = paste("Positivity threshold:", threshold))
    print(p)
  }
  
  # Return diagnostics
  structure(
    list(
      violations = violations,
      n_violations = sum(violations$n_extreme),
      effective_sample_size = violations$n_eff,
      plot = if(plot) p else NULL
    ),
    class = "margot_positivity_check"
  )
}
```

**Files to create**:
- `R/margot-diagnostics.R` - Positivity and support checks

### 5. Memory Management
**Status**: Not implemented  
**Priority**: HIGH - Large simulations can exhaust memory  
**Why**: 1000 reps × 10,000 rows × 50 vars × 2 (true/observed) = 2-3 GB

**Implementation options**:
```r
# Option 1: Streaming to disk
margot_monte_carlo <- function(..., output_dir = NULL, keep = "summary") {
  if (!is.null(output_dir)) {
    # Stream results to parquet files
    for (i in seq_len(n_reps)) {
      result <- run_single_simulation(...)
      
      if (keep == "summary") {
        # Only save summary statistics
        summary <- calculate_summary(result)
        arrow::write_parquet(summary, 
          file.path(output_dir, sprintf("summary_%04d.parquet", i)))
      } else {
        # Save full data
        arrow::write_parquet(result$data, 
          file.path(output_dir, sprintf("data_%04d.parquet", i)))
      }
    }
  }
}

# Option 2: On-the-fly summarisation
margot_monte_carlo <- function(..., summarise_fn = NULL) {
  if (!is.null(summarise_fn)) {
    # Only keep summaries in memory
    results <- parallel::mclapply(seq_len(n_reps), function(i) {
      sim_data <- run_single_simulation(...)
      summarise_fn(sim_data)  # User-defined summary function
    })
  }
}
```

**Files to modify**:
- `R/monte-carlo.R` - Add memory management options

### 6. Enhanced Testing Infrastructure
**Status**: Basic tests exist (54% coverage)  
**Priority**: HIGH - Need property-based tests  
**Why**: Must verify bias → 0 as shadow → 0

**Implementation**:
```r
# Property-based test example
test_that("bias approaches zero as shadow strength decreases", {
  shadow_strengths <- c(1, 0.5, 0.1, 0.01, 0)
  
  biases <- sapply(shadow_strengths, function(strength) {
    sim_data <- margot_simulate(
      n = 1000,
      params = list(true_effect = 0.5),
      shadows = list(
        create_shadow("measurement_error", list(sigma = strength))
      )
    )
    
    # Calculate bias
    estimated <- mean(sim_data$estimated_effect)
    true <- 0.5
    abs(estimated - true)
  })
  
  # Bias should decrease monotonically
  expect_true(all(diff(biases) <= 0))
  # Final bias should be near zero
  expect_lt(biases[length(biases)], 0.01)
})
```

**Files to create**:
- `tests/testthat/test-properties.R` - Property-based tests
- Update existing tests to improve coverage

### 7. CRAN Compliance
**Status**: Package passes basic R CMD check  
**Priority**: HIGH - Must be sorted before submission  
**Why**: CRAN requirements affect design decisions

**Checklist**:
- [ ] Reduce example data to < 5 MB
- [ ] Set `LazyData: false` if data is large
- [ ] Ensure all examples run in < 5 seconds
- [ ] Add `\donttest{}` for longer examples
- [ ] Check for platform-specific code
- [ ] Verify no external dependencies in tests

## Next Phase: CRAN Submission Priorities

Based on IMPLEMENTATION_PLAN.md, these are scheduled for completion before CRAN submission:

### 1. Natural Value Interventions
**Status**: Design complete, not implemented  
**Priority**: MEDIUM  
**Files**: `R/margot-simulate-core.R` lines 436-444

### 2. Comprehensive Vignettes
**Status**: Outlined in detail  
**Priority**: HIGH
- `heterogeneous-effects-tutorial.Rmd`
- `practical-shift-interventions.Rmd`
- `monte-carlo-best-practices.Rmd`
- `unmeasured-confounding.Rmd`

### 3. Unmeasured Confounder U
**Status**: Design complete, not implemented  
**Priority**: MEDIUM  
**Implementation**: Add U parameters to `.default_sim_params()`

## Long-term Features (Post-CRAN)

From the implementation plan and code review:

### Phase 1: Enhanced Shadow Framework
- Shadow parameter library (YAML/JSON manifests)
- Advanced shadow plotting and DAG visualisation
- Shadow composition algebra

### Phase 2: Theoretical Enhancements
- Frugal parameterisation interface (`margot_simulate_frugal()`)
- SCM constructor approach
- Variation independence testing

### Phase 3: Advanced Features
- Shift intervention engine completion
- Benchmark suite with classic estimators
- Cross-language support (Python wrapper)
- Path-specific effects and natural direct/indirect effects

## Testing & Documentation Strategy

### Testing Priorities
1. **Property-based tests** - Invariants like bias → 0
2. **Shadow interaction tests** - Verify dependency system
3. **Memory/performance tests** - Large simulation benchmarks
4. **CRAN compliance tests** - Platform independence

### Documentation Priorities
1. **API documentation** - Complete roxygen2 for all functions
2. **Vignettes** - Four comprehensive tutorials
3. **README updates** - Add limitations section
4. **pkgdown site** - Update after each major feature

## Implementation Order

### Week 1-2: Architecture (MUST DO NOW)
1. Implement S3 object system
2. Fix RNG/seed discipline
3. Add shadow dependency system
4. Create positivity diagnostics

### Week 3-4: Infrastructure
1. Memory management options
2. Property-based tests
3. CRAN compliance fixes

### Week 5-6: Features & Documentation
1. Natural value interventions
2. Unmeasured confounder U
3. Write comprehensive vignettes
4. Update all documentation

### Post-CRAN: Advanced Features
- Implement based on user feedback and priorities

## Success Metrics

- [ ] All high-priority items complete before API release
- [ ] Test coverage > 80%
- [ ] All vignettes render without errors
- [ ] Memory usage < 4GB for standard simulations
- [ ] CRAN checks pass on all platforms
- [ ] Reproducibility guaranteed across parallel runs

## File Structure Changes

### New Files Required
```
R/
├── margot-s3-methods.R          # S3 generic methods
├── margot-shadow-dependencies.R  # Shadow interaction system  
├── margot-diagnostics.R         # Positivity & support checks
└── margot-memory.R              # Memory management utilities

tests/testthat/
├── test-properties.R            # Property-based tests
├── test-reproducibility.R       # RNG stream tests
└── test-memory.R               # Memory usage tests

vignettes/
├── heterogeneous-effects-tutorial.Rmd
├── practical-shift-interventions.Rmd
├── monte-carlo-best-practices.Rmd
└── unmeasured-confounding.Rmd
```

### Files to Modify
```
R/
├── margot-shadows.R            # Convert to S3
├── margot-scenario.R           # Convert to S3
├── monte-carlo.R               # Add RNG discipline
└── margot-simulate-core.R      # Natural values, U confounder

man/
└── *.Rd                        # Update all documentation
```

## Notes

- Maintain backward compatibility throughout
- Document all breaking changes if absolutely necessary
- Keep the simple API for basic use cases
- Advanced features should be opt-in, not required