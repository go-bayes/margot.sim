# GitHub Issues for margot.sim

## Critical Issues

### Issue 1: Enforce temporal order validation across all shadow types
**Priority**: High
**Labels**: bug, validation

Currently, temporal order validation is only applied to some shadow types. Need to:
- Enforce validation in `apply_shadows()` for all shadow types
- Ensure shadows can only depend on concurrent or past variables
- Add comprehensive tests

### Issue 2: Add validation for estimator returns in Monte Carlo
**Priority**: High  
**Labels**: enhancement, validation

`margot_monte_carlo()` should validate estimator functions return required fields:
- Check for `estimate` (required)
- Warn if missing `se` or `converged`
- Provide clear error messages

### Issue 3: Clarify two-stage censoring approach
**Priority**: Medium
**Labels**: documentation

The relationship between `apply_censoring = TRUE` in `margot_simulate()` and `apply_censoring_post_hoc()` is confusing:
- Document why censoring probs are calculated but not applied
- Provide clear workflow examples
- Consider renaming for clarity

### Issue 4: Implement heterogeneous treatment effects by baseline covariates
**Priority**: High
**Labels**: enhancement, feature

Add support for treatment effect heterogeneity by baseline characteristics:
- Effect modification by baseline covariates (b1, b2, b3)
- Effect modification by baseline outcome (t0_y) if available
- Effect modification by baseline exposure (t0_a)
- Clear documentation and examples

### Issue 5: Track shadow interaction effects
**Priority**: Medium
**Labels**: enhancement, validation

When multiple shadows are applied sequentially:
- Track how earlier shadows affect later ones
- Warn about potentially problematic combinations
- Document shadow ordering best practices

### Issue 6: Validate intervention function outputs
**Priority**: High
**Labels**: bug, validation

Add validation after intervention functions are applied:
- Check binary treatments are 0/1
- Check continuous treatments are within reasonable bounds
- Provide informative error messages

### Issue 7: Add warnings for positivity violations
**Priority**: Medium
**Labels**: enhancement, user-experience

When positivity shadows filter data:
- Warn if >X% of data removed
- Report which strata are empty
- Suggest diagnostics

### Issue 8: Fix misclassification for continuous variables
**Priority**: Medium
**Labels**: bug

Currently only warns but continues. Should either:
- Error with clear message for continuous vars
- Implement proper continuous misclassification

### Issue 9: Add memory-efficient options for large Monte Carlo
**Priority**: Low
**Labels**: enhancement, performance

For large simulations with `save_data = TRUE`:
- Option to save to disk
- Implement chunking
- Document memory requirements

### Issue 10: Document parallel processing limitations
**Priority**: Medium
**Labels**: documentation

Clarify what functions/objects are available in parallel workers:
- Document which objects need explicit export
- Provide examples with custom functions
- Add troubleshooting guide

### Issue 11: Implement flexible baseline covariate generation
**Priority**: Medium
**Labels**: enhancement, feature

Currently hardcoded to MVN(0, Σ) with correlation 0.3:
- Allow custom baseline distributions
- Support categorical baselines
- Enable time-varying baseline generation

### Issue 12: Add support for bounded continuous outcomes
**Priority**: Low
**Labels**: enhancement, feature

Many real outcomes have natural bounds:
- Add `outcome_bounds` parameter
- Implement appropriate transformations
- Document bounded outcome handling

### Issue 13: Clarify causal estimands in longitudinal settings
**Priority**: High
**Labels**: documentation

Better documentation of what estimands mean longitudinally:
- Define ATE over which time periods
- Clarify natural vs stochastic interventions
- Provide estimand notation

### Issue 14: Add performance benchmarks
**Priority**: Low
**Labels**: documentation, performance

Provide guidance on computational complexity:
- Benchmark different parameter combinations
- Document memory/time scaling
- Optimization tips