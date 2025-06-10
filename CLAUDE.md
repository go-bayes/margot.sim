- remember to update _pkgdown.yml after adding or deleting exported functions
- use lower case comments and nz english

# margot.sim Package Context

## Overview
margot.sim is an R package for simulating longitudinal data with realistic observational challenges and evaluating causal inference methods through Monte Carlo simulation. It implements a "shadowing" framework inspired by Plato's Allegory of the Cave, where observed data are distorted versions ("shadows") of true causal processes.

## Core Components

### 1. Simulation Framework
- `margot_simulate()`: Main function for generating longitudinal data following structural causal models
- `margot_simulate_causal()`: Wrapper for causal inference simulations
- `margot_simulate_flex()`: Supports flexible non-normal distributions
- Handles binary and continuous exposures/outcomes with complex temporal relationships

### 2. Shadowing System
The package's key innovation is applying realistic observational distortions:

**Measurement Error Shadows:**
- Classical error, misclassification, differential error
- Dichotomisation, correlated errors

**Missing Data Shadows:**
- MCAR, MAR, MNAR mechanisms
- Temporal order validation (future can't affect past)

**Selection/Censoring Shadows:**
- Positivity violations, selection bias
- Post-hoc censoring with various dependencies

### 3. Monte Carlo Framework
- `margot_monte_carlo()`: Evaluates estimator performance systematically
- Calculates bias, variance, MSE, coverage automatically
- Supports parallel processing

### 4. Shift Interventions (new)
- Modified treatment policies (MTPs)
- Threshold-based and wave-specific interventions

## Technical Details
- Version: 0.1.1 (experimental)
- Main dependencies: tidyverse, MASS
- Comprehensive test coverage with testthat
- CI/CD via GitHub Actions
- Documentation: 3 vignettes + extensive function docs

## Recent Changes
- Fixed vignette errors
- Added temporal order validation for shadows
- Implemented shift intervention functions
- Enhanced testing and documentation

## Development Practices
- Always run R CMD check before commits
- Update NEWS.md for user-facing changes
- Ensure examples run without errors
- Follow tidyverse style guide
- Test shadow temporal ordering carefully

## Shadow Bias Comparison Framework Design

### Overview
This framework enables systematic comparison of causal effect estimates before and after applying observational shadows, measuring how data distortions affect our ability to recover true causal effects (ATE, ATT, ATU, and eventually HTE).

### Core Design Principles

#### 1. Dual Data Architecture
- Always maintain two parallel datasets: `data_true` (complete, undistorted) and `data_observed` (after shadows)
- This enables "ground truth" comparisons at any point in the analysis
- Shadows preserve original values as `*_true` columns for affected variables

#### 2. Standardized Estimand Computation
```r
compute_causal_effects <- function(data, exposure, outcome, covariates = NULL, estimands = c("ate", "att", "atu")) {
  # Returns list with:
  # - ate: Average Treatment Effect E[Y(1) - Y(0)]
  # - att: Average Treatment on Treated E[Y(1) - Y(0) | A = 1]
  # - atu: Average Treatment on Untreated E[Y(1) - Y(0) | A = 0]
  # - (future) hte: Heterogeneous Treatment Effects by subgroups
}
```

#### 3. Shadow Workflow Enhancement
- Simulation functions return structured output:
```r
list(
  data_true = ...,           # Complete true data
  data_observed = ...,       # After shadows applied
  shadows_applied = ...,     # Which shadows were used
  effects_true = ...,        # True causal effects
  effects_observed = ...,    # Estimated from shadowed data
  comparison = ...           # Bias metrics
)
```

#### 4. Comparison Framework
```r
compare_shadow_bias <- function(effects_true, effects_observed) {
  # Computes for each estimand:
  # - Absolute bias
  # - Relative bias (%)
  # - Root mean squared error (for MC simulations)
  # - Coverage (for MC simulations)
}
```

#### 5. Monte Carlo Integration
- Extended `margot_monte_carlo()` to automatically:
  - Preserve true data before shadowing
  - Compute both true and observed effects
  - Track bias separately for each estimand
  - Return structured comparison results

### Implementation Phases

#### Phase 1: Core Infrastructure
1. Create `compute_causal_effects()` function
2. Modify simulation functions for dual data architecture
3. Update shadow application to preserve truth

#### Phase 2: Comparison Tools
1. Build `compare_shadow_bias()` function
2. Create visualization methods
3. Add summary statistics

#### Phase 3: Integration
1. Update Monte Carlo framework
2. Ensure margot package compatibility
3. Add comprehensive testing

### Future Extensions
- Model misspecification as separate bias source (not a shadow)
- HTE using generic baseline variables (b1, b2, b3)
- Threshold-based subgroup definitions for continuous variables

### Design Rationale
- Separates data distortions (shadows) from analysis choices (models)
- Enables clean comparison of "what we would know" vs "what we can know"
- Maintains backward compatibility while adding new capabilities
- Focuses on getting ATE estimation right before tackling heterogeneous effects

## Current Development Plan (2025-01-11)

### High Priority Tasks
1. **Create Misclassification Error Vignette/Example**
   - Basic setup: wave 0 (baseline), wave 1 (exposure with 20% misclassification), wave 2 (outcome)
   - Compare plain vs differential misclassification (error depends on baseline variables)
   - Show how differential misclassification creates more bias than random misclassification
   - Location: New vignette `vignettes/misclassification-bias.Rmd` or section in shadows vignette

2. **Add Model Misspecification Examples**
   - Show users how to generate data with correct causal structure
   - Demonstrate fitting intentionally misspecified models (omitting confounders, wrong functional forms)
   - Compare ATEs from correctly vs incorrectly specified models
   - Show interaction between shadows and model misspecification
   - Add to practical workflow vignette and function examples

3. **Add Roxygen2 Examples to Shadow Functions**
   Priority order:
   - `apply_misclassification()` - sensitivity/specificity examples
   - `apply_differential_error()` - predictor-dependent error
   - `apply_classical_error()` - basic measurement error
   - `apply_dichotomisation()` - continuous to binary with error
   - `create_shadow()` - diverse examples
   - `apply_shadows()` - multiple shadows simultaneously

### Medium Priority Tasks
4. **Add Roxygen2 Examples to Core Functions**
   - `margot_simulate()` - comprehensive examples
   - `margot_simulate_causal()` - intervention examples
   - `compute_causal_effects()` - different estimands
   - `compare_shadow_effects()` - before/after comparisons

5. **Add Roxygen2 Examples to Causal Effect Functions**
   - Transport analysis functions
   - Effect comparison functions
   - Heterogeneity analysis

### Low Priority Tasks
6. **Add Roxygen2 Examples to Utility Functions**
   - Distribution functions
   - Helper utilities
   - Diagnostic functions

### Implementation Notes
- Each roxygen example should include:
  - Brief description of use case
  - Complete, runnable code
  - Expected output or interpretation
  - 2-3 different scenarios per function where relevant
- Focus on practical, realistic examples that users would encounter
- Ensure examples demonstrate best practices

## Package Gaps and Future Priorities (2025-01-11)

### Immediate Priorities (High Impact for Users)
1. **Complete roxygen documentation with examples** - Many functions lack comprehensive examples
2. **Add key missing shadows**:
   - Truncation (different from censoring - values beyond thresholds not recorded)
   - Coarsening (continuous data recorded in bins/categories)
   - Mode effects (measurement differences by survey method)
3. **Implement basic HTE functionality** - Currently limited despite being in design
4. **Create diagnostic visualization tools** - For balance, positivity, shadow validation

### Medium-term Priorities
1. **Causal method implementation** (Simplified strategy):
   - Focus on `lmtp` integration for modified treatment policies
   - Focus on `causal_forests` for heterogeneous treatment effects
   - Leverage batch estimators from margot package (see R/to-develop)
   - Include E-value computation for sensitivity analysis
   - Note: Deprioritizing standalone G-computation/IPW/G-estimation implementations
2. **Integration with tidymodels/mlr3** - Modern ML ecosystem
3. **Performance optimizations** - Large data, parallel processing, memory efficiency
4. **Interactive diagnostic tools** - Shiny apps for exploring shadows/effects

### Long-term Vision
1. **Comprehensive HTE framework** - CATE estimation, subgroup discovery, causal forests
2. **Full time-varying treatment support** - Currently limited
3. **Integration hub for causal methods** - Bridge to TMLE, SuperLearner, etc.
4. **Educational platform** - Interactive tutorials, case studies

### Other Identified Gaps
- **Missing Methods**: Doubly robust, IV, regression discontinuity
- **Advanced Features**: Competing risks, multi-state models, spatial/clustered data
- **User Experience**: Pipeline functions, automatic reporting, better errors
- **Validation**: Balance diagnostics, convergence checks, cross-package validation
- **Integration**: Survey packages, export formats (SAS/Stata/SPSS)

### Key Insight
The main gap is bridging from simulation to practical analysis - making it easier for users to go from "I have this real dataset with these problems" to "here's my bias-corrected estimate with appropriate uncertainty quantification."

# important-instruction-reminders
Do what has been asked; nothing more, nothing less.
NEVER create files unless they're absolutely necessary for achieving your goal.
ALWAYS prefer editing an existing file to creating a new one.
NEVER proactively create documentation files (*.md) or README files. Only create documentation files if explicitly requested by the User.

      
      IMPORTANT: this context may or may not be relevant to your tasks. You should not respond to this context or otherwise consider it in your response unless it is highly relevant to your task. Most of the time, it is not relevant.