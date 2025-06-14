# margot.sim Architecture for margotverse

## Overview

This document describes how margot.sim fits into the broader margotverse ecosystem and the architectural changes needed to support this integration.

## Core Design Principles

1. **Minimal Core Dependencies**: margot.sim should only depend on essential packages
2. **Clean Interfaces**: Well-defined S3 classes and generics that can be shared
3. **Optional Heavy Dependencies**: Large packages (arrow, dagitty, etc.) should be in Suggests
4. **Reproducible Simulations**: Proper RNG management for parallel execution

## S3 Classes to Share

The following S3 classes should eventually move to margot.core:

### Shadow Classes
- `margot_shadow`: Base class for all shadow types
- `{type}_shadow`: Specific shadow implementations
- `shadow_list`: Collection of shadows

### Scenario Classes  
- `margot_scenario`: Bundle of shadows with metadata
- `margot_scenario_result`: Results from applying scenarios
- `margot_scenario_comparison`: Comparison across scenarios

### Simulation Classes
- `margot_causal_sim`: Causal simulation results
- `margot_mc_results`: Monte Carlo results
- `margot_distribution`: Distribution specifications

## Interface with Other Packages

### Inputs from margot.core
- `margot_panel`: Panel data structure (when available)
- Common generics: `tidy()`, `augment()`, `glance()`
- Validation functions

### Outputs to margot.lmtp  
- Simulated datasets with true/observed structure
- Shadow specifications for sensitivity analysis
- Monte Carlo evaluation frameworks

### Outputs to margot.grf
- Simulated datasets for forest methods
- Heterogeneous effect scenarios
- Benchmark datasets

### Outputs to margot.viz
- S3 objects with plot methods
- Diagnostic information
- Shadow effect summaries

## Migration Strategy

### Phase 1: Current State (COMPLETED)
- [x] Implement S3 object system within margot.sim
- [x] Create constructors, validators, and methods
- [x] Ensure backward compatibility

### Phase 2: Prepare for Split
- [ ] Document all exported S3 classes
- [ ] Create `margot_sim_` prefixed versions of core classes
- [ ] Add deprecation warnings for direct class usage
- [ ] Move heavy dependencies to Suggests

### Phase 3: Create margot.core
- [ ] Extract S3 classes to margot.core
- [ ] Re-export from margot.sim for compatibility
- [ ] Update documentation and examples

### Phase 4: Full Integration  
- [ ] Import S3 classes from margot.core
- [ ] Remove duplicate definitions
- [ ] Update vignettes for margotverse workflow

## Dependencies to Move to Suggests

Current hard dependencies that should become optional:
- arrow (for large-scale I/O)
- dagitty (for DAG visualization) 
- Future heavy parallel backends

## Testing Strategy

1. **Contract Tests**: Ensure S3 classes maintain consistent interfaces
2. **Integration Tests**: Test with other margotverse packages
3. **Backward Compatibility**: Ensure existing code continues to work

## Options and Configuration

Standardize options across margotverse:
- `options(margot.verbose = TRUE/FALSE)`
- `options(margot.parallel = TRUE/FALSE)`  
- `options(margot.seed = 123)`
- `options(margot.cache_dir = "path")`

## Next Steps

1. Create issue tracking margotverse integration
2. Add CI workflow for multi-package testing
3. Document breaking changes in NEWS.md
4. Create migration guide for users