# margot.core Migration Instructions

## FOR THE NEXT CLAUDE CODE INSTANCE

### Context
You are tasked with creating the `margot.core` package by extracting S3 classes and core infrastructure from `margot.sim`. This is CRITICAL and must be done before any new features are added to prevent API breakage.

### Your Working Environment
- You have access to the parent GIT folder containing both:
  - `/margot.sim/` - The current package with all S3 classes
  - `/margot/` - The original margot package (for reference if needed)
- You will create `/margot.core/` as a new package

### PHASE 1: Create margot.core Package Structure

```bash
# From the parent GIT directory
cd /path/to/GIT/
R -e "usethis::create_package('margot.core')"
cd margot.core

# In R:
usethis::use_mit_license("Joseph A. Bulbulia")
usethis::use_git()
usethis::use_readme_rmd()
usethis::use_news_md()
usethis::use_testthat(3)
usethis::use_pkgdown()
```

### PHASE 2: Files to Move/Copy from margot.sim to margot.core

#### 1. **Core S3 Infrastructure** (from `R/margot-s3-methods.R`)
Move these sections:
- `%||%` operator definition
- All shadow constructors and methods:
  - `new_shadow()` and all validation methods
  - `print.margot_shadow`, `summary.margot_shadow`
  - Helper functions: `is_shadow()`, `as_shadow()`
- All scenario constructors and methods:
  - `new_scenario()` and validation
  - `print.margot_scenario`, `c.margot_scenario`, `[.margot_scenario`
  - Helper functions: `is_scenario()`, `as_scenario()`
- Shadow list class and methods
- Data frame conversion methods

#### 2. **Shadow Dependency System** (from `R/margot-shadow-dependencies.R`)
Move ONLY these functions:
- `get_shadow_dependencies()`
- `check_shadow_ordering()`
- `reorder_shadows()`
- **ADD MISSING**: `detect_cycles()` function for DAG validation!

#### 3. **Core Utilities** (from `R/margot-core-exports.R`)
Move:
- `.margot_core_classes()` list
- `.margot_core_generics()` list
- `margot_options()` and `margot_option_defaults()`
- Remove all references to "future margot.core" since this IS margot.core

#### 4. **Create New File**: `R/margot-panel.R`
Based on planning documents, create:
```r
#' Create a margot panel object
#' @export
margot_panel <- function(data, id, time) {
  stopifnot(all(c(id, time) %in% names(data)))
  structure(
    data,
    id_col = id,
    time_col = time,
    class = c("margot_panel", class(data))
  )
}

#' @export
print.margot_panel <- function(x, ...) {
  cat("<margot_panel>\n")
  cat("  rows:", nrow(x), "\n")
  cat("  ids:", length(unique(x[[attr(x, "id_col")]])), "\n")
  cat("  time periods:", length(unique(x[[attr(x, "time_col")]])), "\n")
  invisible(x)
}
```

### PHASE 3: Update margot.sim to Use margot.core

#### 1. **Update DESCRIPTION**
Add to Imports:
```
Imports:
    margot.core (>= 0.0.1),
    ... (existing imports)
```

#### 2. **Remove Moved Code**
Delete from margot.sim:
- The S3 methods that now live in margot.core
- The dependency functions that moved
- The option management functions

#### 3. **Update Function Calls**
Replace internal calls with margot.core:: prefixes where needed

#### 4. **Add Re-exports** (in a new file `R/margot-core-reexports.R`)
```r
#' @importFrom margot.core new_shadow
#' @export
margot.core::new_shadow

#' @importFrom margot.core new_scenario  
#' @export
margot.core::new_scenario

# ... etc for main user-facing functions
```

### PHASE 4: Critical Additions

#### 1. **Add Cycle Detection** (MISSING - HIGH PRIORITY!)
In margot.core, create `R/dependency-validation.R`:
```r
#' Detect cycles in shadow dependency graph
#' @export
detect_cycles <- function(dependencies) {
  # Implementation needed - use DFS to detect cycles
  # This is CRITICAL for preventing infinite loops
}
```

#### 2. **Add Contract Tests**
In margot.sim tests, add:
```r
test_that("margot.core classes remain stable", {
  # Test that S3 classes have expected structure
  shadow <- margot.core::new_shadow(...)
  expect_s3_class(shadow, "margot_shadow")
  expect_s3_class(shadow, "measurement_error_shadow")
})
```

### PHASE 5: Documentation

#### 1. **margot.core README**
```markdown
# margot.core

Core S3 classes and infrastructure for the margotsphere ecosystem.

This package provides:
- S3 classes for shadows and scenarios
- Validation functions
- Shadow dependency management
- Core utilities shared across margotsphere packages

## Installation
devtools::install_github("go-bayes/margot.core")
```

#### 2. **Update margot.sim README**
Add note about margot.core dependency

### CRITICAL SUCCESS FACTORS

1. **Minimal Dependencies**: margot.core should have almost NO dependencies beyond base R
2. **No Circular Imports**: margot.core must NOT import from any other margotsphere package
3. **Complete S3 System**: ALL S3 classes must move to prevent "class leak"
4. **Add Cycle Detection**: The shadow dependency system needs cycle detection (currently missing)
5. **Preserve APIs**: User-facing functions in margot.sim should work identically after migration

### Testing Strategy

1. Run margot.sim tests BEFORE migration
2. Create margot.core with its own tests
3. Update margot.sim to use margot.core
4. Run margot.sim tests AFTER migration - they should all pass
5. Check that examples still work

### Git Strategy

1. Create margot.core in a new repo
2. Make changes to margot.sim in a feature branch
3. Test both packages together
4. Only merge when both packages work correctly together

### Priority Order

1. **FIRST**: Get S3 classes working in margot.core
2. **SECOND**: Add missing cycle detection
3. **THIRD**: Update margot.sim to use margot.core
4. **FOURTH**: Ensure all tests pass
5. **ONLY THEN**: Continue with new features

Remember: This migration is CRITICAL to prevent future breaking changes. The planning documents emphasize this must be done BEFORE any new features are added.