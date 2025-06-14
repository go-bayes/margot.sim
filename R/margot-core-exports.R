#' Exports for margot.core Package
#'
#' @description
#' This file contains functions and classes that will eventually be moved
#' to the margot.core package. They are currently defined here but will
#' be imported from margot.core in the future.
#'
#' @details
#' The margotverse architecture splits functionality across multiple packages:
#' - margot.core: Core S3 classes and common utilities
#' - margot.sim: Simulation and shadow functionality
#' - margot.lmtp: LMTP estimation methods
#' - margot.grf: Generalized random forest methods
#' - margot.viz: Visualization functions
#' - margot.report: Reporting templates
#'
#' This file helps track what will move to margot.core.

# list of S3 classes to export to margot.core --------------------------

#' Get list of S3 classes for margot.core
#'
#' @return Character vector of S3 class names that should move to margot.core
#' @keywords internal
#' @examples
#' \dontrun{
#' # see what classes will move
#' .margot_core_classes()
#' }
.margot_core_classes <- function() {
  c(
    # shadow classes
    "margot_shadow",
    "measurement_error_shadow",
    "missing_data_shadow", 
    "censoring_shadow",
    "truncation_shadow",
    "selection_shadow",
    "positivity_shadow",
    "mode_effects_shadow",
    "coarsening_shadow",
    "item_missingness_shadow",
    "combined_shadow",
    "shadow_list",
    
    # scenario classes
    "margot_scenario",
    "margot_scenario_result",
    "margot_scenario_comparison",
    
    # simulation classes
    "margot_causal_sim",
    "margot_mc_results",
    "margot_distribution",
    "margot_sensitivity_analysis",
    
    # diagnostic classes
    "margot_positivity_diagnostic",
    "margot_shadow_diagnostics",
    "shadow_effects"
  )
}

#' Get list of generic functions for margot.core
#'
#' @return Character vector of generic function names for margot.core
#' @keywords internal
.margot_core_generics <- function() {
  c(
    # constructors
    "new_shadow",
    "new_scenario", 
    "new_shadow_list",
    
    # validators
    "validate_shadow",
    "validate_scenario",
    
    # coercion
    "as_shadow",
    "as_scenario",
    
    # type checking
    "is_shadow",
    "is_scenario",
    
    # shadow operations
    "apply_shadow",
    "update_shadow_params",
    
    # scenario operations  
    "apply_scenario",
    "compare_scenarios"
  )
}

#' Check if margot.core is available
#'
#' @return Logical indicating if margot.core package is installed
#' @export
#' @examples
#' has_margot_core()
has_margot_core <- function() {
  # margot.core doesn't exist yet, so always return FALSE
  FALSE
}

#' Get S3 class source
#'
#' @description
#' Returns where an S3 class is defined - either margot.sim or margot.core
#' 
#' @param class_name Character string of S3 class name
#' @return Character string: "margot.sim", "margot.core", or "unknown"
#' @export
#' @examples
#' get_class_source("margot_shadow")
get_class_source <- function(class_name) {
  if (!has_margot_core()) {
    # if margot.core doesn't exist, everything is in margot.sim
    if (class_name %in% .margot_core_classes()) {
      return("margot.sim")
    }
  } else {
    # check if class is in margot.core
    # Since margot.core doesn't exist yet, this will always be empty
    core_classes <- character(0)
    
    if (class_name %in% core_classes) {
      return("margot.core")
    }
  }
  
  # check if it's in margot.sim
  if (class_name %in% .margot_core_classes()) {
    return("margot.sim")
  }
  
  return("unknown")
}

#' Compatibility layer for margot.core migration
#'
#' @description
#' This function provides a compatibility layer during the migration
#' to margot.core. It will emit deprecation warnings when classes
#' move to margot.core.
#'
#' @param class_name Character string of class to check
#' @param warn Logical, whether to emit deprecation warnings
#' @keywords internal
.check_core_migration <- function(class_name, warn = TRUE) {
  if (has_margot_core()) {
    source <- get_class_source(class_name)
    if (source == "margot.core" && warn) {
      warning(
        "Class '", class_name, "' has moved to margot.core. ",
        "Please update your code to use margot.core::", class_name, "()",
        call. = FALSE
      )
    }
  }
}

#' List heavy dependencies that should move to Suggests
#'
#' @return Data frame of packages and their reasons for being optional
#' @export
#' @examples
#' margot_optional_deps()
margot_optional_deps <- function() {
  deps <- data.frame(
    package = c("arrow", "dagitty", "igraph", "future.apply", 
                "ggdag", "DiagrammeR", "visNetwork"),
    reason = c(
      "Large-scale data I/O - only needed for big simulations",
      "DAG visualization - only needed for causal diagrams", 
      "Graph algorithms - only needed for dependency visualization",
      "Parallel processing - only needed for large Monte Carlo",
      "DAG plotting - optional visualization",
      "Flow diagrams - optional visualization",
      "Interactive networks - optional visualization"
    ),
    current = c("Suggests", "Not used", "Suggests", "Suggests",
                "Not used", "Not used", "Not used"),
    stringsAsFactors = FALSE
  )
  
  deps
}

#' Standard margotverse options
#'
#' @description
#' Get or set standard options used across margotverse packages
#'
#' @param ... Named options to set
#' @param .list List of named options to set
#' @return If no arguments, returns list of current margot options
#' @export
#' @examples
#' # get all margot options
#' margot_options()
#' 
#' # set specific options
#' margot_options(verbose = TRUE, parallel = FALSE)
#' 
#' # reset to defaults
#' margot_options(.list = margot_option_defaults())
margot_options <- function(..., .list = NULL) {
  opts <- list(...)
  if (!is.null(.list)) {
    opts <- c(opts, .list)
  }
  
  if (length(opts) == 0) {
    # return current options
    current <- options()
    margot_opts <- current[grep("^margot\\.", names(current))]
    return(margot_opts)
  }
  
  # set options with margot prefix
  opts_to_set <- stats::setNames(
    opts,
    paste0("margot.", names(opts))
  )
  
  options(opts_to_set)
  invisible(opts_to_set)
}

#' Get default margotverse options
#'
#' @return List of default option values
#' @export
#' @examples
#' margot_option_defaults()
margot_option_defaults <- function() {
  list(
    verbose = FALSE,
    parallel = FALSE,
    seed = 123,
    cache_dir = tempdir(),
    progress = interactive(),
    validate = TRUE,
    max_memory = "4GB"
  )
}