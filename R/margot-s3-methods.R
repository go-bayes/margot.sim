#' S3 Methods for margot Objects
#'
#' @description
#' This file implements the formal S3 object system for margot.sim,
#' providing standardised constructors, validators, and methods for
#' shadows and scenarios.

# Shadow S3 Methods -------------------------------------------------------

#' Constructor for margot_shadow objects
#'
#' @description
#' Low-level constructor for shadow objects. Users should typically use
#' create_shadow() instead.
#'
#' @param type Character string specifying shadow type
#' @param params List of parameters for the shadow
#' @param name Character string naming the shadow
#' @param ... Additional attributes
#'
#' @return A margot_shadow object with appropriate subclass
#' @export
#' @keywords internal
new_shadow <- function(type, params = list(), name = NULL, ...) {
  # validate basic inputs
  stopifnot(is.character(type), length(type) == 1)
  stopifnot(is.list(params))
  
  # create object
  structure(
    list(
      type = type,
      params = params,
      name = name %||% paste0(type, "_shadow"),
      ...
    ),
    class = c(paste0(type, "_shadow"), "margot_shadow")
  )
}

#' Validator for margot_shadow objects
#'
#' @description
#' Validates that a shadow object has the correct structure and
#' valid parameters for its type.
#'
#' @param x An object to validate
#' @param ... Additional arguments passed to specific validators
#'
#' @return The validated object (invisibly)
#' @export
validate_shadow <- function(x, ...) {
  UseMethod("validate_shadow")
}

#' Default shadow validator
#' @export
#' @keywords internal
validate_shadow.default <- function(x, ...) {
  stop("No validate_shadow method for class ", class(x)[1])
}

#' Base shadow validator
#' @export
#' @keywords internal
validate_shadow.margot_shadow <- function(x, ...) {
  # check required fields
  if (!all(c("type", "params", "name") %in% names(x))) {
    stop("Shadow object missing required fields")
  }
  
  # check types
  if (!is.character(x$type) || length(x$type) != 1) {
    stop("Shadow type must be a single character string")
  }
  
  if (!is.list(x$params)) {
    stop("Shadow params must be a list")
  }
  
  if (!is.character(x$name) || length(x$name) != 1) {
    stop("Shadow name must be a single character string")
  }
  
  # check if there's a specific validator for this shadow type
  specific_class <- class(x)[1]  # get most specific class
  
  # if there's a specific validator, use it
  if (specific_class != "margot_shadow" && 
      !is.null(getS3method("validate_shadow", specific_class, optional = TRUE))) {
    NextMethod()
  } else {
    # base validation passes
    invisible(x)
  }
}

#' Measurement error shadow validator
#' @export
#' @keywords internal
validate_shadow.measurement_error_shadow <- function(x, ...) {
  params <- x$params
  
  # check required parameters
  if (!("variables" %in% names(params)) || length(params$variables) == 0) {
    stop("Measurement error shadow requires 'variables' parameter")
  }
  
  if (!("error_type" %in% names(params))) {
    stop("Measurement error shadow requires 'error_type' parameter")
  }
  
  # validate error type
  valid_types <- c("classical", "differential", "dichotomise", "correlated", "misclassification")
  if (!params$error_type %in% valid_types) {
    stop("Invalid error_type. Must be one of: ", paste(valid_types, collapse = ", "))
  }
  
  # type-specific validation
  if (params$error_type == "classical" && !("sigma" %in% names(params))) {
    stop("Classical error requires 'sigma' parameter")
  }
  
  if (params$error_type == "misclassification") {
    if (!all(c("sensitivity", "specificity") %in% names(params))) {
      stop("Misclassification requires 'sensitivity' and 'specificity' parameters")
    }
    if (params$sensitivity < 0 || params$sensitivity > 1) {
      stop("Sensitivity must be between 0 and 1")
    }
    if (params$specificity < 0 || params$specificity > 1) {
      stop("Specificity must be between 0 and 1")
    }
  }
  
  invisible(x)
}

#' Censoring shadow validator
#' @export
#' @keywords internal
validate_shadow.censoring_shadow <- function(x, ...) {
  params <- x$params
  
  if (!("rate" %in% names(params))) {
    stop("Censoring shadow requires 'rate' parameter")
  }
  
  if (params$rate < 0 || params$rate > 1) {
    stop("Censoring rate must be between 0 and 1")
  }
  
  invisible(x)
}

#' Item missingness shadow validator
#' @export
#' @keywords internal
validate_shadow.item_missingness_shadow <- function(x, ...) {
  params <- x$params
  
  if (!("variables" %in% names(params)) || length(params$variables) == 0) {
    stop("Item missingness shadow requires 'variables' parameter")
  }
  
  if (!("rate" %in% names(params))) {
    stop("Item missingness shadow requires 'rate' parameter")
  }
  
  if (params$rate < 0 || params$rate > 1) {
    stop("Missing rate must be between 0 and 1")
  }
  
  if (!("mechanism" %in% names(params))) {
    stop("Item missingness shadow requires 'mechanism' parameter")
  }
  
  valid_mechanisms <- c("MCAR", "MAR", "MNAR")
  if (!params$mechanism %in% valid_mechanisms) {
    stop("Invalid mechanism. Must be one of: ", paste(valid_mechanisms, collapse = ", "))
  }
  
  if (params$mechanism %in% c("MAR", "MNAR") && 
      (is.null(params$dependent_vars) || length(params$dependent_vars) == 0)) {
    stop("MAR and MNAR mechanisms require 'dependent_vars' parameter")
  }
  
  invisible(x)
}

#' Selection shadow validator
#' @export
#' @keywords internal
validate_shadow.selection_shadow <- function(x, ...) {
  params <- x$params
  
  if (!("selection_type" %in% names(params))) {
    stop("Selection shadow requires 'selection_type' parameter")
  }
  
  valid_types <- c("baseline", "post_treatment", "custom")
  if (!params$selection_type %in% valid_types) {
    stop("Invalid selection_type. Must be one of: ", paste(valid_types, collapse = ", "))
  }
  
  if (params$selection_type == "custom" && is.null(params$selection_prob_fn)) {
    stop("Custom selection requires 'selection_prob_fn' parameter")
  }
  
  invisible(x)
}

#' Positivity shadow validator
#' @export
#' @keywords internal
validate_shadow.positivity_shadow <- function(x, ...) {
  params <- x$params
  
  if (!("exposure_var" %in% names(params)) || length(params$exposure_var) == 0) {
    stop("Positivity shadow requires 'exposure_var' parameter")
  }
  
  if (!("filter_fn" %in% names(params)) || !is.function(params$filter_fn)) {
    stop("Positivity shadow requires 'filter_fn' parameter that is a function")
  }
  
  invisible(x)
}

#' Truncation shadow validator
#' @export
#' @keywords internal
validate_shadow.truncation_shadow <- function(x, ...) {
  params <- x$params
  
  if (!("variables" %in% names(params)) || length(params$variables) == 0) {
    stop("Truncation shadow requires 'variables' parameter")
  }
  
  if (!("lower" %in% names(params)) || !("upper" %in% names(params))) {
    stop("Truncation shadow requires 'lower' and 'upper' parameters")
  }
  
  if (params$lower >= params$upper) {
    stop("Lower bound must be less than upper bound")
  }
  
  invisible(x)
}

#' Coarsening shadow validator
#' @export
#' @keywords internal
validate_shadow.coarsening_shadow <- function(x, ...) {
  params <- x$params
  
  if (!("variables" %in% names(params)) || length(params$variables) == 0) {
    stop("Coarsening shadow requires 'variables' parameter")
  }
  
  if (!("type" %in% names(params))) {
    stop("Coarsening shadow requires 'type' parameter")
  }
  
  valid_types <- c("midpoint", "lower", "upper", "heaping")
  if (!params$type %in% valid_types) {
    stop("Invalid coarsening type. Must be one of: ", paste(valid_types, collapse = ", "))
  }
  
  if (params$type == "heaping" && 
      (is.null(params$heaping_digits) || is.null(params$heaping_prob))) {
    stop("Heaping coarsening requires 'heaping_digits' and 'heaping_prob' parameters")
  }
  
  invisible(x)
}

#' Mode effects shadow validator
#' @export
#' @keywords internal
validate_shadow.mode_effects_shadow <- function(x, ...) {
  params <- x$params
  
  if (!("variables" %in% names(params)) || length(params$variables) == 0) {
    stop("Mode effects shadow requires 'variables' parameter")
  }
  
  if (!("mode_var" %in% names(params)) || length(params$mode_var) == 0) {
    stop("Mode effects shadow requires 'mode_var' parameter")
  }
  
  if (!("effect_specs" %in% names(params)) || !is.list(params$effect_specs)) {
    stop("Mode effects shadow requires 'effect_specs' parameter as a list")
  }
  
  invisible(x)
}

#' Print method for shadow objects
#'
#' @param x A margot_shadow object
#' @param ... Additional arguments (unused)
#'
#' @return Invisibly returns the object
#' @export
print.margot_shadow <- function(x, ...) {
  cat("margot Shadow Object\n")
  cat("Type:", x$type, "\n")
  cat("Name:", x$name, "\n")
  cat("\nParameters:\n")
  
  # format parameters nicely
  param_names <- names(x$params)
  for (pname in param_names) {
    pval <- x$params[[pname]]
    
    # format based on type
    if (is.function(pval)) {
      cat("  ", pname, ": <function>\n", sep = "")
    } else if (is.null(pval)) {
      cat("  ", pname, ": NULL\n", sep = "")
    } else if (length(pval) == 1) {
      cat("  ", pname, ": ", pval, "\n", sep = "")
    } else if (is.character(pval)) {
      cat("  ", pname, ": ", paste(pval, collapse = ", "), "\n", sep = "")
    } else {
      cat("  ", pname, ": [", paste(head(pval, 3), collapse = ", "), 
          if(length(pval) > 3) "..." else "", "]\n", sep = "")
    }
  }
  
  invisible(x)
}

#' Test if object is a shadow
#'
#' @param x Object to test
#' @return Logical indicating if x is a margot_shadow
#' @export
is_shadow <- function(x) {
  inherits(x, "margot_shadow")
}

# Scenario S3 Methods -----------------------------------------------------

#' Constructor for margot_scenario objects
#'
#' @description
#' Low-level constructor for scenario objects. Users should typically use
#' create_scenario() instead.
#'
#' @param name Character string naming the scenario
#' @param shadows List of shadow objects
#' @param description Character description
#' @param justification Character justification
#' @param references Character vector of references
#' @param ... Additional attributes
#'
#' @return A margot_scenario object
#' @export
#' @keywords internal
new_scenario <- function(name, 
                        shadows = list(),
                        description = "",
                        justification = "",
                        references = NULL,
                        population = NULL,
                        ...) {
  # validate inputs
  stopifnot(is.character(name), length(name) == 1)
  stopifnot(is.list(shadows))
  
  # capture additional arguments
  extra_args <- list(...)
  
  # build scenario object
  scenario <- list(
    name = name,
    shadows = shadows,
    description = description,
    justification = justification,
    references = references,
    population = population,
    created = Sys.time(),
    n_shadows = length(shadows)
  )
  
  # add any extra arguments
  if (length(extra_args) > 0) {
    scenario <- c(scenario, extra_args)
  }
  
  structure(
    scenario,
    class = "margot_scenario"
  )
}

#' Validator for margot_scenario objects
#'
#' @param x An object to validate
#' @param ... Additional arguments
#'
#' @return The validated object (invisibly)
#' @export
validate_scenario <- function(x, ...) {
  UseMethod("validate_scenario")
}

#' Default scenario validator
#' @export
#' @keywords internal
validate_scenario.default <- function(x, ...) {
  stop("No validate_scenario method for class ", class(x)[1])
}

#' Base scenario validator
#' @export
#' @keywords internal
validate_scenario.margot_scenario <- function(x, ...) {
  # check core required fields only
  required_fields <- c("name", "shadows")
  if (!all(required_fields %in% names(unclass(x)))) {
    stop("Scenario object missing required fields")
  }
  
  # validate types
  if (!is.character(x$name) || length(x$name) != 1) {
    stop("Scenario name must be a single character string")
  }
  
  if (!is.list(x$shadows)) {
    stop("Scenario shadows must be a list")
  }
  
  # validate all shadows
  if (length(x$shadows) > 0) {
    shadow_valid <- sapply(x$shadows, is_shadow)
    if (!all(shadow_valid)) {
      stop("All elements in shadows list must be margot_shadow objects")
    }
    
    # validate each shadow
    for (i in seq_along(x$shadows)) {
      tryCatch(
        validate_shadow(x$shadows[[i]]),
        error = function(e) {
          stop("Invalid shadow at position ", i, ": ", e$message)
        }
      )
    }
  }
  
  invisible(x)
}

#' Test if object is a scenario
#'
#' @param x Object to test
#' @return Logical indicating if x is a margot_scenario
#' @export
is_scenario <- function(x) {
  inherits(x, "margot_scenario")
}

# Generic S3 Methods ------------------------------------------------------

#' Summary method for margot objects
#'
#' @param object A margot object
#' @param ... Additional arguments
#'
#' @return Summary information
#' @export
summary.margot_shadow <- function(object, ...) {
  cat("Shadow Summary\n")
  cat("--------------\n")
  cat("Type:", object$type, "\n")
  cat("Name:", object$name, "\n")
  
  # type-specific summaries
  if (object$type == "measurement_error") {
    cat("Error type:", object$params$error_type, "\n")
    cat("Variables affected:", paste(object$params$variables, collapse = ", "), "\n")
    
    if (object$params$error_type == "classical") {
      cat("Error SD:", object$params$sigma, "\n")
    } else if (object$params$error_type == "misclassification") {
      cat("Sensitivity:", object$params$sensitivity, "\n")
      cat("Specificity:", object$params$specificity, "\n")
    }
  } else if (object$type == "censoring") {
    cat("Censoring rate:", object$params$rate, "\n")
  }
  
  invisible(object)
}

#' Plot method for shadow objects
#'
#' @param x A margot_shadow object
#' @param data Optional data to show shadow effects on
#' @param ... Additional arguments
#'
#' @return A plot (base R or ggplot2)
#' @export
plot.margot_shadow <- function(x, data = NULL, ...) {
  # dispatch to specific plot methods
  UseMethod("plot_shadow_type", x)
}

#' Default plot for shadows
#' @keywords internal
plot_shadow_type <- function(x, ...) {
  UseMethod("plot_shadow_type")
}

#' Default shadow plot
#' @keywords internal
#' @export
plot_shadow_type.default <- function(x, ...) {
  cat("No specific plot method for", class(x)[1], "\n")
  invisible(NULL)
}

# Utility Functions -------------------------------------------------------

#' Convert old-style shadow to S3
#'
#' @description
#' Converts shadows created with the old interface to proper S3 objects
#'
#' @param old_shadow List-based shadow from old interface
#' @return A validated margot_shadow S3 object
#' @export
as_shadow <- function(old_shadow) {
  if (is_shadow(old_shadow)) {
    return(validate_shadow(old_shadow))
  }
  
  if (!is.list(old_shadow)) {
    stop("Input must be a list or shadow object")
  }
  
  # extract fields
  type <- old_shadow$type
  params <- old_shadow$params
  name <- old_shadow$name
  
  # create new shadow
  new_shadow <- new_shadow(type = type, params = params, name = name)
  
  # validate
  validate_shadow(new_shadow)
}

#' Convert old-style scenario to S3
#'
#' @description
#' Converts scenarios created with the old interface to proper S3 objects
#'
#' @param old_scenario List-based scenario from old interface
#' @return A validated margot_scenario S3 object
#' @export
as_scenario <- function(old_scenario) {
  if (is_scenario(old_scenario)) {
    return(validate_scenario(old_scenario))
  }
  
  if (!is.list(old_scenario)) {
    stop("Input must be a list or scenario object")
  }
  
  # convert shadows if needed
  if ("shadows" %in% names(old_scenario) && length(old_scenario$shadows) > 0) {
    old_scenario$shadows <- lapply(old_scenario$shadows, as_shadow)
  }
  
  # create new scenario
  scenario <- new_scenario(
    name = old_scenario$name,
    shadows = old_scenario$shadows,
    description = old_scenario$description %||% "",
    justification = old_scenario$justification %||% "",
    references = old_scenario$references
  )
  
  # validate
  validate_scenario(scenario)
}

# Helper function for NULL default
`%||%` <- function(x, y) if (is.null(x)) y else x

# Collection Methods ------------------------------------------------------

#' Combine multiple shadows into a list
#'
#' @param ... Shadow objects to combine
#' @return A list of validated shadow objects with class "shadow_list"
#' @export
c.margot_shadow <- function(...) {
  shadows <- list(...)
  
  # validate all are shadows
  if (!all(sapply(shadows, is_shadow))) {
    stop("All objects must be margot_shadow objects")
  }
  
  # validate each shadow
  shadows <- lapply(shadows, validate_shadow)
  
  structure(
    shadows,
    class = c("shadow_list", "list")
  )
}

#' Print method for shadow lists
#' @export
print.shadow_list <- function(x, ...) {
  n_shadows <- length(x)
  cat("Shadow List (", n_shadows, " shadow", 
      if(n_shadows != 1) "s" else "", ")\n", sep = "")
  cat("--------------------\n")
  
  for (i in seq_along(x)) {
    shadow <- x[[i]]
    cat(i, ". ", shadow$name, " (", shadow$type, ")\n", sep = "")
  }
  
  invisible(x)
}

#' Extract shadow from a list
#' @export
`[.shadow_list` <- function(x, i) {
  result <- NextMethod()
  if (length(result) == 1 && is_shadow(result[[1]])) {
    return(result[[1]])
  }
  class(result) <- c("shadow_list", "list")
  result
}

# Conversion Methods ------------------------------------------------------

#' Convert shadow to data frame for summary
#' 
#' @param x A margot_shadow object
#' @param ... Additional arguments
#' @return A data frame with shadow information
#' @export
as.data.frame.margot_shadow <- function(x, ...) {
  # basic info
  df <- data.frame(
    name = x$name,
    type = x$type,
    stringsAsFactors = FALSE
  )
  
  # add type-specific info
  if (x$type == "measurement_error") {
    df$error_type <- x$params$error_type
    df$n_variables <- length(x$params$variables)
    
    if (x$params$error_type == "classical") {
      df$sigma <- x$params$sigma
    } else if (x$params$error_type == "misclassification") {
      df$sensitivity <- x$params$sensitivity
      df$specificity <- x$params$specificity
    }
  } else if (x$type == "censoring") {
    df$rate <- x$params$rate
  } else if (x$type == "item_missingness") {
    df$mechanism <- x$params$mechanism
    df$rate <- x$params$rate
    df$n_variables <- length(x$params$variables)
  }
  
  df
}

#' Convert scenario to data frame for summary
#' 
#' @param x A margot_scenario object
#' @param ... Additional arguments
#' @return A data frame with scenario information
#' @export
as.data.frame.margot_scenario <- function(x, ...) {
  data.frame(
    name = x$name,
    n_shadows = x$n_shadows,
    description = substr(x$description, 1, 50),
    has_justification = nchar(x$justification) > 0,
    n_references = length(x$references),
    stringsAsFactors = FALSE
  )
}

# Utility Methods ---------------------------------------------------------

#' Get names of shadows in a scenario
#'
#' @param x A margot_scenario object
#' @return Character vector of shadow names
#' @export
names.margot_scenario <- function(x) {
  if (length(x$shadows) == 0) return(character(0))
  
  shadow_names <- names(x$shadows)
  if (is.null(shadow_names)) {
    shadow_names <- sapply(x$shadows, function(s) s$name)
  }
  shadow_names
}

#' Length method for scenarios
#' @export
length.margot_scenario <- function(x) {
  x$n_shadows
}

#' Subset shadows from a scenario
#' @export
`[.margot_scenario` <- function(x, i) {
  if (is.character(i)) {
    # subset by name
    shadow_names <- names.margot_scenario(x)
    i <- which(shadow_names %in% i)
  }
  
  x$shadows[i]
}