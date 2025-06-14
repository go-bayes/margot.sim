#' S3 Methods for margot.sim Objects
#'
#' @description
#' This file contains S3 generic methods and class definitions for the
#' margot.sim package, providing a formal object system for shadows,
#' scenarios, and simulation results.
#'
#' @details
#' The S3 system provides:
#' - Consistent object construction and validation
#' - Method dispatch for printing, plotting, and summarising
#' - Type safety and error checking
#' - Extensibility for future shadow types

# helper operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# shadow constructors and methods -------------------------------------

#' Create a margot shadow object
#'
#' @param type Character string specifying the shadow type
#' @param params List of parameters specific to the shadow type
#' @param ... Additional arguments passed to shadow-specific constructors
#'
#' @return An object of class "margot_shadow" and "{type}_shadow"
#' @export
#' @examples
#' # create a measurement error shadow
#' shadow <- new_shadow(
#'   type = "measurement_error",
#'   params = list(
#'     variables = "x",
#'     error_type = "classical",
#'     sigma = 0.5
#'   )
#' )
new_shadow <- function(type, params, ...) {
  # validate type
  valid_types <- c(
    "measurement_error", "missing_data", "censoring", "truncation",
    "selection", "misclassification", "confounding", "positivity",
    "mode_effects", "coarsening", "item_missingness"
  )
  
  if (!type %in% valid_types) {
    stop(
      "Invalid shadow type '", type, "'. ",
      "Valid types are: ", paste(valid_types, collapse = ", "),
      call. = FALSE
    )
  }
  
  # create base structure
  shadow <- structure(
    list(
      type = type,
      params = params,
      metadata = list(
        created = Sys.time(),
        version = utils::packageVersion("margot.sim")
      )
    ),
    class = c(paste0(type, "_shadow"), "margot_shadow")
  )
  
  # validate using method dispatch
  validate_shadow(shadow)
  
  shadow
}

#' Validate a shadow object
#'
#' @param x A shadow object to validate
#' @param ... Additional arguments for shadow-specific validation
#'
#' @return The validated shadow object (invisibly)
#' @export
validate_shadow <- function(x, ...) {
  UseMethod("validate_shadow")
}

#' @export
validate_shadow.default <- function(x, ...) {
  if (!inherits(x, "margot_shadow")) {
    stop("Object is not a margot_shadow", call. = FALSE)
  }
  
  if (!is.list(x$params)) {
    stop("Shadow params must be a list", call. = FALSE)
  }
  
  invisible(x)
}

#' @export
validate_shadow.measurement_error_shadow <- function(x, ...) {
  NextMethod()
  
  params <- x$params
  
  # check required parameters
  if (is.null(params$variables)) {
    stop("Measurement error shadow requires 'variables' parameter", call. = FALSE)
  }
  
  if (is.null(params$error_type)) {
    stop("Measurement error shadow requires 'error_type' parameter", call. = FALSE)
  }
  
  # validate error type
  valid_error_types <- c("classical", "berkson", "differential", "heteroscedastic", "misclassification")
  if (!params$error_type %in% valid_error_types) {
    stop(
      "Invalid error_type '", params$error_type, "'. ",
      "Valid types are: ", paste(valid_error_types, collapse = ", "),
      call. = FALSE
    )
  }
  
  # type-specific validation
  if (params$error_type %in% c("classical", "berkson", "heteroscedastic")) {
    if (is.null(params$sigma) || !is.numeric(params$sigma) || params$sigma < 0) {
      stop("Error type '", params$error_type, "' requires numeric sigma >= 0", call. = FALSE)
    }
  }
  
  if (params$error_type == "misclassification") {
    if (is.null(params$sensitivity) || is.null(params$specificity)) {
      stop("Misclassification requires 'sensitivity' and 'specificity' parameters", call. = FALSE)
    }
    if (params$sensitivity < 0 || params$sensitivity > 1 ||
        params$specificity < 0 || params$specificity > 1) {
      stop("Sensitivity and specificity must be between 0 and 1", call. = FALSE)
    }
  }
  
  invisible(x)
}

#' @export
validate_shadow.missing_data_shadow <- function(x, ...) {
  NextMethod()
  
  params <- x$params
  
  if (is.null(params$mechanism)) {
    stop("Missing data shadow requires 'mechanism' parameter", call. = FALSE)
  }
  
  valid_mechanisms <- c("MCAR", "MAR", "MNAR")
  if (!params$mechanism %in% valid_mechanisms) {
    stop(
      "Invalid mechanism '", params$mechanism, "'. ",
      "Valid mechanisms are: ", paste(valid_mechanisms, collapse = ", "),
      call. = FALSE
    )
  }
  
  if (is.null(params$prob) || !is.numeric(params$prob) || 
      params$prob < 0 || params$prob > 1) {
    stop("Missing data shadow requires 'prob' between 0 and 1", call. = FALSE)
  }
  
  invisible(x)
}

#' @export
validate_shadow.censoring_shadow <- function(x, ...) {
  NextMethod()
  
  params <- x$params
  
  # censoring can have various parameters - rate, side, mechanism, etc.
  if (!is.null(params$side)) {
    if (!params$side %in% c("left", "right", "interval")) {
      stop("Censoring side must be 'left', 'right', or 'interval'", call. = FALSE)
    }
  }
  
  # check for rate if provided
  if (!is.null(params$rate)) {
    if (!is.numeric(params$rate) || params$rate < 0 || params$rate > 1) {
      stop("Censoring rate must be between 0 and 1", call. = FALSE)
    }
  }
  
  invisible(x)
}

#' @export
validate_shadow.truncation_shadow <- function(x, ...) {
  NextMethod()
  
  params <- x$params
  
  if (is.null(params$lower) && is.null(params$upper)) {
    stop("Truncation shadow requires at least one of 'lower' or 'upper' bounds", call. = FALSE)
  }
  
  if (!is.null(params$lower) && !is.null(params$upper)) {
    if (params$lower >= params$upper) {
      stop("Truncation lower bound must be less than upper bound", call. = FALSE)
    }
  }
  
  invisible(x)
}

#' @export
validate_shadow.positivity_shadow <- function(x, ...) {
  NextMethod()
  
  params <- x$params
  
  if (is.null(params$exposure_var)) {
    stop("Positivity shadow requires 'exposure_var' parameter", call. = FALSE)
  }
  
  if (is.null(params$filter_fn) || !is.function(params$filter_fn)) {
    stop("Positivity shadow requires 'filter_fn' to be a function", call. = FALSE)
  }
  
  invisible(x)
}

#' @export
validate_shadow.selection_shadow <- function(x, ...) {
  NextMethod()
  
  params <- x$params
  
  # accept either retention_model or selection_type for compatibility
  if (is.null(params$retention_model) && is.null(params$selection_type)) {
    stop("Selection shadow requires either 'retention_model' or 'selection_type' parameter", call. = FALSE)
  }
  
  invisible(x)
}

#' @export
validate_shadow.mode_effects_shadow <- function(x, ...) {
  NextMethod()
  
  params <- x$params
  
  if (is.null(params$reference_mode)) {
    stop("Mode effects shadow requires 'reference_mode' parameter", call. = FALSE)
  }
  
  if (is.null(params$effect_size) || !is.numeric(params$effect_size)) {
    stop("Mode effects shadow requires numeric 'effect_size' parameter", call. = FALSE)
  }
  
  invisible(x)
}

#' @export
validate_shadow.coarsening_shadow <- function(x, ...) {
  NextMethod()
  
  params <- x$params
  
  if (is.null(params$coarsen_fn) || !is.function(params$coarsen_fn)) {
    stop("Coarsening shadow requires 'coarsen_fn' to be a function", call. = FALSE)
  }
  
  invisible(x)
}

#' @export
validate_shadow.item_missingness_shadow <- function(x, ...) {
  NextMethod()
  
  params <- x$params
  
  if (is.null(params$variables)) {
    stop("Item missingness shadow requires 'variables' parameter", call. = FALSE)
  }
  
  # accept either missing_rate or rate for compatibility
  rate_param <- params$missing_rate %||% params$rate
  
  if (is.null(rate_param) || !is.numeric(rate_param) ||
      rate_param < 0 || rate_param > 1) {
    stop("Item missingness shadow requires 'missing_rate' or 'rate' between 0 and 1", call. = FALSE)
  }
  
  invisible(x)
}

#' Print method for shadow objects
#'
#' @param x A margot_shadow object
#' @param ... Additional arguments (unused)
#'
#' @return The object invisibly
#' @export
print.margot_shadow <- function(x, ...) {
  cat("<margot_shadow>\n")
  cat("Type:", x$type, "\n")
  cat("Parameters:\n")
  
  # format parameters nicely
  params <- x$params
  param_names <- names(params)
  
  for (name in param_names) {
    value <- params[[name]]
    
    if (is.function(value)) {
      cat("  ", name, ": <function>\n", sep = "")
    } else if (is.character(value) && length(value) > 3) {
      cat("  ", name, ": ", paste0(head(value, 3), collapse = ", "), ", ...\n", sep = "")
    } else if (is.numeric(value) && length(value) > 5) {
      cat("  ", name, ": ", paste0(head(value, 5), collapse = ", "), ", ...\n", sep = "")
    } else {
      cat("  ", name, ": ", paste0(value, collapse = ", "), "\n", sep = "")
    }
  }
  
  invisible(x)
}

#' Summary method for shadow objects
#'
#' @param object A margot_shadow object
#' @param ... Additional arguments (unused)
#'
#' @return A summary list
#' @export
summary.margot_shadow <- function(object, ...) {
  structure(
    list(
      type = object$type,
      n_params = length(object$params),
      param_names = names(object$params),
      created = object$metadata$created,
      version = object$metadata$version
    ),
    class = "summary.margot_shadow"
  )
}

#' @export
print.summary.margot_shadow <- function(x, ...) {
  cat("Shadow Type:", x$type, "\n")
  cat("Number of Parameters:", x$n_params, "\n")
  cat("Parameters:", paste(x$param_names, collapse = ", "), "\n")
  cat("Created:", format(x$created), "\n")
  cat("Package Version:", as.character(x$version), "\n")
  invisible(x)
}

# scenario constructors and methods -----------------------------------

#' Create a margot scenario object
#'
#' @param name Character string naming the scenario
#' @param description Character string describing the scenario
#' @param shadows List of shadow objects or a single shadow
#' @param ... Additional metadata
#'
#' @return An object of class "margot_scenario"
#' @export
#' @examples
#' shadow1 <- new_shadow(
#'   type = "measurement_error",
#'   params = list(variables = "x", error_type = "classical", sigma = 0.5)
#' )
#' 
#' scenario <- new_scenario(
#'   name = "Measurement Error Study",
#'   description = "Examining impact of measurement error on estimates",
#'   shadows = shadow1
#' )
new_scenario <- function(name, description = "", shadows = list(), ...) {
  # ensure shadows is a list
  if (inherits(shadows, "margot_shadow")) {
    shadows <- list(shadows)
  }
  
  # validate all shadows
  if (!is.list(shadows)) {
    stop("Shadows must be a list of shadow objects", call. = FALSE)
  }
  
  for (i in seq_along(shadows)) {
    if (!inherits(shadows[[i]], "margot_shadow")) {
      stop("Element ", i, " of shadows is not a margot_shadow object", call. = FALSE)
    }
  }
  
  # create scenario
  scenario <- structure(
    list(
      name = name,
      description = description,
      shadows = shadows,
      n_shadows = length(shadows),
      metadata = list(
        created = Sys.time(),
        version = utils::packageVersion("margot.sim")
      ),
      ...  # additional fields like justification, references, population
    ),
    class = "margot_scenario"
  )
  
  validate_scenario(scenario)
  
  scenario
}

#' Validate a scenario object
#'
#' @param x A scenario object to validate
#' @param ... Additional arguments
#'
#' @return The validated scenario object (invisibly)
#' @export
validate_scenario <- function(x, ...) {
  UseMethod("validate_scenario")
}

#' @export
validate_scenario.default <- function(x, ...) {
  stop("validate_scenario() requires a margot_scenario object", call. = FALSE)
}

#' @export
validate_scenario.margot_scenario <- function(x, ...) {
  if (!is.character(x$name) || length(x$name) != 1) {
    stop("Scenario name must be a single character string", call. = FALSE)
  }
  
  if (!is.character(x$description)) {
    stop("Scenario description must be a character string", call. = FALSE)
  }
  
  if (!is.list(x$shadows)) {
    stop("Scenario shadows must be a list", call. = FALSE)
  }
  
  # validate each shadow
  for (shadow in x$shadows) {
    validate_shadow(shadow)
  }
  
  invisible(x)
}

# print.margot_scenario is defined in margot-scenarios.R for richer output

#' Combine scenarios
#'
#' @param ... margot_scenario objects to combine
#'
#' @return A new margot_scenario with combined shadows
#' @export
c.margot_scenario <- function(...) {
  scenarios <- list(...)
  
  # validate all are scenarios
  for (i in seq_along(scenarios)) {
    if (!inherits(scenarios[[i]], "margot_scenario")) {
      stop("All arguments must be margot_scenario objects", call. = FALSE)
    }
  }
  
  # combine shadows
  all_shadows <- unlist(lapply(scenarios, function(s) s$shadows), recursive = FALSE)
  
  # create combined name and description
  names <- vapply(scenarios, function(s) s$name, character(1))
  combined_name <- paste(names, collapse = " + ")
  
  descriptions <- vapply(scenarios, function(s) s$description, character(1))
  descriptions <- descriptions[nchar(descriptions) > 0]
  combined_desc <- if (length(descriptions) > 0) {
    paste(descriptions, collapse = "; ")
  } else {
    ""
  }
  
  new_scenario(
    name = combined_name,
    description = combined_desc,
    shadows = all_shadows
  )
}

#' Extract shadows from a scenario
#'
#' @param x A margot_scenario object
#' @param i Index or indices of shadows to extract
#' @param ... Additional arguments (unused)
#'
#' @return A shadow object or list of shadow objects
#' @export
`[.margot_scenario` <- function(x, i, ...) {
  shadows <- x$shadows[i]
  
  if (length(shadows) == 1) {
    return(shadows[[1]])
  }
  
  # return a new scenario with subset of shadows
  new_scenario(
    name = paste0(x$name, " [subset]"),
    description = x$description,
    shadows = shadows
  )
}

#' Get the number of shadows in a scenario
#'
#' @param x A margot_scenario object
#'
#' @return Integer number of shadows
#' @export
length.margot_scenario <- function(x) {
  length(x$shadows)
}

#' Get names of shadows in a scenario
#'
#' @param x A margot_scenario object
#'
#' @return Character vector of shadow types
#' @export
names.margot_scenario <- function(x) {
  vapply(x$shadows, function(s) s$type, character(1))
}

# shadow list class for collections -----------------------------------

#' Create a shadow list
#'
#' @param ... Shadow objects or lists of shadow objects
#'
#' @return An object of class "shadow_list"
#' @export
new_shadow_list <- function(...) {
  shadows <- list(...)
  
  # flatten if nested lists provided
  if (length(shadows) == 1 && is.list(shadows[[1]])) {
    shadows <- shadows[[1]]
  }
  
  # validate all are shadows
  for (i in seq_along(shadows)) {
    if (!inherits(shadows[[i]], "margot_shadow")) {
      stop("Element ", i, " is not a margot_shadow object", call. = FALSE)
    }
  }
  
  structure(shadows, class = c("shadow_list", "list"))
}

#' Print method for shadow lists
#'
#' @param x A shadow_list object
#' @param ... Additional arguments (unused)
#'
#' @return The object invisibly
#' @export
print.shadow_list <- function(x, ...) {
  n <- length(x)
  cat("<shadow_list> with", n, "shadow", if (n != 1) "s", "\n")
  
  if (n > 0) {
    shadow_types <- vapply(x, function(s) s$type, character(1))
    type_counts <- table(shadow_types)
    
    cat("Types:\n")
    for (type in names(type_counts)) {
      count <- type_counts[[type]]
      cat("  ", type, ": ", count, "\n", sep = "")
    }
  }
  
  invisible(x)
}

#' Combine shadow lists
#'
#' @param ... shadow_list objects or individual shadows
#'
#' @return A new shadow_list
#' @export
c.shadow_list <- function(...) {
  items <- list(...)
  
  # extract all shadows
  all_shadows <- list()
  
  for (item in items) {
    if (inherits(item, "shadow_list")) {
      all_shadows <- c(all_shadows, unclass(item))
    } else if (inherits(item, "margot_shadow")) {
      all_shadows <- c(all_shadows, list(item))
    } else {
      stop("Can only combine shadow_list and margot_shadow objects", call. = FALSE)
    }
  }
  
  new_shadow_list(all_shadows)
}

#' Extract from shadow list
#'
#' @param x A shadow_list object
#' @param i Indices to extract
#' @param ... Additional arguments (unused)
#'
#' @return A shadow or shadow_list
#' @export
`[.shadow_list` <- function(x, i, ...) {
  result <- NextMethod()
  
  if (length(result) == 1 && is.list(result)) {
    return(result[[1]])
  }
  
  new_shadow_list(result)
}

# helper functions ----------------------------------------------------

#' Check if object is a shadow
#'
#' @param x Object to test
#'
#' @return Logical
#' @export
is_shadow <- function(x) {
  inherits(x, "margot_shadow")
}

#' Check if object is a scenario
#'
#' @param x Object to test
#'
#' @return Logical
#' @export
is_scenario <- function(x) {
  inherits(x, "margot_scenario")
}

#' Convert objects to shadows
#'
#' @param x Object to convert
#' @param ... Additional arguments passed to methods
#'
#' @return A margot_shadow object
#' @export
as_shadow <- function(x, ...) {
  UseMethod("as_shadow")
}

#' @export
as_shadow.margot_shadow <- function(x, ...) {
  x
}

#' @export
as_shadow.list <- function(x, ...) {
  if (is.null(x$type)) {
    stop("List must have a 'type' element to convert to shadow", call. = FALSE)
  }
  
  if (is.null(x$params)) {
    stop("List must have a 'params' element to convert to shadow", call. = FALSE)
  }
  
  new_shadow(type = x$type, params = x$params, ...)
}

#' Convert objects to scenarios
#'
#' @param x Object to convert
#' @param ... Additional arguments passed to methods
#'
#' @return A margot_scenario object
#' @export
as_scenario <- function(x, ...) {
  UseMethod("as_scenario")
}

#' @export
as_scenario.margot_scenario <- function(x, ...) {
  x
}

#' @export
as_scenario.margot_shadow <- function(x, name = "Unnamed scenario", ...) {
  new_scenario(name = name, shadows = list(x), ...)
}

#' @export
as_scenario.list <- function(x, name = "Unnamed scenario", ...) {
  # if it looks like a scenario structure
  if (!is.null(x$name) && !is.null(x$shadows)) {
    return(new_scenario(
      name = x$name,
      description = x$description %||% "",
      shadows = x$shadows
    ))
  }
  
  # otherwise assume it's a list of shadows
  new_scenario(name = name, shadows = x, ...)
}

# plotting methods ----------------------------------------------------

#' Plot shadow distributions
#'
#' @param x A margot_shadow object
#' @param data Optional data to show impact on
#' @param ... Additional arguments passed to plotting functions
#'
#' @return A ggplot2 object
#' @export
plot.margot_shadow <- function(x, data = NULL, ...) {
  # dispatch to shadow-specific plotting
  plot_shadow_type(x, data, ...)
}

#' Generic shadow plotting function
#'
#' @param x A shadow object
#' @param data Optional data
#' @param ... Additional arguments
#'
#' @return A plot object
#' @export
plot_shadow_type <- function(x, data = NULL, ...) {
  UseMethod("plot_shadow_type")
}

#' @export
plot_shadow_type.default <- function(x, data = NULL, ...) {
  message("No specific plot method for shadow type '", x$type, "'")
  invisible(NULL)
}

# conversion methods for data frames ----------------------------------

#' Convert shadow to data frame
#'
#' @param x A margot_shadow object
#' @param ... Additional arguments (unused)
#'
#' @return A data frame representation
#' @export
as.data.frame.margot_shadow <- function(x, ...) {
  params_df <- as.data.frame(x$params, stringsAsFactors = FALSE)
  
  data.frame(
    type = x$type,
    params_df,
    stringsAsFactors = FALSE
  )
}

#' Convert scenario to data frame
#'
#' @param x A margot_scenario object
#' @param ... Additional arguments (unused)
#'
#' @return A data frame with one row per shadow
#' @export
as.data.frame.margot_scenario <- function(x, ...) {
  if (length(x$shadows) == 0) {
    return(data.frame(
      scenario = character(),
      shadow_type = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  shadow_dfs <- lapply(seq_along(x$shadows), function(i) {
    shadow_df <- as.data.frame(x$shadows[[i]])
    shadow_df$scenario <- x$name
    shadow_df$shadow_index <- i
    shadow_df
  })
  
  do.call(rbind, shadow_dfs)
}