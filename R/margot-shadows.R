#' Shadowing Framework for margot
#'
#' @description
#' This file implements a unified shadowing framework for applying various
#' observational distortions to simulated data, including censoring,
#' measurement error, and selection bias.

# Shadow Creation ----------------------------------------------------------------

#' Create a shadow object
#'
#' @param type Character string specifying shadow type: "censoring", "measurement_error", or "selection"
#' @param params List of parameters specific to the shadow type. For measurement_error type:
#'   - variables: Character vector of variable names to apply error to
#'   - error_type: "classical", "differential", "dichotomise", "correlated", or "misclassification"
#'   - For "classical": sigma (numeric) for error standard deviation
#'   - For "misclassification": sensitivity and specificity (both numeric 0-1)
#' @param name Optional name for the shadow
#'
#' @return A shadow object with class attributes for method dispatch
#' @export
#'
#' @examples
#' # Create a classical measurement error shadow
#' shadow <- create_shadow(
#'   type = "measurement_error",
#'   params = list(
#'     variables = c("t1_l", "t2_l"),
#'     error_type = "classical",
#'     sigma = 0.5
#'   )
#' )
#' 
#' # Create a misclassification shadow for binary variables
#' misclass_shadow <- create_shadow(
#'   type = "measurement_error",
#'   params = list(
#'     variables = "t1_a",
#'     error_type = "misclassification",
#'     sensitivity = 0.85,  # 85% of true positives correctly classified
#'     specificity = 0.90   # 90% of true negatives correctly classified
#'   )
#' )
create_shadow <- function(type = c("censoring", "measurement_error", "selection", "item_missingness", "positivity"),
                        params = list(),
                        name = NULL) {
  type <- match.arg(type)

  # Validate params based on type
  params <- validate_shadow_params(type, params)

  structure(
    list(
      type = type,
      params = params,
      name = name %||% paste0(type, "_shadow")
    ),
    class = c(paste0(type, "_shadow"), "margot_shadow")
  )
}

#' Extract time index from variable name
#' @keywords internal
get_time_index <- function(var_name) {
  # Extract time from patterns like t0_x, t1_y, etc.
  time_match <- regmatches(var_name, regexpr("^t[0-9]+", var_name))
  if (length(time_match) > 0) {
    return(as.numeric(sub("^t", "", time_match)))
  }
  return(NA)
}

#' Validate temporal order of shadow dependencies
#' @keywords internal
validate_temporal_order <- function(target_vars, dependency_vars) {
  # Check that dependencies occur before or at same time as targets
  for (target in target_vars) {
    target_time <- get_time_index(target)
    if (is.na(target_time)) next
    
    for (dep in dependency_vars) {
      dep_time <- get_time_index(dep)
      if (is.na(dep_time)) next
      
      if (dep_time > target_time) {
        stop(sprintf(
          "Temporal order violation: %s (time %d) cannot depend on future variable %s (time %d)",
          target, target_time, dep, dep_time
        ))
      }
    }
  }
  invisible(TRUE)
}

#' Validate shadow parameters
#' @keywords internal
validate_shadow_params <- function(type, params) {
  switch(type,
         censoring = {
           # Use existing censoring parameters
           defaults <- list(
             rate = 0.1,
             exposure_dependence = FALSE,
             l_dependence = FALSE,
             y_dependence = FALSE,
             latent_dependence = FALSE
           )
         },
         measurement_error = {
           defaults <- list(
             variables = character(),
             error_type = "classical",
             sigma = 1,
             rho = 0,  # correlation between errors
             differential_var = NULL,
             differential_fn = NULL,
             # misclassification parameters
             sensitivity = 0.9,  # P(observed = 1 | true = 1)
             specificity = 0.9   # P(observed = 0 | true = 0)
           )
         },
         selection = {
           defaults <- list(
             selection_type = "baseline",
             selection_prob_fn = NULL,
             selection_vars = character()
           )
         },
         item_missingness = {
           defaults <- list(
             variables = character(),
             rate = 0.1,
             mechanism = "MCAR",
             dependent_vars = NULL
           )
         },
         positivity = {
           defaults <- list(
             exposure_var = character(),
             filter_fn = NULL
           )
         }
  )

  # Merge with defaults
  params <- modifyList(defaults, params)
  
  # Validate temporal order for relevant shadow types
  if (type == "measurement_error" && !is.null(params$differential_var)) {
    validate_temporal_order(params$variables, params$differential_var)
  }
  
  if (type == "item_missingness" && params$mechanism != "MCAR" && !is.null(params$dependent_vars)) {
    validate_temporal_order(params$variables, params$dependent_vars)
  }
  
  if (type == "selection" && !is.null(params$selection_vars)) {
    # Selection typically happens at baseline or depends on past
    # No strict validation needed as selection is usually cross-sectional
  }
  
  params
}

# Generic Apply Function -------------------------------------------------------

#' Apply a shadow to data
#'
#' @param data Data frame to apply shadow to
#' @param shadow A shadow object created with create_shadow()
#' @param preserve_truth Logical, whether to preserve original values (default FALSE)
#' @param ... Additional arguments passed to specific shadow methods
#'
#' @return Modified data frame
#' @export
apply_shadow <- function(data, shadow, preserve_truth = FALSE, ...) {
  UseMethod("apply_shadow", shadow)
}

#' Apply multiple shadows sequentially
#'
#' @param data Data frame
#' @param shadows List of shadow objects
#' @param verbose Logical, print progress?
#' @param preserve_truth Logical, whether to preserve original values (default FALSE)
#'
#' @return Data frame with all shadows applied
#' @export
apply_shadows <- function(data, shadows, verbose = FALSE, preserve_truth = FALSE) {
  if (!is.list(shadows)) shadows <- list(shadows)

  for (i in seq_along(shadows)) {
    shadow <- shadows[[i]]
    
    # debugging: check what we're dealing with
    if (!inherits(shadow, "margot_shadow")) {
      stop(sprintf("Expected margot_shadow object at position %d, got %s", 
                   i, paste(class(shadow), collapse = ", ")))
    }
    
    if (verbose) {
      cat(sprintf("Applying shadow %d/%d: %s\n",
                  i, length(shadows), shadow$name))
    }
    data <- apply_shadow(data, shadow, preserve_truth = preserve_truth)
  }

  # Add metadata about applied shadows
  attr(data, "applied_shadows") <- lapply(shadows, function(m) m$name)
  data
}

# Censoring Shadows --------------------------------------------------------------

#' Apply censoring shadow
#'
#' @param data Data frame to apply shadow to
#' @param shadow A censoring shadow object created with create_shadow()
#' @param ... Additional arguments passed to apply_censoring_post_hoc
#'
#' @return Modified data frame with censoring applied
#' @export
apply_shadow.censoring_shadow <- function(data, shadow, ...) {
  # This wraps the existing apply_censoring_post_hoc functionality
  # but with the unified interface

  params <- shadow$params

  # For now, use the existing censoring logic
  # In future, could refactor apply_censoring_post_hoc to use this interface
  apply_censoring_post_hoc(
    data,
    censoring_type = "custom",
    censoring_function = function(data, ...) {
      # Implement custom censoring based on params
      # This is a placeholder - would integrate with existing code
      data
    }
  )
}

# Measurement Error Shadows ------------------------------------------------------

#' Apply measurement error shadow
#'
#' @param data Data frame to apply shadow to
#' @param shadow A measurement error shadow object created with create_shadow()
#' @param ... Additional arguments (currently unused)
#'
#' @return Modified data frame with measurement error applied
#' @export
apply_shadow.measurement_error_shadow <- function(data, shadow, ...) {
  params <- shadow$params
  n <- nrow(data)

  # Ensure variables exist
  missing_vars <- setdiff(params$variables, names(data))
  if (length(missing_vars) > 0) {
    stop("Variables not found in data: ", paste(missing_vars, collapse = ", "))
  }

  if (params$error_type == "classical") {
    # Classical measurement error: X_obs = X_true + epsilon
    apply_classical_error(data, params)

  } else if (params$error_type == "differential") {
    # Differential error: error depends on other variables
    apply_differential_error(data, params)

  } else if (params$error_type == "dichotomise") {
    # Dichotomisation at threshold
    apply_dichotomisation(data, params)

  } else if (params$error_type == "correlated") {
    # Correlated errors across variables
    apply_correlated_error(data, params)
    
  } else if (params$error_type == "misclassification") {
    # Misclassification for binary variables
    apply_misclassification(data, params)

  } else {
    stop("Unknown error_type: ", params$error_type)
  }
}

#' Apply classical measurement error
#' @keywords internal
apply_classical_error <- function(data, params) {
  for (var in params$variables) {
    # Store original values
    data[[paste0(var, "_true")]] <- data[[var]]

    # Add error
    data[[var]] <- data[[var]] + rnorm(nrow(data), 0, params$sigma)
  }
  data
}

#' Apply differential measurement error
#' @keywords internal
apply_differential_error <- function(data, params) {
  if (is.null(params$differential_var) || is.null(params$differential_fn)) {
    stop("differential_var and differential_fn required for differential error")
  }

  for (var in params$variables) {
    # Store original
    data[[paste0(var, "_true")]] <- data[[var]]

    # Error variance depends on another variable
    modifier_values <- data[[params$differential_var]]
    error_sd <- params$differential_fn(modifier_values)

    # Add differential error
    data[[var]] <- data[[var]] + rnorm(nrow(data), 0, error_sd)
  }
  data
}

#' Apply dichotomisation
#' @keywords internal
apply_dichotomisation <- function(data, params) {
  if (is.null(params$threshold)) {
    stop("threshold required for dichotomisation")
  }

  for (var in params$variables) {
    # Store original
    data[[paste0(var, "_true")]] <- data[[var]]

    # Dichotomise
    if (is.function(params$threshold)) {
      threshold <- params$threshold(data[[var]])
    } else {
      threshold <- params$threshold
    }

    data[[var]] <- as.numeric(data[[var]] > threshold)
  }
  data
}

#' Apply correlated measurement error
#' @keywords internal
apply_correlated_error <- function(data, params) {
  n <- nrow(data)
  n_vars <- length(params$variables)

  # Create correlation matrix
  if (length(params$rho) == 1) {
    # Single correlation for all pairs
    cor_matrix <- matrix(params$rho, n_vars, n_vars)
    diag(cor_matrix) <- 1
  } else if (is.matrix(params$rho)) {
    cor_matrix <- params$rho
  } else {
    stop("rho must be a scalar or correlation matrix")
  }

  # Create covariance matrix
  if (length(params$sigma) == 1) {
    sigmas <- rep(params$sigma, n_vars)
  } else {
    sigmas <- params$sigma
  }

  cov_matrix <- cor_matrix * outer(sigmas, sigmas)

  # Generate correlated errors
  errors <- MASS::mvrnorm(n, rep(0, n_vars), cov_matrix)

  # Apply errors
  for (i in seq_along(params$variables)) {
    var <- params$variables[i]
    data[[paste0(var, "_true")]] <- data[[var]]
    data[[var]] <- data[[var]] + errors[, i]
  }

  data
}

#' Apply misclassification to binary variables
#' @keywords internal
apply_misclassification <- function(data, params) {
  n <- nrow(data)
  
  for (var in params$variables) {
    # Store original values
    data[[paste0(var, "_true")]] <- data[[var]]
    
    # Check if variable is binary
    unique_vals <- unique(data[[var]][!is.na(data[[var]])])
    if (!all(unique_vals %in% c(0, 1))) {
      warning(paste("Variable", var, "is not binary. Skipping misclassification."))
      next
    }
    
    # Apply misclassification based on sensitivity and specificity
    true_vals <- data[[var]]
    observed_vals <- true_vals
    
    # For true positives (true = 1), apply sensitivity
    true_pos_idx <- which(true_vals == 1)
    if (length(true_pos_idx) > 0) {
      # With probability (1 - sensitivity), misclassify as 0
      misclass_pos <- rbinom(length(true_pos_idx), 1, 1 - params$sensitivity)
      observed_vals[true_pos_idx[misclass_pos == 1]] <- 0
    }
    
    # For true negatives (true = 0), apply specificity
    true_neg_idx <- which(true_vals == 0)
    if (length(true_neg_idx) > 0) {
      # With probability (1 - specificity), misclassify as 1
      misclass_neg <- rbinom(length(true_neg_idx), 1, 1 - params$specificity)
      observed_vals[true_neg_idx[misclass_neg == 1]] <- 1
    }
    
    data[[var]] <- observed_vals
  }
  
  data
}

# Selection Bias Shadows ---------------------------------------------------------

#' Apply selection bias shadow
#'
#' @param data Data frame to apply shadow to
#' @param shadow A selection shadow object created with create_shadow()
#' @param ... Additional arguments (currently unused)
#'
#' @return Subset of data frame with selection bias applied
#' @export
apply_shadow.selection_shadow <- function(data, shadow, ...) {
  params <- shadow$params
  n <- nrow(data)

  if (params$selection_type == "baseline") {
    # Selection based on baseline covariates
    apply_baseline_selection(data, params)

  } else if (params$selection_type == "post_treatment") {
    # Post-treatment selection (collider bias)
    apply_post_treatment_selection(data, params)

  } else if (params$selection_type == "custom") {
    # Custom selection function
    if (is.null(params$selection_prob_fn)) {
      stop("selection_prob_fn required for custom selection")
    }

    # Calculate selection probabilities
    selection_probs <- params$selection_prob_fn(data)

    # Select observations
    selected <- rbinom(n, 1, selection_probs) == 1
    data[selected, ]

  } else {
    stop("Unknown selection_type: ", params$selection_type)
  }
}

#' Apply baseline selection
#' @keywords internal
apply_baseline_selection <- function(data, params) {
  # Example: select based on baseline covariates
  # Higher values of b1 more likely to be selected
  if (length(params$selection_vars) == 0) {
    params$selection_vars <- "b1"
  }

  # Linear predictor for selection
  lp <- 0
  for (var in params$selection_vars) {
    if (var %in% names(data)) {
      lp <- lp + data[[var]] * params[[paste0(var, "_coef")]]
    }
  }

  # Selection probability
  selection_prob <- plogis(lp)

  # Apply selection
  selected <- rbinom(nrow(data), 1, selection_prob) == 1
  data[selected, ]
}

# Shadow Combination Utilities ---------------------------------------------------

#' Create a complex shadow combining multiple distortions
#'
#' @param ... Individual shadow objects to combine
#' @param name Name for the combined shadow
#'
#' @return A composite shadow object
#' @export
combine_shadows <- function(..., name = "combined_shadow") {
  shadows <- list(...)

  structure(
    list(
      type = "combined",
      shadows = shadows,
      name = name
    ),
    class = c("combined_shadow", "margot_shadow")
  )
}

#' Apply combined shadow
#'
#' @param data Data frame to apply shadow to
#' @param shadow A combined shadow object created with combine_shadows()
#' @param ... Additional arguments passed to apply_shadows
#'
#' @return Modified data frame with all component shadows applied
#' @export
apply_shadow.combined_shadow <- function(data, shadow, ...) {
  apply_shadows(data, shadow$shadows, ...)
}

# Shadow Effect Analysis ---------------------------------------------------------

#' Compare data before and after shadowing
#'
#' @param original Original data
#' @param shadowed Shadowed data
#' @param variables Variables to compare
#'
#' @return Summary of shadow effects
#' @export
analyse_shadow_effects <- function(original, shadowed, variables = NULL) {
  if (is.null(variables)) {
    # Find variables that exist in both
    variables <- intersect(names(original), names(shadowed))
    # Exclude ID and metadata variables
    variables <- grep("^(id|.*_true|.*not_lost.*)$", variables,
                      value = TRUE, invert = TRUE)
  }

  results <- list()

  for (var in variables) {
    if (var %in% names(original) && var %in% names(shadowed)) {
      orig_vals <- original[[var]]
      shadow_vals <- shadowed[[var]]

      # Calculate statistics based on variable type
      if (is.numeric(orig_vals)) {
        results[[var]] <- list(
          n_original = sum(!is.na(orig_vals)),
          n_shadowed = sum(!is.na(shadow_vals)),
          prop_missing = mean(is.na(shadow_vals)) - mean(is.na(orig_vals)),
          mean_shift = mean(shadow_vals, na.rm = TRUE) - mean(orig_vals, na.rm = TRUE),
          sd_ratio = sd(shadow_vals, na.rm = TRUE) / sd(orig_vals, na.rm = TRUE)
        )
      }
    }
  }

  class(results) <- "shadow_effects"
  results
}

#' Print shadow effects
#'
#' @param x A shadow_effects object from analyse_shadow_effects()
#' @param ... Additional arguments (currently unused)
#'
#' @return Invisibly returns the original object
#' @export
print.shadow_effects <- function(x, ...) {
  cat("Shadow Effects Analysis\n")
  cat("====================\n\n")

  for (var in names(x)) {
    cat(sprintf("Variable: %s\n", var))
    cat(sprintf("  Missing introduced: %.1f%%\n", x[[var]]$prop_missing * 100))
    cat(sprintf("  Mean shift: %.3f\n", x[[var]]$mean_shift))
    cat(sprintf("  SD ratio: %.3f\n", x[[var]]$sd_ratio))
    cat("\n")
  }
}

# Item-level missingness shadows ------------------------------------------------

#' Create item-level missingness shadow
#'
#' @param variables Character vector of variables to apply missingness to
#' @param missing_rate Numeric. Overall missing rate (0-1)
#' @param missing_mechanism Character. One of "MCAR", "MAR", "MNAR"
#' @param dependent_vars Character vector. Variables that influence missingness (for MAR/MNAR)
#' @param name Optional name for the shadow
#'
#' @return Item missingness shadow object
#' @export
create_item_missingness_shadow <- function(
    variables,
    missing_rate,
    missing_mechanism = c("MCAR", "MAR", "MNAR"),
    dependent_vars = NULL,
    name = "item_missingness") {
  
  missing_mechanism <- match.arg(missing_mechanism)
  
  if (missing_rate < 0 || missing_rate > 1) {
    stop("missing_rate must be between 0 and 1")
  }
  
  if (missing_mechanism %in% c("MAR", "MNAR") && is.null(dependent_vars)) {
    stop("dependent_vars required for MAR and MNAR mechanisms")
  }
  
  create_shadow(
    type = "item_missingness",
    params = list(
      variables = variables,
      rate = missing_rate,
      mechanism = missing_mechanism,
      dependent_vars = dependent_vars
    ),
    name = name
  )
}

#' Apply item-level missingness shadow
#'
#' @param data Data frame to apply shadow to
#' @param shadow An item missingness shadow object created with create_item_missingness_shadow()
#' @param ... Additional arguments (currently unused)
#'
#' @return Modified data frame with missing values introduced
#' @export
apply_shadow.item_missingness_shadow <- function(data, shadow, ...) {
  params <- shadow$params
  n <- nrow(data)
  
  # ensure variables exist
  missing_vars <- setdiff(params$variables, names(data))
  if (length(missing_vars) > 0) {
    stop("variables not found in data: ", paste(missing_vars, collapse = ", "))
  }
  
  if (params$mechanism == "MCAR") {
    # missing completely at random - simple binomial
    for (var in params$variables) {
      missing_idx <- as.logical(rbinom(n, 1, params$rate))
      data[missing_idx, var] <- NA
    }
    
  } else if (params$mechanism == "MAR") {
    # missing at random - depends on observed variables
    if (is.null(params$dependent_vars)) {
      stop("dependent_vars required for MAR mechanism")
    }
    
    # check dependent variables exist
    dep_missing <- setdiff(params$dependent_vars, names(data))
    if (length(dep_missing) > 0) {
      stop("dependent variables not found: ", paste(dep_missing, collapse = ", "))
    }
    
    # create missingness based on dependent variables
    for (var in params$variables) {
      # linear predictor based on dependent variables
      lp <- rep(qlogis(params$rate), n)
      
      for (dep_var in params$dependent_vars) {
        # standardise to avoid extreme probabilities
        dep_vals <- data[[dep_var]]
        if (is.numeric(dep_vals)) {
          dep_std <- (dep_vals - mean(dep_vals, na.rm = TRUE)) / sd(dep_vals, na.rm = TRUE)
          dep_std[is.na(dep_std)] <- 0
          lp <- lp + 0.5 * dep_std  # moderate effect
        } else {
          # for binary/factor variables
          lp <- lp + 0.5 * as.numeric(dep_vals)
        }
      }
      
      # convert to probability and apply
      miss_prob <- plogis(lp)
      missing_idx <- as.logical(rbinom(n, 1, miss_prob))
      data[missing_idx, var] <- NA
    }
    
  } else if (params$mechanism == "MNAR") {
    # missing not at random - depends on the value itself
    for (var in params$variables) {
      # store true values
      true_var <- paste0(var, "_true_premissing")
      data[[true_var]] <- data[[var]]
      
      # missingness depends on the variable's own value
      var_vals <- data[[var]]
      if (is.numeric(var_vals)) {
        # higher values more likely to be missing
        var_std <- (var_vals - mean(var_vals, na.rm = TRUE)) / sd(var_vals, na.rm = TRUE)
        var_std[is.na(var_std)] <- 0
        lp <- qlogis(params$rate) + 0.7 * var_std
      } else {
        # for binary, TRUE/1 more likely to be missing
        lp <- qlogis(params$rate) + 0.7 * as.numeric(var_vals)
      }
      
      # also include dependent variables if specified
      if (!is.null(params$dependent_vars)) {
        for (dep_var in params$dependent_vars) {
          dep_vals <- data[[dep_var]]
          if (is.numeric(dep_vals)) {
            dep_std <- (dep_vals - mean(dep_vals, na.rm = TRUE)) / sd(dep_vals, na.rm = TRUE)
            dep_std[is.na(dep_std)] <- 0
            lp <- lp + 0.3 * dep_std
          }
        }
      }
      
      miss_prob <- plogis(lp)
      missing_idx <- as.logical(rbinom(n, 1, miss_prob))
      data[missing_idx, var] <- NA
    }
  }
  
  data
}

# Positivity violation shadows ---------------------------------------------------

#' Create positivity violation shadow
#'
#' @param exposure_var Character. Name of exposure variable to filter
#' @param filter_fn Function that takes data and returns logical vector of valid rows
#' @param name Optional name for the shadow
#'
#' @return Positivity shadow object
#' @export
create_positivity_shadow <- function(
    exposure_var,
    filter_fn,
    name = "positivity_filter") {
  
  if (!is.function(filter_fn)) {
    stop("filter_fn must be a function")
  }
  
  create_shadow(
    type = "positivity",
    params = list(
      exposure_var = exposure_var,
      filter_fn = filter_fn
    ),
    name = name
  )
}

#' Apply positivity violation shadow
#'
#' @param data Data frame to apply shadow to
#' @param shadow A positivity shadow object created with create_positivity_shadow()
#' @param ... Additional arguments (currently unused)
#'
#' @return Filtered data frame with positivity violations removed
#' @export
apply_shadow.positivity_shadow <- function(data, shadow, ...) {
  params <- shadow$params
  
  # check exposure variable exists
  if (!params$exposure_var %in% names(data)) {
    stop("exposure variable '", params$exposure_var, "' not found in data")
  }
  
  # apply filter function
  valid_rows <- params$filter_fn(data)
  
  if (!is.logical(valid_rows) || length(valid_rows) != nrow(data)) {
    stop("filter_fn must return a logical vector of same length as data rows")
  }
  
  # subset data to valid rows only
  filtered_data <- data[valid_rows, ]
  
  # add attribute to track filtering
  attr(filtered_data, "positivity_filter") <- list(
    n_original = nrow(data),
    n_filtered = nrow(filtered_data),
    prop_retained = nrow(filtered_data) / nrow(data),
    exposure_var = params$exposure_var
  )
  
  filtered_data
}

# Helper functions -------------------------------------------------------------

`%||%` <- function(x, y) if (is.null(x)) y else x

# Examples ---------------------------------------------------------------------

#' Example: Create and apply various shadows
#' @export
example_shadows <- function() {
  # Generate base data
  dat <- margot_simulate(n = 1000, waves = 3, seed = 123)

  # Create shadows
  shadows <- list(
    # Classical measurement error in L
    create_shadow(
      "measurement_error",
      params = list(
        variables = c("t1_l", "t2_l"),
        error_type = "classical",
        sigma = 0.5
      ),
      name = "classical_error_l"
    ),

    # Differential error in Y based on treatment
    create_shadow(
      "measurement_error",
      params = list(
        variables = "t3_y",
        error_type = "differential",
        differential_var = "t2_a",
        differential_fn = function(a) 0.3 + 0.5 * a
      ),
      name = "differential_error_y"
    ),

    # Dichotomise outcome
    create_shadow(
      "measurement_error",
      params = list(
        variables = "t3_y",
        error_type = "dichotomise",
        threshold = function(y) median(y, na.rm = TRUE)
      ),
      name = "dichotomise_outcome"
    )
  )

  # Apply shadows
  shadowed_data <- apply_shadows(dat, shadows, verbose = TRUE)

  # Analyse effects
  effects <- analyse_shadow_effects(dat, shadowed_data)
  print(effects)

  invisible(list(original = dat, shadowed = shadowed_data, effects = effects))
}
