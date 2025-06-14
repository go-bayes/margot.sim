#' Extended Shadow Types for margot
#'
#' @description
#' This file implements additional shadow types for the margot package:
#' truncation, coarsening, and mode effects shadows.

# Truncation Shadow -------------------------------------------------------

#' Create a truncation shadow
#'
#' Creates a shadow that truncates values outside specified bounds, simulating
#' measurement instruments with limited ranges or administrative data thresholds.
#'
#' @param variables Character vector of variable names to truncate
#' @param lower Numeric lower bound (default -Inf)
#' @param upper Numeric upper bound (default Inf)
#' @param type Character string: "simple" (values become NA), "boundary" (values pile up at limits),
#'   or "redistribute" (truncated mass redistributed to interior)
#' @param name Optional name for the shadow
#'
#' @return A truncation shadow object
#' @export
#'
#' @examples
#' # create truncation shadow for lab equipment with range 0-300
#' truncate_cholesterol <- create_truncation_shadow(
#'   variables = "cholesterol",
#'   lower = 0,
#'   upper = 300,
#'   type = "simple"
#' )
#'
#' # create truncation shadow for income top-coding
#' truncate_income <- create_truncation_shadow(
#'   variables = c("income_wave1", "income_wave2"),
#'   upper = 200000,
#'   type = "boundary"  # pile up at boundary
#' )
create_truncation_shadow <- function(variables,
                                   lower = -Inf,
                                   upper = Inf,
                                   type = c("simple", "boundary", "redistribute"),
                                   name = NULL) {
  type <- match.arg(type)
  
  # validate inputs
  if (length(variables) == 0) {
    stop("Must specify at least one variable to truncate")
  }
  
  if (!is.numeric(lower) || !is.numeric(upper)) {
    stop("Lower and upper bounds must be numeric")
  }
  
  if (lower >= upper) {
    stop("Lower bound must be less than upper bound")
  }
  
  params <- list(
    variables = variables,
    lower = lower,
    upper = upper,
    type = type
  )
  
  structure(
    list(
      type = "truncation",
      params = params,
      name = name %||% "truncation_shadow"
    ),
    class = c("truncation_shadow", "margot_shadow")
  )
}

#' Apply truncation shadow
#'
#' @param data Data frame to apply shadow to
#' @param shadow A truncation shadow object
#' @param ... Additional arguments (currently unused)
#'
#' @return Data frame with truncation applied
#' @export
apply_shadow.truncation_shadow <- function(data, shadow, ...) {
  params <- shadow$params
  
  for (var in params$variables) {
    if (!var %in% names(data)) {
      warning(sprintf("Variable '%s' not found in data, skipping", var))
      next
    }
    
    # preserve original values
    data[[paste0(var, "_true")]] <- data[[var]]
    
    vals <- data[[var]]
    
    if (params$type == "simple") {
      # simple truncation to NA
      if (!is.infinite(params$lower)) {
        vals[vals < params$lower] <- NA
      }
      if (!is.infinite(params$upper)) {
        vals[vals > params$upper] <- NA
      }
      
    } else if (params$type == "boundary") {
      # pile up at boundaries
      if (!is.infinite(params$lower)) {
        vals[vals < params$lower] <- params$lower
      }
      if (!is.infinite(params$upper)) {
        vals[vals > params$upper] <- params$upper
      }
      
    } else if (params$type == "redistribute") {
      # future enhancement: redistribute truncated mass
      stop("Redistribution type not yet implemented")
    }
    
    # add truncation indicator
    data[[paste0(var, "_truncated")]] <- 
      (!is.na(data[[paste0(var, "_true")]]) & is.na(vals)) |
      (data[[paste0(var, "_true")]] != vals)
    
    data[[var]] <- vals
  }
  
  data
}

# Coarsening Shadow -------------------------------------------------------

#' Create a coarsening shadow
#'
#' Creates a shadow that converts continuous data to categorical bins,
#' simulating loss of precision in data collection or privacy-preserving
#' data release.
#'
#' @param variables Character vector of variable names to coarsen
#' @param breaks Either a numeric vector of break points or a single number
#'   giving the number of intervals
#' @param labels Character vector of labels for the bins (optional)
#' @param type Character string specifying how to handle values within bins:
#'   "midpoint" (bin centres), "lower" (left edge), "upper" (right edge),
#'   "random" (uniform within bin), or "heaping" (digit preference patterns)
#' @param heaping_digits Numeric vector of preferred final digits when type="heaping"
#'   (default c(0, 5) for rounding to 0s and 5s)
#' @param heaping_prob Probability of heaping when type="heaping" (default 0.7)
#' @param include_lowest Logical, whether to include the lowest value in the first bin
#' @param right Logical, whether intervals should be closed on the right
#' @param name Optional name for the shadow
#'
#' @return A coarsening shadow object
#' @export
#'
#' @examples
#' # create age group coarsening
#' coarsen_age <- create_coarsening_shadow(
#'   variables = "age",
#'   breaks = c(0, 18, 25, 35, 45, 55, 65, Inf),
#'   labels = c("<18", "18-24", "25-34", "35-44", "45-54", "55-64", "65+"),
#'   type = "midpoint"
#' )
#'
#' # create income brackets with heaping at round numbers
#' coarsen_income <- create_coarsening_shadow(
#'   variables = c("income_self", "income_spouse"),
#'   breaks = c(0, 25000, 50000, 75000, 100000, 150000, Inf),
#'   type = "heaping",
#'   heaping_digits = c(0),  # heap at multiples of 10000
#'   heaping_prob = 0.8
#' )
#'
#' # create 5-point likert scale from continuous
#' coarsen_satisfaction <- create_coarsening_shadow(
#'   variables = "satisfaction_score",
#'   breaks = 5,  # 5 equal intervals
#'   type = "lower",  # assume people pick lowest qualifying option
#'   labels = c("Very Unsatisfied", "Unsatisfied", "Neutral", 
#'              "Satisfied", "Very Satisfied")
#' )
create_coarsening_shadow <- function(variables,
                                   breaks,
                                   labels = NULL,
                                   type = c("midpoint", "lower", "upper", "random", "heaping"),
                                   heaping_digits = c(0, 5),
                                   heaping_prob = 0.7,
                                   include_lowest = TRUE,
                                   right = TRUE,
                                   name = NULL) {
  type <- match.arg(type)
  
  # validate inputs
  if (length(variables) == 0) {
    stop("Must specify at least one variable to coarsen")
  }
  
  if (length(breaks) == 0) {
    stop("Must specify breaks for coarsening")
  }
  
  # check labels if provided
  if (!is.null(labels)) {
    if (length(breaks) == 1) {
      # breaks is number of intervals
      expected_labels <- breaks
    } else {
      # breaks is vector of breakpoints
      expected_labels <- length(breaks) - 1
    }
    if (length(labels) != expected_labels) {
      stop(sprintf("Number of labels (%d) must match number of intervals (%d)",
                   length(labels), expected_labels))
    }
  }
  
  # validate heaping parameters
  if (type == "heaping") {
    if (!is.numeric(heaping_digits) || any(heaping_digits < 0) || any(heaping_digits > 9)) {
      stop("heaping_digits must be numeric values between 0 and 9")
    }
    if (!is.numeric(heaping_prob) || heaping_prob < 0 || heaping_prob > 1) {
      stop("heaping_prob must be a probability between 0 and 1")
    }
  }
  
  params <- list(
    variables = variables,
    breaks = breaks,
    labels = labels,
    type = type,
    heaping_digits = heaping_digits,
    heaping_prob = heaping_prob,
    include_lowest = include_lowest,
    right = right
  )
  
  structure(
    list(
      type = "coarsening",
      params = params,
      name = name %||% "coarsening_shadow"
    ),
    class = c("coarsening_shadow", "margot_shadow")
  )
}

#' Apply coarsening shadow
#'
#' @param data Data frame to apply shadow to
#' @param shadow A coarsening shadow object
#' @param ... Additional arguments (currently unused)
#'
#' @return Data frame with coarsening applied
#' @export
apply_shadow.coarsening_shadow <- function(data, shadow, ...) {
  params <- shadow$params
  
  for (var in params$variables) {
    if (!var %in% names(data)) {
      warning(sprintf("Variable '%s' not found in data, skipping", var))
      next
    }
    
    # preserve original values
    data[[paste0(var, "_true")]] <- data[[var]]
    
    # create bins
    binned <- cut(data[[var]], 
                  breaks = params$breaks,
                  labels = params$labels,
                  include.lowest = params$include_lowest,
                  right = params$right)
    
    # convert back to numeric based on type
    if (params$type == "midpoint") {
      # use bin midpoints
      if (is.null(params$labels)) {
        # extract interval boundaries and calculate midpoints
        intervals <- levels(binned)
        midpoints <- sapply(intervals, function(x) {
          # parse interval string more carefully
          x_clean <- gsub("\\[|\\]|\\(|\\)", "", x)
          bounds <- as.numeric(unlist(strsplit(x_clean, ",")))
          if (length(bounds) == 2 && !any(is.na(bounds))) {
            # handle infinite bounds
            if (is.infinite(bounds[1])) bounds[1] <- bounds[2] - 1000
            if (is.infinite(bounds[2])) bounds[2] <- bounds[1] + 1000
            mean(bounds)
          } else {
            NA
          }
        })
        data[[var]] <- midpoints[as.numeric(binned)]
      } else {
        # keep as factor if labels provided
        data[[var]] <- binned
      }
      
    } else if (params$type == "lower") {
      # use lower bound of interval
      if (is.null(params$labels)) {
        intervals <- levels(binned)
        lower_bounds <- sapply(intervals, function(x) {
          x_clean <- gsub("\\[|\\]|\\(|\\)", "", x)
          bounds <- as.numeric(unlist(strsplit(x_clean, ",")))
          if (length(bounds) >= 1 && !is.na(bounds[1])) bounds[1] else NA
        })
        data[[var]] <- lower_bounds[as.numeric(binned)]
      } else {
        data[[var]] <- binned
      }
      
    } else if (params$type == "upper") {
      # use upper bound of interval
      if (is.null(params$labels)) {
        intervals <- levels(binned)
        upper_bounds <- sapply(intervals, function(x) {
          x_clean <- gsub("\\[|\\]|\\(|\\)", "", x)
          bounds <- as.numeric(unlist(strsplit(x_clean, ",")))
          if (length(bounds) >= 2 && !is.na(bounds[2])) bounds[2] else NA
        })
        data[[var]] <- upper_bounds[as.numeric(binned)]
      } else {
        data[[var]] <- binned
      }
      
    } else if (params$type == "random") {
      # random value within interval
      if (is.null(params$labels)) {
        n <- length(data[[var]])
        result <- numeric(n)
        
        for (i in seq_along(levels(binned))) {
          level <- levels(binned)[i]
          idx <- which(as.numeric(binned) == i)
          if (length(idx) > 0) {
            x_clean <- gsub("\\[|\\]|\\(|\\)", "", level)
            bounds <- as.numeric(unlist(strsplit(x_clean, ",")))
            if (length(bounds) == 2 && !any(is.na(bounds))) {
              # handle infinite bounds
              if (is.infinite(bounds[1])) bounds[1] <- bounds[2] - 1000
              if (is.infinite(bounds[2])) bounds[2] <- bounds[1] + 1000
              result[idx] <- runif(length(idx), bounds[1], bounds[2])
            }
          }
        }
        data[[var]] <- result
      } else {
        data[[var]] <- binned
      }
      
    } else if (params$type == "heaping") {
      # heaping at preferred digits
      if (is.null(params$labels)) {
        n <- length(data[[var]])
        result <- numeric(n)
        
        for (i in seq_along(levels(binned))) {
          level <- levels(binned)[i]
          idx <- which(as.numeric(binned) == i)
          if (length(idx) > 0) {
            x_clean <- gsub("\\[|\\]|\\(|\\)", "", level)
            bounds <- as.numeric(unlist(strsplit(x_clean, ",")))
            if (length(bounds) == 2 && !any(is.na(bounds))) {
              # handle infinite bounds
              if (is.infinite(bounds[1])) bounds[1] <- bounds[2] - 1000
              if (is.infinite(bounds[2])) bounds[2] <- bounds[1] + 1000
              
              # determine which values get heaped
              heap_mask <- runif(length(idx)) < params$heaping_prob
              
              # for heaped values, find nearest preferred digit
              for (j in which(heap_mask)) {
                true_val <- data[[paste0(var, "_true")]][idx[j]]
                
                # skip NA values
                if (is.na(true_val)) {
                  result[idx[j]] <- NA
                  next
                }
                
                # find magnitude for heaping (e.g., heap to nearest 10, 100, etc.)
                magnitude <- 10^floor(log10(abs(true_val) + 1))
                
                # find nearest value ending in preferred digit
                candidates <- numeric()
                for (digit in params$heaping_digits) {
                  # calculate candidates at this magnitude
                  base <- floor(true_val / magnitude) * magnitude
                  candidate <- base + digit * (magnitude / 10)
                  
                  # ensure candidate is within bounds
                  while (candidate < bounds[1]) {
                    candidate <- candidate + magnitude
                  }
                  while (candidate > bounds[2]) {
                    candidate <- candidate - magnitude
                  }
                  
                  if (candidate >= bounds[1] && candidate <= bounds[2]) {
                    candidates <- c(candidates, candidate)
                  }
                }
                
                if (length(candidates) > 0) {
                  # choose nearest candidate
                  distances <- abs(candidates - true_val)
                  result[idx[j]] <- candidates[which.min(distances)]
                } else {
                  # fallback to midpoint if no valid heap value
                  result[idx[j]] <- mean(bounds)
                }
              }
              
              # for non-heaped values, use random within interval
              non_heap_idx <- idx[!heap_mask]
              if (length(non_heap_idx) > 0) {
                result[non_heap_idx] <- runif(length(non_heap_idx), bounds[1], bounds[2])
              }
            }
          }
        }
        data[[var]] <- result
      } else {
        data[[var]] <- binned
      }
    }
    
    # add coarsening indicator
    data[[paste0(var, "_coarsened")]] <- TRUE
  }
  
  data
}

# Mode Effects Shadow -----------------------------------------------------

#' Create a mode effects shadow
#'
#' Creates a shadow that applies systematic measurement differences based on
#' data collection mode (e.g., phone vs in-person, self-report vs clinical).
#'
#' @param variables Character vector of variable names affected by mode
#' @param mode_var Character name of the variable indicating collection mode
#' @param effect_specs List specifying effects for each mode. Each mode should
#'   have a list with elements: shift (additive), scale (multiplicative),
#'   and/or noise (additional error SD)
#' @param reference_mode Character name of the reference mode (no effect applied)
#' @param name Optional name for the shadow
#'
#' @return A mode effects shadow object
#' @export
#'
#' @examples
#' # create mode effects for survey responses
#' mode_shadow <- create_mode_effects_shadow(
#'   variables = c("income", "health_rating"),
#'   mode_var = "survey_mode",
#'   effect_specs = list(
#'     phone = list(shift = 0.1, scale = 0.95),     # slight underreporting
#'     online = list(shift = -0.05, scale = 1.05),  # slight overreporting
#'     paper = list(shift = 0, scale = 1, noise = 0.2)  # more random error
#'   ),
#'   reference_mode = "in_person"
#' )
create_mode_effects_shadow <- function(variables,
                                     mode_var,
                                     effect_specs,
                                     reference_mode = NULL,
                                     name = NULL) {
  # validate inputs
  if (length(variables) == 0) {
    stop("Must specify at least one variable affected by mode")
  }
  
  if (length(mode_var) != 1 || !is.character(mode_var)) {
    stop("mode_var must be a single character string")
  }
  
  if (!is.list(effect_specs)) {
    stop("effect_specs must be a list")
  }
  
  # validate effect specifications
  for (mode in names(effect_specs)) {
    spec <- effect_specs[[mode]]
    if (!is.list(spec)) {
      stop(sprintf("Effect specification for mode '%s' must be a list", mode))
    }
    
    # check for valid effect types
    valid_effects <- c("shift", "scale", "noise")
    invalid <- setdiff(names(spec), valid_effects)
    if (length(invalid) > 0) {
      stop(sprintf("Invalid effect types for mode '%s': %s", 
                   mode, paste(invalid, collapse = ", ")))
    }
    
    # validate numeric values
    if (!is.null(spec$shift) && !is.numeric(spec$shift)) {
      stop(sprintf("Shift for mode '%s' must be numeric", mode))
    }
    if (!is.null(spec$scale) && (!is.numeric(spec$scale) || spec$scale <= 0)) {
      stop(sprintf("Scale for mode '%s' must be positive numeric", mode))
    }
    if (!is.null(spec$noise) && (!is.numeric(spec$noise) || spec$noise < 0)) {
      stop(sprintf("Noise for mode '%s' must be non-negative numeric", mode))
    }
  }
  
  params <- list(
    variables = variables,
    mode_var = mode_var,
    effect_specs = effect_specs,
    reference_mode = reference_mode
  )
  
  structure(
    list(
      type = "mode_effects",
      params = params,
      name = name %||% "mode_effects_shadow"
    ),
    class = c("mode_effects_shadow", "margot_shadow")
  )
}

#' Apply mode effects shadow
#'
#' @param data Data frame to apply shadow to
#' @param shadow A mode effects shadow object
#' @param ... Additional arguments (currently unused)
#'
#' @return Data frame with mode effects applied
#' @export
apply_shadow.mode_effects_shadow <- function(data, shadow, ...) {
  params <- shadow$params
  
  # check mode variable exists
  if (!params$mode_var %in% names(data)) {
    stop(sprintf("Mode variable '%s' not found in data", params$mode_var))
  }
  
  modes <- data[[params$mode_var]]
  
  for (var in params$variables) {
    if (!var %in% names(data)) {
      warning(sprintf("Variable '%s' not found in data, skipping", var))
      next
    }
    
    # preserve original values
    data[[paste0(var, "_true")]] <- data[[var]]
    
    vals <- data[[var]]
    
    # apply mode-specific effects
    for (mode in names(params$effect_specs)) {
      mode_idx <- which(modes == mode)
      if (length(mode_idx) == 0) next
      
      spec <- params$effect_specs[[mode]]
      
      # apply effects in order: scale, shift, noise
      if (!is.null(spec$scale)) {
        vals[mode_idx] <- vals[mode_idx] * spec$scale
      }
      
      if (!is.null(spec$shift)) {
        vals[mode_idx] <- vals[mode_idx] + spec$shift
      }
      
      if (!is.null(spec$noise) && spec$noise > 0) {
        vals[mode_idx] <- vals[mode_idx] + rnorm(length(mode_idx), 0, spec$noise)
      }
    }
    
    data[[var]] <- vals
    
    # add mode effect indicator
    data[[paste0(var, "_mode_affected")]] <- modes %in% names(params$effect_specs)
  }
  
  data
}

# Update validate_shadow_params in margot-shadows.R ----------------------
# This would need to be added to the existing validate_shadow_params function

#' @keywords internal
validate_truncation_params <- function(params) {
  defaults <- list(
    variables = character(),
    lower = -Inf,
    upper = Inf,
    type = "simple"
  )
  
  params <- modifyList(defaults, params)
  
  # additional validation done in create_truncation_shadow
  
  params
}

#' @keywords internal
validate_coarsening_params <- function(params) {
  defaults <- list(
    variables = character(),
    breaks = numeric(),
    labels = NULL,
    type = "midpoint",
    heaping_digits = c(0, 5),
    heaping_prob = 0.7,
    include_lowest = TRUE,
    right = TRUE
  )
  
  params <- modifyList(defaults, params)
  
  # additional validation done in create_coarsening_shadow
  
  params
}

#' @keywords internal
validate_mode_effects_params <- function(params) {
  defaults <- list(
    variables = character(),
    mode_var = character(),
    effect_specs = list(),
    reference_mode = NULL
  )
  
  params <- modifyList(defaults, params)
  
  # additional validation done in create_mode_effects_shadow
  
  params
}