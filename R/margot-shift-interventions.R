#' Shift intervention functions for margot.sim
#'
#' @description
#' Functions for creating shift interventions commonly used in 
#' modified treatment policies (MTPs) and longitudinal causal inference.
#'
#' @name shift-interventions
#' @keywords internal
NULL

#' Create bounded shift intervention
#'
#' @param shift_amount Numeric amount to shift (positive or negative)
#' @param min_value Minimum allowed value after shift
#' @param max_value Maximum allowed value after shift
#' @param start_wave Wave at which to start shifting (default: 1)
#' 
#' @return Function suitable for use as intervention in margot_simulate
#' @export
#'
#' @examples
#' # Shift up by 1, bounded between 1 and 7
#' shift_up_bounded <- create_shift_intervention(
#'   shift_amount = 1,
#'   min_value = 1,
#'   max_value = 7,
#'   start_wave = 1
#' )
#' 
#' # Use in simulation
#' \dontrun{
#' sim_data <- margot_simulate(
#'   n = 1000,
#'   waves = 3,
#'   exposure_type = "continuous",
#'   intervention = shift_up_bounded
#' )
#' }
create_shift_intervention <- function(shift_amount, 
                                     min_value = -Inf, 
                                     max_value = Inf,
                                     start_wave = 1) {
  
  function(data, time, trt) {
    # Return natural values before start_wave
    if (time < start_wave) {
      return(data[[trt]])
    }
    
    # Apply bounded shift
    shifted <- data[[trt]] + shift_amount
    
    # Apply bounds
    shifted <- pmax(min_value, pmin(max_value, shifted))
    
    return(shifted)
  }
}

#' Create threshold-based shift intervention
#'
#' @param threshold Value below/above which to apply shift
#' @param shift_to Target value to shift to
#' @param direction "up" to shift values below threshold, "down" for above
#' @param start_wave Wave at which to start shifting
#'
#' @return Function suitable for use as intervention in margot_simulate
#' @export
#'
#' @examples
#' # Shift values below 1 up to 1
#' shift_low_values <- create_threshold_shift(
#'   threshold = 1,
#'   shift_to = 1,
#'   direction = "up"
#' )
create_threshold_shift <- function(threshold, 
                                  shift_to, 
                                  direction = c("up", "down"),
                                  start_wave = 1) {
  direction <- match.arg(direction)
  
  function(data, time, trt) {
    # Return natural values before start_wave
    if (time < start_wave) {
      return(data[[trt]])
    }
    
    values <- data[[trt]]
    
    if (direction == "up") {
      # Shift values below threshold up to shift_to
      ifelse(values < threshold, shift_to, values)
    } else {
      # Shift values above threshold down to shift_to
      ifelse(values > threshold, shift_to, values)
    }
  }
}

#' Create wave-specific shift intervention
#'
#' @param wave_shifts Named list mapping waves to shift functions
#'
#' @return Function suitable for use as intervention in margot_simulate
#' @export
#'
#' @examples
#' # Different shifts at different waves
#' wave_specific <- create_wave_specific_shift(
#'   wave_shifts = list(
#'     "0" = function(x) x,              # No change at baseline
#'     "1" = function(x) pmin(x + 1, 7), # Shift up by 1 at wave 1
#'     "2" = function(x) pmax(x - 1, 1)  # Shift down by 1 at wave 2
#'   )
#' )
create_wave_specific_shift <- function(wave_shifts) {
  
  function(data, time, trt) {
    wave_key <- as.character(time)
    
    if (wave_key %in% names(wave_shifts)) {
      shift_fn <- wave_shifts[[wave_key]]
      shift_fn(data[[trt]])
    } else {
      # Default to no change
      data[[trt]]
    }
  }
}

#' Example shift interventions matching lmtp style
#'
#' @param min_score Minimum score value
#' @param max_score Maximum score value
#' @param baseline_wave Wave to keep as observed (typically 0)
#' @param shift_wave Wave to apply shift (typically 1)
#'
#' @return List containing shift_up and shift_down intervention functions
#' @export
#'
#' @examples
#' # Create lmtp-style shift interventions
#' shifts <- create_lmtp_style_shifts(
#'   min_score = 1,
#'   max_score = 7,
#'   baseline_wave = 0,
#'   shift_wave = 1
#' )
#' 
#' # Use in margot_simulate_causal
#' \dontrun{
#' results <- margot_simulate_causal(
#'   n = 1000,
#'   waves = 2,
#'   treatments = "a",
#'   interventions = list(
#'     natural = function(data, time, trt) data[[trt]],
#'     shift_up = shifts$shift_up,
#'     shift_down = shifts$shift_down
#'   )
#' )
#' }
create_lmtp_style_shifts <- function(min_score = 1, 
                                    max_score = 7,
                                    baseline_wave = 0,
                                    shift_wave = 1) {
  
  # Shift up function
  shift_up <- function(data, time, trt) {
    if (time == baseline_wave) {
      # Keep baseline as observed
      return(data[[trt]])
    }
    
    if (time == shift_wave) {
      # Apply upward shift with bounds
      values <- data[[trt]]
      return(ifelse(values <= max_score - 1, values + 1, max_score))
    }
    
    # Other waves: return as is
    return(data[[trt]])
  }
  
  # Shift down function
  shift_down <- function(data, time, trt) {
    if (time == baseline_wave) {
      # Keep baseline as observed
      return(data[[trt]])
    }
    
    if (time == shift_wave) {
      # Apply downward shift with bounds
      values <- data[[trt]]
      return(ifelse(values >= min_score + 1, values - 1, min_score))
    }
    
    # Other waves: return as is
    return(data[[trt]])
  }
  
  list(
    shift_up = shift_up,
    shift_down = shift_down
  )
}

#' Create modified treatment policy (MTP) intervention
#'
#' @description
#' Creates interventions that depend on the natural value of treatment,
#' similar to the lmtp package approach.
#'
#' @param policy Function that takes (natural_value, covariates) and returns modified value
#' @param covariate_names Character vector of covariate names to consider
#' @param start_wave Wave at which to start applying policy
#'
#' @return Function suitable for use as intervention in margot_simulate
#' @export
#'
#' @examples
#' # Only treat if natural value > 0 AND covariate L > 0
#' mtp <- create_mtp_intervention(
#'   policy = function(natural, covars) {
#'     ifelse(natural > 0 & covars$L > 0, natural, 0)
#'   },
#'   covariate_names = "L",
#'   start_wave = 1
#' )
create_mtp_intervention <- function(policy, 
                                   covariate_names = NULL,
                                   start_wave = 1) {
  
  function(data, time, trt) {
    if (time < start_wave) {
      return(data[[trt]])
    }
    
    # Get natural values
    natural_values <- data[[trt]]
    
    # Get covariates if specified
    if (!is.null(covariate_names)) {
      # Build covariate data frame
      covar_data <- data.frame(row.names = seq_len(nrow(data)))
      
      for (covar in covariate_names) {
        # Try time-specific version first
        time_covar <- paste0("t", time, "_", covar)
        if (time_covar %in% names(data)) {
          covar_data[[covar]] <- data[[time_covar]]
        } else if (covar %in% names(data)) {
          covar_data[[covar]] <- data[[covar]]
        }
      }
      
      # Apply policy with covariates
      policy(natural_values, covar_data)
    } else {
      # Apply policy without covariates
      policy(natural_values, NULL)
    }
  }
}

#' Example: Incremental propensity score interventions
#'
#' @param delta Multiplicative shift for odds of treatment
#' @param start_wave Wave to start intervention
#'
#' @return Function suitable for use as intervention in margot_simulate
#' @export
#'
#' @examples
#' # Increase odds of treatment by 20%
#' ips_intervention <- create_ips_intervention(delta = 1.2)
create_ips_intervention <- function(delta, start_wave = 1) {
  
  if (delta <= 0) {
    stop("delta must be positive")
  }
  
  function(data, time, trt) {
    if (time < start_wave) {
      return(data[[trt]])
    }
    
    # This would need access to propensity scores
    # For now, approximate with logistic transform
    values <- data[[trt]]
    
    if (all(values %in% c(0, 1))) {
      # Binary treatment
      # Transform through log-odds
      probs <- mean(values)
      odds <- probs / (1 - probs)
      new_odds <- odds * delta
      new_prob <- new_odds / (1 + new_odds)
      
      # Resample with new probability
      rbinom(length(values), 1, new_prob)
    } else {
      # Continuous treatment - simple multiplicative shift
      values * delta
    }
  }
}