#' Wrapper for Shadow Application with Dual Data Architecture
#'
#' This function provides a unified interface for applying shadows while
#' optionally maintaining a complete copy of the original data.
#'
#' @param data Data frame to apply shadows to
#' @param shadows Shadow object or list of shadow objects
#' @param preserve_complete Logical, whether to return both true and observed data
#' @param verbose Logical, print progress messages
#' @param ... Additional arguments passed to shadow methods
#'
#' @return If preserve_complete is FALSE, returns the shadowed data.
#'   If preserve_complete is TRUE, returns a list with:
#'   \itemize{
#'     \item data_true: Original data before shadows
#'     \item data_observed: Data after shadows applied
#'     \item shadows_applied: List of applied shadow names
#'   }
#'
#' @export
apply_shadows_with_truth <- function(data, 
                                     shadows, 
                                     preserve_complete = TRUE,
                                     verbose = FALSE,
                                     ...) {
  # preserve original data if requested
  if (preserve_complete) {
    # deep copy preserving attributes
    data_true <- data
    # ensure it's truly independent
    data_true[] <- lapply(data, function(x) x)
  }
  
  # apply shadows
  data_observed <- apply_shadows(
    data = data,
    shadows = shadows,
    verbose = verbose,
    preserve_truth = TRUE  # always preserve column-level truth
  )
  
  # return based on preserve_complete flag
  if (preserve_complete) {
    result <- list(
      data_true = data_true,
      data_observed = data_observed,
      shadows_applied = attr(data_observed, "applied_shadows")
    )
    class(result) <- c("margot_shadow_result", "list")
    return(result)
  } else {
    return(data_observed)
  }
}

#' Print Method for Shadow Result
#'
#' @param x A margot_shadow_result object
#' @param ... Additional arguments (ignored)
#'
#' @export
print.margot_shadow_result <- function(x, ...) {
  cat("Margot Shadow Application Result\n")
  cat("================================\n")
  cat("Structure: Dual data architecture\n")
  cat(sprintf("  - data_true: %d obs. of %d variables\n", 
              nrow(x$data_true), ncol(x$data_true)))
  cat(sprintf("  - data_observed: %d obs. of %d variables\n", 
              nrow(x$data_observed), ncol(x$data_observed)))
  
  if (!is.null(x$shadows_applied)) {
    cat("\nShadows applied:\n")
    for (shadow in x$shadows_applied) {
      cat(sprintf("  - %s\n", shadow))
    }
  }
  
  # check for *_true columns
  true_cols <- grep("_true$", names(x$data_observed), value = TRUE)
  if (length(true_cols) > 0) {
    cat("\nPreserved truth columns:\n")
    for (col in true_cols) {
      original_var <- sub("_true$", "", col)
      cat(sprintf("  - %s (original: %s)\n", col, original_var))
    }
  }
  
  invisible(x)
}

#' Compare Effects Before and After Shadows
#'
#' Convenience function that computes causal effects from both true and
#' observed data in a shadow result object.
#'
#' @param shadow_result A margot_shadow_result object
#' @param wave Treatment wave
#' @param outcome_wave Outcome wave
#' @param treatment_name Treatment variable name
#' @param outcome_name Outcome variable name
#' @param estimands Which estimands to compute
#' @param ... Additional arguments passed to compute_effects_from_sim
#'
#' @return A list with:
#'   \itemize{
#'     \item effects_true: Effects from true data
#'     \item effects_observed: Effects from observed data
#'     \item comparison: Bias comparison table
#'   }
#'
#' @export
compare_shadow_effects <- function(shadow_result,
                                   wave = 1,
                                   outcome_wave = NULL,
                                   treatment_name = "a",
                                   outcome_name = "y",
                                   estimands = c("ate", "att", "atu"),
                                   ...) {
  # check input
  if (!inherits(shadow_result, "margot_shadow_result")) {
    stop("Input must be a margot_shadow_result object")
  }
  
  # compute effects from true data
  effects_true <- compute_effects_from_sim(
    shadow_result$data_true,
    wave = wave,
    outcome_wave = outcome_wave,
    treatment_name = treatment_name,
    outcome_name = outcome_name,
    estimands = estimands,
    ...
  )
  
  # compute effects from observed data
  effects_observed <- compute_effects_from_sim(
    shadow_result$data_observed,
    wave = wave,
    outcome_wave = outcome_wave,
    treatment_name = treatment_name,
    outcome_name = outcome_name,
    estimands = estimands,
    ...
  )
  
  # compare bias
  comparison <- compare_shadow_bias(effects_true, effects_observed)
  
  # return structured result
  result <- list(
    effects_true = effects_true,
    effects_observed = effects_observed,
    comparison = comparison
  )
  
  class(result) <- c("shadow_effect_comparison", "list")
  return(result)
}

#' Print Method for Shadow Effect Comparison
#'
#' @param x A shadow_effect_comparison object
#' @param digits Number of digits for rounding
#' @param ... Additional arguments (ignored)
#'
#' @export
print.shadow_effect_comparison <- function(x, digits = 3, ...) {
  cat("Shadow Effect Comparison\n")
  cat("========================\n\n")
  
  cat("True Effects:\n")
  print(x$effects_true, digits = digits)
  
  cat("\nObserved Effects (after shadows):\n")
  print(x$effects_observed, digits = digits)
  
  cat("\nBias Analysis:\n")
  print(x$comparison, digits = digits)
  
  invisible(x)
}