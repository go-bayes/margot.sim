#' Simulate data for causal inference with proper censoring
#'
#' @description
#' Wrapper function that simulates data under multiple intervention regimes
#' for causal inference. Supports:
#' - Multiple treatment strategies
#' - Sampling weights for target populations
#' - Post-hoc censoring to create realistic observed data
#' - Automatic computation of true causal effects
#' - Bias analysis from censoring
#'
#' @param n Integer. Number of subjects to simulate
#' @param waves Integer. Number of measurement waves
#' @param treatments Character vector. Names of treatment variables (currently uses first)
#' @param interventions Named list of intervention functions. Each function should
#'   have signature function(data, time, trt) and return treatment values
#' @param sampling_weights Weights or function to create target population
#' @param apply_censoring Logical. Apply censoring to create observed data?
#' @param use_process_function Logical. Use margot_process_longitudinal?
#' @param common_params List with elements:
#'   - params: Parameters for margot_simulate
#'   - censoring_params: Censoring parameters
#'   - verbose: Print progress?
#' @param ... Additional arguments passed to margot_simulate
#'
#' @return Object of class "margot_causal_sim" containing:
#'   - data: List of datasets under each intervention
#'   - data_true: Complete data before any shadows (if shadows applied)
#'   - data_observed: Data after shadows applied (if shadows applied)
#'   - effects: True causal effects (if multiple interventions)
#'   - effects_true: Effects computed from true data
#'   - effects_observed: Effects computed from observed data
#'   - censoring_bias: Bias induced by censoring (if apply_censoring = TRUE)
#'   - metadata: Simulation metadata
#'
#' @examples
#' # Define interventions
#' interventions <- list(
#'   never_treat = function(data, time, trt) {
#'     rep(0, nrow(data))
#'   },
#'   always_treat = function(data, time, trt) {
#'     rep(1, nrow(data))
#'   },
#'   natural = function(data, time, trt) {
#'     data[[trt]]  # return natural treatment
#'   }
#' )
#'
#' # Simulate with censoring
#' results <- margot_simulate_causal(
#'   n = 1000,
#'   waves = 3,
#'   treatments = "a",
#'   interventions = interventions,
#'   apply_censoring = TRUE,
#'   common_params = list(
#'     params = list(a_lag_y_coef = 0.3),
#'     verbose = TRUE
#'   )
#' )
#'
#' @export
margot_simulate_causal <- function(
    n,
    waves,
    treatments,
    interventions,
    sampling_weights = NULL,
    apply_censoring = FALSE,
    use_process_function = TRUE,
    common_params = list(),
    ...) {

  # validate inputs
  if (!is.list(interventions)) {
    stop("interventions must be a named list of functions")
  }

  intervention_names <- names(interventions)
  if (is.null(intervention_names) || any(intervention_names == "")) {
    stop("all interventions must be named")
  }

  # simulate under each intervention
  data_list <- list()

  for (name in intervention_names) {
    if (!is.null(common_params$verbose) && common_params$verbose) {
      message(sprintf("simulating under intervention: %s", name))
    }

    # extract params
    sim_params <- if (!is.null(common_params$params)) common_params$params else list()
    censoring_params <- if (!is.null(common_params$censoring_params)) common_params$censoring_params else list(
      rate = 0.1,
      exposure_dependence = TRUE,
      l_dependence = FALSE,
      y_dependence = FALSE,
      latent_dependence = FALSE
    )

    # simulate complete data (no censoring in generation)
    # but we need to pass censoring params to store probabilities
    complete_data <- margot_simulate(
      n = n,
      waves = waves,
      intervention = interventions[[name]],
      params = sim_params,
      sampling_weights = sampling_weights,
      apply_process_function = FALSE,  # do this later
      censoring = censoring_params,
      ...
    )

    if (apply_censoring) {
      # apply censoring post-hoc
      observed_data <- apply_censoring_post_hoc(
        complete_data,
        censoring_type = "built_in",
        apply_process_function = use_process_function
      )

      # also process complete data for consistency
      if (use_process_function) {
        complete_data <- apply_censoring_post_hoc(
          complete_data,
          censoring_type = "custom",
          censoring_function = function(x, ...) x,  # no-op
          apply_process_function = TRUE
        )
      }

      data_list[[name]] <- list(
        complete = complete_data,
        observed = observed_data
      )
    } else {
      # optionally process complete data
      if (use_process_function) {
        complete_data <- apply_censoring_post_hoc(
          complete_data,
          censoring_type = "custom",
          censoring_function = function(x, ...) x,  # no-op
          apply_process_function = TRUE
        )
      }

      data_list[[name]] <- complete_data
    }
  }

  # compute true effects
  if (length(data_list) >= 2) {
    # extract appropriate datasets
    if (apply_censoring) {
      effect_data <- lapply(data_list, function(x) x$complete)
      observed_data <- lapply(data_list, function(x) x$observed)
    } else {
      effect_data <- data_list
      observed_data <- NULL
    }

    # find outcome variable
    first_data <- effect_data[[1]]
    outcome_vars <- grep(paste0("^t", waves + 1, "_y"), names(first_data), value = TRUE)

    if (length(outcome_vars) == 0) {
      warning("no outcome variables found")
      effects <- NULL
      censoring_bias <- NULL
    } else {
      # true effects from complete data
      effects <- compute_true_effects(
        effect_data,
        outcome_name = outcome_vars[1],
        estimand = "ATE"
      )

      # bias from censoring if applicable
      censoring_bias <- NULL
      observed_effects <- NULL
      if (!is.null(observed_data)) {
        observed_effects <- compute_true_effects(
          observed_data,
          outcome_name = outcome_vars[1],
          estimand = "ATE"
        )

        censoring_bias <- merge(
          effects[, c("contrast", "estimate")],
          observed_effects[, c("contrast", "estimate")],
          by = "contrast",
          suffixes = c("_true", "_observed")
        )
        censoring_bias$bias <- censoring_bias$estimate_observed - censoring_bias$estimate_true
        censoring_bias$relative_bias <- censoring_bias$bias / abs(censoring_bias$estimate_true)
      }
    }
  } else {
    effects <- NULL
    censoring_bias <- NULL
    observed_effects <- NULL
  }

  # prepare dual data architecture output
  result <- list(
    data = data_list,
    effects = effects,
    censoring_bias = censoring_bias,
    metadata = list(
      n = n,
      waves = waves,
      treatments = treatments,
      intervention_names = intervention_names,
      sampling_weights_applied = !is.null(sampling_weights),
      censoring_applied = apply_censoring,
      timestamp = Sys.time()
    )
  )
  
  # add dual data architecture when censoring/shadows applied
  if (apply_censoring) {
    # extract complete and observed data separately
    result$data_true <- lapply(data_list, function(x) x$complete)
    result$data_observed <- lapply(data_list, function(x) x$observed)
    
    # compute effects using new interface if we have the right structure
    if (length(intervention_names) >= 2 && !is.null(effects)) {
      # for now, keep the existing effects structure
      result$effects_true <- effects
      result$effects_observed <- observed_effects
    }
  }
  
  structure(result, class = "margot_causal_sim")
}

#' Compute true causal effects from simulated data
#' @keywords internal
compute_true_effects <- function(data_list, outcome_name, estimand = "ATE") {

  if (length(data_list) < 2) {
    stop("need at least 2 datasets to compute contrasts")
  }

  # extract outcome values
  outcomes <- lapply(data_list, function(d) d[[outcome_name]])

  # compute pairwise contrasts
  regime_names <- names(data_list)
  results <- list()

  for (i in 1:(length(regime_names) - 1)) {
    for (j in (i + 1):length(regime_names)) {

      y1 <- outcomes[[j]]
      y0 <- outcomes[[i]]

      # align samples (both must be non-missing)
      keep <- !is.na(y1) & !is.na(y0)
      y1_clean <- y1[keep]
      y0_clean <- y0[keep]

      if (estimand == "ATE") {
        effect <- mean(y1_clean) - mean(y0_clean)
        se <- sqrt(var(y1_clean)/length(y1_clean) + var(y0_clean)/length(y0_clean))
      } else {
        stop("only ATE currently implemented")
      }

      contrast_name <- paste0(regime_names[j], "_vs_", regime_names[i])

      results[[contrast_name]] <- data.frame(
        contrast = contrast_name,
        estimand = estimand,
        estimate = effect,
        se = se,
        ci_lower = effect - 1.96 * se,
        ci_upper = effect + 1.96 * se,
        n = length(y1_clean),
        stringsAsFactors = FALSE
      )
    }
  }

  do.call(rbind, results)
}

#' Print method for margot_causal_sim
#'
#' @param x A margot_causal_sim object
#' @param ... Additional arguments (currently unused)
#'
#' @return Invisibly returns the original object
#' @export
print.margot_causal_sim <- function(x, ...) {
  cat("margot causal simulation results\n")
  cat("--------------------------------\n")
  cat("sample size:", x$metadata$n, "\n")
  cat("waves:", x$metadata$waves, "\n")
  cat("interventions:", paste(x$metadata$intervention_names, collapse = ", "), "\n")

  if (x$metadata$sampling_weights_applied) {
    cat("sampling weights: applied\n")
  }

  if (x$metadata$censoring_applied) {
    cat("censoring: applied (complete and observed data available)\n")
    cat("data structure: dual architecture (data_true and data_observed)\n")
  }

  if (!is.null(x$effects)) {
    cat("\ntrue causal effects (from complete data):\n")
    print(x$effects, row.names = FALSE)
  }

  if (!is.null(x$censoring_bias)) {
    cat("\ncensoring bias analysis:\n")
    print(x$censoring_bias, row.names = FALSE)
  }

  invisible(x)
}