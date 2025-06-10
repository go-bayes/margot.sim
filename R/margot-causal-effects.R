#' Compute Causal Effects from Data
#'
#' Calculates various causal estimands (ATE, ATT, ATU) from simulated or observed data.
#' This function assumes the data contains potential outcomes or can be used to compute
#' contrasts between treatment groups.
#'
#' @param data A data frame containing the outcome and exposure variables
#' @param exposure Character string naming the exposure/treatment variable
#' @param outcome Character string naming the outcome variable
#' @param covariates Character vector of covariate names (currently unused, for future HTE)
#' @param estimands Character vector specifying which estimands to compute
#' @param weights Optional weights for each observation
#'
#' @return A list containing the requested causal effects:
#' \itemize{
#'   \item ate: Average Treatment Effect E[Y(1) - Y(0)]
#'   \item att: Average Treatment on Treated E[Y(1) - Y(0) | A = 1]
#'   \item atu: Average Treatment on Untreated E[Y(1) - Y(0) | A = 0]
#' }
#'
#' @examples
#' # Generate simple data
#' set.seed(123)
#' n <- 1000
#' data <- data.frame(
#'   a = rbinom(n, 1, 0.5),
#'   y = rnorm(n, mean = 2)
#' )
#' data$y[data$a == 1] <- data$y[data$a == 1] + 0.5  # True ATE = 0.5
#'
#' # Compute effects
#' effects <- compute_causal_effects(data, exposure = "a", outcome = "y")
#' print(effects)
#'
#' @export
compute_causal_effects <- function(data,
                                   exposure,
                                   outcome,
                                   covariates = NULL,
                                   estimands = c("ate", "att", "atu"),
                                   weights = NULL) {
  # validate inputs
  if (!exposure %in% names(data)) {
    stop(paste0("Exposure variable '", exposure, "' not found in data"))
  }
  if (!outcome %in% names(data)) {
    stop(paste0("Outcome variable '", outcome, "' not found in data"))
  }
  
  # extract variables
  a <- data[[exposure]]
  y <- data[[outcome]]
  
  # handle weights
  if (is.null(weights)) {
    weights <- rep(1, nrow(data))
  } else if (length(weights) != nrow(data)) {
    stop("Weights must have same length as number of rows in data")
  }
  
  # ensure binary treatment for now
  # if treatment has measurement error, it might be continuous
  # for now, dichotomize at 0.5 if needed
  if (!all(a %in% c(0, 1, NA))) {
    warning("Non-binary treatment detected. Dichotomizing at 0.5 for effect estimation.")
    a <- as.numeric(a > 0.5)
  }
  
  # remove missing values
  complete_cases <- !is.na(a) & !is.na(y)
  a <- a[complete_cases]
  y <- y[complete_cases]
  weights <- weights[complete_cases]
  
  # compute weighted means
  treated_idx <- a == 1
  control_idx <- a == 0
  
  # weighted means
  y1_mean <- weighted.mean(y[treated_idx], weights[treated_idx], na.rm = TRUE)
  y0_mean <- weighted.mean(y[control_idx], weights[control_idx], na.rm = TRUE)
  
  # initialize results
  results <- list()
  
  # compute ate
  if ("ate" %in% estimands) {
    results$ate <- y1_mean - y0_mean
  }
  
  # compute att (for treated units only)
  if ("att" %in% estimands) {
    # for observational data, att = observed difference among treated
    # in experimental data with perfect compliance, att = ate
    # here we compute the simple difference for treated units
    results$att <- y1_mean - y0_mean
    # note: in a more sophisticated implementation, we would need
    # counterfactual outcomes y(0) for treated units
  }
  
  # compute atu (for untreated units only)  
  if ("atu" %in% estimands) {
    # for observational data, atu requires counterfactual y(1) for untreated
    # here we use the same effect as ate as a placeholder
    results$atu <- y1_mean - y0_mean
    # note: in a more sophisticated implementation, we would need
    # counterfactual outcomes y(1) for untreated units
  }
  
  # add metadata
  results$n_treated <- sum(treated_idx)
  results$n_control <- sum(control_idx)
  results$n_total <- length(a)
  
  # add class for methods
  class(results) <- c("margot_effects", "list")
  
  return(results)
}

#' Compare Shadow Bias in Causal Effects
#'
#' Compares causal effect estimates from true (unshadowed) data with estimates
#' from observed (shadowed) data to quantify bias introduced by observational
#' distortions.
#'
#' @param effects_true A list of true causal effects (from compute_causal_effects)
#' @param effects_observed A list of observed causal effects (from compute_causal_effects)
#' @param include_relative Logical, whether to include relative bias (default TRUE)
#'
#' @return A data frame with bias metrics for each estimand:
#' \itemize{
#'   \item estimand: The causal estimand (ate, att, atu)
#'   \item truth: True value of the estimand
#'   \item observed: Observed value of the estimand
#'   \item bias: Absolute bias (observed - truth)
#'   \item relative_bias: Relative bias as percentage of truth
#' }
#'
#' @examples
#' # Simulate true and observed effects
#' true_effects <- list(ate = 0.5, att = 0.6, atu = 0.4)
#' obs_effects <- list(ate = 0.3, att = 0.35, atu = 0.25)
#'
#' # Compare bias
#' bias_summary <- compare_shadow_bias(true_effects, obs_effects)
#' print(bias_summary)
#'
#' @export
compare_shadow_bias <- function(effects_true,
                                effects_observed,
                                include_relative = TRUE) {
  # get common estimands
  estimands <- intersect(names(effects_true), names(effects_observed))
  estimands <- setdiff(estimands, c("n_treated", "n_control", "n_total"))
  
  if (length(estimands) == 0) {
    stop("No common estimands found between true and observed effects")
  }
  
  # initialize results
  results <- data.frame(
    estimand = estimands,
    truth = numeric(length(estimands)),
    observed = numeric(length(estimands)),
    bias = numeric(length(estimands)),
    stringsAsFactors = FALSE
  )
  
  # compute bias for each estimand
  for (i in seq_along(estimands)) {
    est <- estimands[i]
    results$truth[i] <- effects_true[[est]]
    results$observed[i] <- effects_observed[[est]]
    results$bias[i] <- results$observed[i] - results$truth[i]
    
    if (include_relative && results$truth[i] != 0) {
      results$relative_bias[i] <- 100 * results$bias[i] / abs(results$truth[i])
    }
  }
  
  # add class for methods
  class(results) <- c("shadow_bias_comparison", "data.frame")
  
  return(results)
}

#' Print Method for Margot Effects
#'
#' @param x A margot_effects object
#' @param digits Number of digits to display
#' @param ... Additional arguments (ignored)
#'
#' @export
print.margot_effects <- function(x, digits = 3, ...) {
  cat("Causal Effect Estimates:\n")
  cat(paste0("  N (total): ", x$n_total, 
             " [Treated: ", x$n_treated, 
             ", Control: ", x$n_control, "]\n"))
  
  # print effects
  effects <- setdiff(names(x), c("n_treated", "n_control", "n_total"))
  for (effect in effects) {
    if (!is.null(x[[effect]])) {
      cat(paste0("  ", toupper(effect), ": ", 
                 round(x[[effect]], digits), "\n"))
    }
  }
  
  invisible(x)
}

#' Print Method for Shadow Bias Comparison
#'
#' @param x A shadow_bias_comparison object
#' @param digits Number of digits to display
#' @param ... Additional arguments (ignored)
#'
#' @export
print.shadow_bias_comparison <- function(x, digits = 3, ...) {
  cat("Shadow Bias Comparison:\n\n")
  
  # format the dataframe for printing
  print_df <- x
  print_df$truth <- round(print_df$truth, digits)
  print_df$observed <- round(print_df$observed, digits)
  print_df$bias <- round(print_df$bias, digits)
  
  if ("relative_bias" %in% names(print_df)) {
    print_df$relative_bias <- paste0(round(print_df$relative_bias, 1), "%")
  }
  
  print.data.frame(print_df, row.names = FALSE)
  
  invisible(x)
}

#' Extract Treatment and Outcome for Causal Effect Computation
#'
#' Helper function to extract treatment and outcome variables from
#' margot simulation data structure with proper time indexing.
#'
#' @param data Data frame from margot simulation
#' @param wave Which wave to use for treatment (default is first wave)
#' @param outcome_wave Which wave to use for outcome (default is last wave)
#' @param treatment_name Name of treatment variable (without time prefix)
#' @param outcome_name Name of outcome variable (without time prefix)
#'
#' @return A data frame with columns 'a' (treatment) and 'y' (outcome)
#' @keywords internal
extract_treatment_outcome <- function(data,
                                      wave = 1,
                                      outcome_wave = NULL,
                                      treatment_name = "a",
                                      outcome_name = "y") {
  # determine outcome wave if not specified
  if (is.null(outcome_wave)) {
    # find the last wave by looking at column names
    time_vars <- grep("^t[0-9]+_", names(data), value = TRUE)
    waves <- as.numeric(sub("^t([0-9]+)_.*", "\\1", time_vars))
    outcome_wave <- max(waves, na.rm = TRUE)
  }
  
  # construct variable names
  trt_var <- paste0("t", wave, "_", treatment_name)
  out_var <- paste0("t", outcome_wave, "_", outcome_name)
  
  # check if variables exist
  if (!trt_var %in% names(data)) {
    stop(paste0("Treatment variable '", trt_var, "' not found in data"))
  }
  if (!out_var %in% names(data)) {
    stop(paste0("Outcome variable '", out_var, "' not found in data"))
  }
  
  # extract and return
  result <- data.frame(
    a = data[[trt_var]],
    y = data[[out_var]],
    stringsAsFactors = FALSE
  )
  
  # add id if available
  if ("id" %in% names(data)) {
    result$id <- data$id
  }
  
  return(result)
}

#' Compute Causal Effects from Margot Simulation Data
#'
#' Wrapper function that extracts treatment and outcome from margot
#' simulation data structure and computes causal effects.
#'
#' @param sim_data Data from margot_simulate or margot_simulate_causal
#' @param wave Treatment wave (default 1)
#' @param outcome_wave Outcome wave (default is last wave)
#' @param treatment_name Name of treatment variable
#' @param outcome_name Name of outcome variable
#' @param estimands Which estimands to compute
#' @param weights Optional weights
#'
#' @return A margot_effects object
#' @export
compute_effects_from_sim <- function(sim_data,
                                     wave = 1,
                                     outcome_wave = NULL,
                                     treatment_name = "a",
                                     outcome_name = "y",
                                     estimands = c("ate", "att", "atu"),
                                     weights = NULL) {
  # extract treatment and outcome
  extracted <- extract_treatment_outcome(
    sim_data,
    wave = wave,
    outcome_wave = outcome_wave,
    treatment_name = treatment_name,
    outcome_name = outcome_name
  )
  
  # compute effects
  compute_causal_effects(
    extracted,
    exposure = "a",
    outcome = "y",
    estimands = estimands,
    weights = weights
  )
}