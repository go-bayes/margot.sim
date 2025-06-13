#' Positivity and Assumption Diagnostics
#'
#' @description
#' Functions for diagnosing positivity violations and other causal assumptions
#' in margot simulations.
#'
#' @details
#' Positivity (overlap) requires that all units have a non-zero probability
#' of receiving each treatment level given their covariates. Violations can
#' lead to biased or undefined causal estimates.

#' Check positivity violations in data
#'
#' @param data Data frame to check
#' @param treatment Character, name of treatment variable
#' @param covariates Character vector of covariate names
#' @param threshold Numeric, minimum probability threshold (default 0.01)
#' @param method Method for checking: "empirical", "model-based", or "both"
#'
#' @return Object of class "margot_positivity_diagnostic" with diagnostics
#' @export
#' @examples
#' # Create example data with potential positivity violation
#' set.seed(123)
#' n <- 1000
#' data <- data.frame(
#'   x1 = rnorm(n),
#'   x2 = rnorm(n),
#'   # treatment depends strongly on x1
#'   a = rbinom(n, 1, plogis(3 * x1)),
#'   y = rnorm(n)
#' )
#' 
#' # Check positivity
#' pos_check <- margot_check_positivity(
#'   data = data,
#'   treatment = "a",
#'   covariates = c("x1", "x2")
#' )
#' print(pos_check)
#' plot(pos_check)
margot_check_positivity <- function(data, 
                                  treatment, 
                                  covariates,
                                  threshold = 0.01,
                                  method = c("empirical", "model-based", "both")) {
  method <- match.arg(method)
  
  # validate inputs
  if (!treatment %in% names(data)) {
    stop("Treatment variable not found in data")
  }
  
  missing_covs <- setdiff(covariates, names(data))
  if (length(missing_covs) > 0) {
    stop("Covariates not found in data: ", paste(missing_covs, collapse = ", "))
  }
  
  # get treatment levels
  treatment_vals <- sort(unique(data[[treatment]]))
  n_levels <- length(treatment_vals)
  
  if (n_levels < 2) {
    stop("Treatment must have at least 2 levels")
  }
  
  results <- list()
  
  # empirical check
  if (method %in% c("empirical", "both")) {
    results$empirical <- check_positivity_empirical(
      data, treatment, covariates, treatment_vals, threshold
    )
  }
  
  # model-based check
  if (method %in% c("model-based", "both")) {
    results$model_based <- check_positivity_model(
      data, treatment, covariates, treatment_vals, threshold
    )
  }
  
  # summary statistics
  results$summary <- summarize_positivity(data, treatment, covariates, treatment_vals)
  
  # metadata
  results$metadata <- list(
    n = nrow(data),
    treatment = treatment,
    covariates = covariates,
    treatment_levels = treatment_vals,
    threshold = threshold,
    method = method
  )
  
  class(results) <- "margot_positivity_diagnostic"
  results
}

#' Empirical positivity check
#' @keywords internal
check_positivity_empirical <- function(data, treatment, covariates, treatment_vals, threshold) {
  n <- nrow(data)
  violations <- list()
  
  # create bins for continuous covariates
  covariate_bins <- list()
  for (cov in covariates) {
    if (is.numeric(data[[cov]])) {
      # use quantiles for binning
      n_bins <- min(10, ceiling(sqrt(n / 20)))
      covariate_bins[[cov]] <- cut(data[[cov]], 
                                  breaks = quantile(data[[cov]], 
                                                  probs = seq(0, 1, length.out = n_bins + 1),
                                                  na.rm = TRUE),
                                  include.lowest = TRUE)
    } else {
      covariate_bins[[cov]] <- data[[cov]]
    }
  }
  
  # create covariate strata
  if (length(covariates) > 0) {
    strata <- do.call(interaction, covariate_bins)
  } else {
    strata <- rep(1, n)
  }
  
  # check each stratum
  stratum_table <- table(strata, data[[treatment]])
  stratum_props <- prop.table(stratum_table, 1)
  
  # find violations
  for (i in 1:nrow(stratum_props)) {
    stratum_n <- sum(stratum_table[i, ])
    if (stratum_n > 0) {
      for (j in 1:ncol(stratum_props)) {
        if (stratum_props[i, j] < threshold) {
          violations[[length(violations) + 1]] <- list(
            stratum = rownames(stratum_props)[i],
            treatment_level = colnames(stratum_props)[j],
            probability = stratum_props[i, j],
            n_units = stratum_table[i, j],
            n_stratum = stratum_n
          )
        }
      }
    }
  }
  
  list(
    violations = violations,
    n_violations = length(violations),
    stratum_table = stratum_table,
    stratum_props = stratum_props
  )
}

#' Model-based positivity check
#' @keywords internal
check_positivity_model <- function(data, treatment, covariates, treatment_vals, threshold) {
  # fit propensity score model
  if (length(treatment_vals) == 2) {
    # binary treatment
    formula <- as.formula(paste(treatment, "~", paste(covariates, collapse = " + ")))
    ps_model <- glm(formula, data = data, family = binomial())
    
    # predicted probabilities
    ps <- predict(ps_model, type = "response")
    
    # check for violations
    violations_treat1 <- which(ps < threshold)
    violations_treat0 <- which((1 - ps) < threshold)
    
    list(
      model = ps_model,
      propensity_scores = ps,
      violations = list(
        treat1 = violations_treat1,
        treat0 = violations_treat0
      ),
      n_violations = length(violations_treat1) + length(violations_treat0),
      min_ps = min(ps),
      max_ps = max(ps)
    )
  } else {
    # multinomial treatment - use multinomial logit or other approach
    warning("Model-based check for non-binary treatments not yet implemented")
    NULL
  }
}

#' Summarize positivity diagnostics
#' @keywords internal
summarize_positivity <- function(data, treatment, covariates, treatment_vals) {
  n <- nrow(data)
  
  # treatment distribution
  treat_table <- table(data[[treatment]])
  treat_props <- prop.table(treat_table)
  
  # covariate balance summary
  balance_stats <- list()
  for (cov in covariates) {
    if (is.numeric(data[[cov]])) {
      # means by treatment
      means <- tapply(data[[cov]], data[[treatment]], mean, na.rm = TRUE)
      sds <- tapply(data[[cov]], data[[treatment]], sd, na.rm = TRUE)
      
      balance_stats[[cov]] <- list(
        type = "continuous",
        means = means,
        sds = sds,
        standardized_diff = (means[2] - means[1]) / sqrt((sds[1]^2 + sds[2]^2) / 2)
      )
    } else {
      # proportions by treatment
      props <- prop.table(table(data[[cov]], data[[treatment]]), 2)
      balance_stats[[cov]] <- list(
        type = "categorical",
        proportions = props
      )
    }
  }
  
  list(
    n = n,
    treatment_distribution = treat_props,
    covariate_balance = balance_stats
  )
}

#' Print method for positivity diagnostics
#'
#' @param x margot_positivity_diagnostic object
#' @param ... Additional arguments
#' @export
print.margot_positivity_diagnostic <- function(x, ...) {
  cat("Margot Positivity Diagnostic\n")
  cat("============================\n\n")
  
  cat(sprintf("Sample size: %d\n", x$metadata$n))
  cat(sprintf("Treatment: %s (%d levels)\n", 
              x$metadata$treatment, 
              length(x$metadata$treatment_levels)))
  cat(sprintf("Covariates: %s\n", 
              paste(x$metadata$covariates, collapse = ", ")))
  cat(sprintf("Threshold: %.3f\n\n", x$metadata$threshold))
  
  # treatment distribution
  cat("Treatment Distribution:\n")
  print(round(x$summary$treatment_distribution, 3))
  cat("\n")
  
  # empirical violations
  if (!is.null(x$empirical)) {
    cat(sprintf("Empirical Violations: %d\n", x$empirical$n_violations))
    
    if (x$empirical$n_violations > 0) {
      cat("\nTop violations:\n")
      # show first 5
      for (i in 1:min(5, x$empirical$n_violations)) {
        v <- x$empirical$violations[[i]]
        cat(sprintf("  Stratum %s, Treatment %s: p=%.3f (n=%d/%d)\n",
                   v$stratum, v$treatment_level, v$probability,
                   v$n_units, v$n_stratum))
      }
      if (x$empirical$n_violations > 5) {
        cat(sprintf("  ... and %d more\n", x$empirical$n_violations - 5))
      }
    }
  }
  
  # model-based results
  if (!is.null(x$model_based)) {
    cat(sprintf("\nModel-based Violations: %d\n", x$model_based$n_violations))
    cat(sprintf("Propensity score range: [%.3f, %.3f]\n",
               x$model_based$min_ps, x$model_based$max_ps))
  }
  
  invisible(x)
}

#' Plot positivity diagnostics
#'
#' @param x margot_positivity_diagnostic object
#' @param type Type of plot: "propensity", "overlap", "balance"
#' @param ... Additional arguments
#' @export
plot.margot_positivity_diagnostic <- function(x, type = "propensity", ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package required for plotting")
  }
  
  type <- match.arg(type, c("propensity", "overlap", "balance"))
  
  if (type == "propensity" && !is.null(x$model_based)) {
    # propensity score distribution
    ps_data <- data.frame(
      ps = x$model_based$propensity_scores,
      treatment = x$metadata$data[[x$metadata$treatment]]
    )
    
    p <- ggplot2::ggplot(ps_data, ggplot2::aes(x = ps, fill = factor(treatment))) +
      ggplot2::geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
      ggplot2::geom_vline(xintercept = x$metadata$threshold, 
                         linetype = "dashed", color = "red") +
      ggplot2::geom_vline(xintercept = 1 - x$metadata$threshold, 
                         linetype = "dashed", color = "red") +
      ggplot2::labs(
        title = "Propensity Score Distribution",
        x = "Propensity Score",
        y = "Count",
        fill = "Treatment"
      ) +
      ggplot2::theme_minimal()
    
    return(p)
  }
  
  # other plot types would be implemented here
  message(sprintf("Plot type '%s' not yet implemented", type))
  invisible(NULL)
}

#' Create positivity shadow based on diagnostics
#'
#' @param diagnostic margot_positivity_diagnostic object
#' @param method How to create shadow: "trim", "truncate", or "custom"
#' @return Shadow object that enforces positivity
#' @export
create_positivity_shadow_from_diagnostic <- function(diagnostic, 
                                                   method = c("trim", "truncate", "custom")) {
  method <- match.arg(method)
  
  if (is.null(diagnostic$model_based)) {
    stop("Model-based diagnostic required to create positivity shadow")
  }
  
  # get violation indices
  violations <- c(
    diagnostic$model_based$violations$treat1,
    diagnostic$model_based$violations$treat0
  )
  
  if (length(violations) == 0) {
    message("No positivity violations found")
    return(NULL)
  }
  
  # create shadow based on method
  if (method == "trim") {
    # remove units with violations
    shadow <- create_shadow(
      type = "positivity",
      params = list(
        method = "trim",
        threshold = diagnostic$metadata$threshold,
        trim_indices = violations,
        exposure_var = diagnostic$metadata$treatment,
        filter_fn = function(data) {
          data[-violations, ]
        }
      ),
      name = "positivity_trim"
    )
  } else if (method == "truncate") {
    # truncate propensity scores
    shadow <- create_shadow(
      type = "positivity", 
      params = list(
        method = "truncate",
        threshold = diagnostic$metadata$threshold,
        lower = diagnostic$metadata$threshold,
        upper = 1 - diagnostic$metadata$threshold,
        exposure_var = diagnostic$metadata$treatment,
        filter_fn = function(data) data
      ),
      name = "positivity_truncate"
    )
  }
  
  shadow
}

#' Diagnose all shadowing effects
#'
#' @param data Original data
#' @param shadowed_data Data after applying shadows
#' @param shadows List of applied shadows
#' @return Comprehensive diagnostics
#' @export
diagnose_all_shadows <- function(data, shadowed_data, shadows) {
  diagnostics <- list()
  
  # data dimensions
  diagnostics$data_loss <- list(
    n_original = nrow(data),
    n_shadowed = nrow(shadowed_data),
    prop_retained = nrow(shadowed_data) / nrow(data),
    vars_original = ncol(data),
    vars_shadowed = ncol(shadowed_data)
  )
  
  # shadow-specific diagnostics
  diagnostics$shadow_diagnostics <- lapply(shadows, function(shadow) {
    list(
      type = shadow$type,
      name = shadow$name,
      params = shadow$params
    )
  })
  
  # missingness patterns
  if (any(is.na(shadowed_data))) {
    diagnostics$missingness <- margot_missingness_summary(shadowed_data)
  }
  
  # variable-level changes
  common_vars <- intersect(names(data), names(shadowed_data))
  var_changes <- list()
  
  for (var in common_vars) {
    if (is.numeric(data[[var]])) {
      var_changes[[var]] <- list(
        mean_change = mean(shadowed_data[[var]], na.rm = TRUE) - mean(data[[var]], na.rm = TRUE),
        sd_change = sd(shadowed_data[[var]], na.rm = TRUE) - sd(data[[var]], na.rm = TRUE),
        cor_with_original = cor(data[[var]], shadowed_data[[var]], use = "complete.obs")
      )
    }
  }
  
  diagnostics$variable_changes <- var_changes
  
  class(diagnostics) <- "margot_shadow_diagnostics"
  diagnostics
}