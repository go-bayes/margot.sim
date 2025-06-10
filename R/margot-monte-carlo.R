#' Monte Carlo Framework for margot
#'
#' @description
#' This file implements Monte Carlo simulation capabilities for evaluating
#' statistical estimators under various shadowing conditions.

# Main Monte Carlo Function ----------------------------------------------------

#' Run Monte Carlo simulation to evaluate estimator performance
#'
#' @param n_reps Number of Monte Carlo replications
#' @param n_per_rep Sample size per replication
#' @param dgp_params List of parameters for margot_simulate_causal()
#' @param shadows List of shadow objects to apply to the data
#' @param estimator_fn Function that takes data and returns estimate(s)
#' @param truth_fn Optional function to compute true parameter value
#' @param extract_fn Optional function to extract additional information
#' @param parallel Logical, use parallel processing?
#' @param n_cores Number of cores for parallel processing
#' @param seed Random seed
#' @param verbose Print progress?
#' @param save_data Logical, save datasets from each replication?
#'
#' @return Object of class "margot_mc_results" with simulation results
#' @export
#'
#' @examples
#' \dontrun{
#' # Define estimator
#' my_estimator <- function(data) {
#'   fit <- lm(t3_y ~ t2_a + t1_a + b1, data = data)
#'   list(
#'     estimate = coef(fit)["t2_a"],
#'     se = sqrt(diag(vcov(fit)))["t2_a"],
#'     converged = TRUE
#'   )
#' }
#'
#' # Run simulation
#' results <- margot_monte_carlo(
#'   n_reps = 500,
#'   n_per_rep = 1000,
#'   dgp_params = list(
#'     waves = 3,
#'     params = list(a_lag_y_coef = 0.3)
#'   ),
#'   estimator_fn = my_estimator
#' )
#' }
margot_monte_carlo <- function(
    n_reps = 500,
    n_per_rep = 1000,
    dgp_params = list(),
    shadows = list(),
    estimator_fn = NULL,
    truth_fn = NULL,
    extract_fn = NULL,
    parallel = FALSE,
    n_cores = NULL,
    seed = NULL,
    verbose = TRUE,
    save_data = FALSE
) {

  # Input validation
  if (is.null(estimator_fn)) {
    stop("estimator_fn must be provided")
  }

  if (!is.list(shadows) && !is.null(shadows)) {
    shadows <- list(shadows)
  }

  # Set seed for reproducibility
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Store start time
  start_time <- Sys.time()

  # Create progress bar if verbose
  if (verbose && !parallel) {
    pb <- txtProgressBar(min = 0, max = n_reps, style = 3)
  }

  # Function for one replication
  run_one_rep <- function(rep_id) {
    # Set seed for this replication
    if (!is.null(seed)) {
      set.seed(seed + rep_id)
    }

    # Track timing
    rep_start <- Sys.time()

    # Generate data
    # Extract common_params if it contains params
    if ("params" %in% names(dgp_params)) {
      common_params_for_sim <- list(params = dgp_params$params)
      dgp_params_clean <- dgp_params[!names(dgp_params) %in% "params"]
    } else {
      common_params_for_sim <- list()
      dgp_params_clean <- dgp_params
    }
    
    sim_args <- modifyList(
      list(n = n_per_rep, apply_censoring = TRUE, common_params = common_params_for_sim),
      dgp_params_clean
    )

    sim_results <- do.call(margot_simulate_causal, sim_args)

    # Extract complete and observed data
    # Handle different output structures
    if ("data" %in% names(sim_results)) {
      # From margot_simulate_causal
      intervention_names <- names(sim_results$data)
      complete_data <- sim_results$data[[1]]$complete
      observed_data <- sim_results$data[[1]]$observed

      # Get true effect if available
      if (!is.null(sim_results$effects)) {
        true_effect <- sim_results$effects$estimate[1]
      } else {
        true_effect <- NA
      }
    } else {
      # Direct from margot_simulate
      complete_data <- sim_results
      observed_data <- sim_results
      true_effect <- NA
    }

    # Apply additional shadows
    shadowed_data <- if (length(shadows) > 0) {
      apply_shadows(observed_data, shadows)
    } else {
      observed_data
    }

    # Calculate truth if function provided
    if (!is.null(truth_fn)) {
      truth <- truth_fn(complete_data)
    } else {
      truth <- true_effect
    }

    # Apply estimator with error handling
    est_result <- tryCatch(
      estimator_fn(shadowed_data),
      error = function(e) {
        list(
          estimate = NA,
          se = NA,
          converged = FALSE,
          error = as.character(e)
        )
      }
    )

    # Ensure est_result is a list
    if (!is.list(est_result)) {
      est_result <- list(estimate = est_result)
    }

    # Extract additional information if requested
    extra_info <- if (!is.null(extract_fn)) {
      extract_fn(shadowed_data, est_result)
    } else {
      list()
    }

    # Calculate effective sample size
    n_effective <- sum(!is.na(shadowed_data[[ncol(shadowed_data)]]))

    # Compile results
    result <- c(
      list(
        rep_id = rep_id,
        truth = truth,
        n_original = n_per_rep,
        n_effective = n_effective,
        computation_time = as.numeric(Sys.time() - rep_start, units = "secs")
      ),
      est_result,
      extra_info
    )

    # Optionally save data
    if (save_data) {
      result$data <- list(
        complete = complete_data,
        shadowed = shadowed_data
      )
    }

    # Update progress bar
    if (verbose && !parallel && exists("pb")) {
      setTxtProgressBar(pb, rep_id)
    }

    result
  }

  # Run replications
  if (parallel) {
    if (is.null(n_cores)) {
      n_cores <- parallel::detectCores() - 1
    }

    if (verbose) {
      cat(sprintf("Running %d replications in parallel on %d cores...\n",
                  n_reps, n_cores))
    }

    # Check if parallel backend is already registered
    if (!requireNamespace("doParallel", quietly = TRUE)) {
      warning("doParallel package not available. Falling back to sequential processing.")
      parallel <- FALSE
    } else {
      # Set up parallel backend
      cl <- parallel::makeCluster(n_cores)
      on.exit(parallel::stopCluster(cl))
      doParallel::registerDoParallel(cl)
      
      # Export necessary objects to cluster
      parallel::clusterExport(cl, c("margot_simulate", "margot_simulate_causal",
                                    "apply_shadows", "apply_shadow"),
                              envir = environment())

      results_list <- parallel::parLapply(cl, 1:n_reps, run_one_rep)
    }
  }
  
  # Run sequentially if parallel is FALSE (either originally or due to missing package)
  if (!parallel) {
    results_list <- lapply(1:n_reps, run_one_rep)
  }

  if (verbose && !parallel) {
    close(pb)
  }

  # Convert to data frame
  results_df <- do.call(rbind, lapply(results_list, function(x) {
    # Remove data element if present
    x$data <- NULL
    as.data.frame(x, stringsAsFactors = FALSE)
  }))

  # Calculate performance metrics
  performance <- calculate_performance_metrics(results_df)

  # Calculate computation time
  total_time <- as.numeric(Sys.time() - start_time, units = "secs")

  # Create output object
  output <- structure(
    list(
      results = results_df,
      performance = performance,
      params = list(
        n_reps = n_reps,
        n_per_rep = n_per_rep,
        dgp_params = dgp_params,
        shadows = shadows,
        seed = seed,
        computation_time = total_time
      ),
      data = if (save_data) lapply(results_list, "[[", "data") else NULL
    ),
    class = "margot_mc_results"
  )

  if (verbose) {
    cat(sprintf("\nCompleted %d replications in %.1f seconds\n",
                n_reps, total_time))
    print(performance)
  }

  output
}

# Performance Metrics ----------------------------------------------------------

#' Calculate performance metrics from MC results
#' @keywords internal
calculate_performance_metrics <- function(results_df) {

  # Check for required columns
  if (!"estimate" %in% names(results_df)) {
    warning("No 'estimate' column found in results")
    return(list())
  }

  # Remove failed replications
  converged <- if ("converged" %in% names(results_df)) {
    results_df$converged
  } else {
    !is.na(results_df$estimate)
  }

  valid_results <- results_df[converged, ]
  n_valid <- nrow(valid_results)

  if (n_valid == 0) {
    warning("No valid estimates found")
    return(list(convergence_rate = 0))
  }

  metrics <- list(
    convergence_rate = n_valid / nrow(results_df),
    n_valid = n_valid
  )

  # If truth is available, calculate bias metrics
  if ("truth" %in% names(valid_results) && !all(is.na(valid_results$truth))) {
    metrics$bias <- mean(valid_results$estimate - valid_results$truth, na.rm = TRUE)
    metrics$variance <- var(valid_results$estimate, na.rm = TRUE)
    metrics$mse <- mean((valid_results$estimate - valid_results$truth)^2, na.rm = TRUE)
    metrics$rmse <- sqrt(metrics$mse)
    metrics$relative_bias <- metrics$bias / mean(abs(valid_results$truth), na.rm = TRUE)

    # Coverage if SE available
    if ("se" %in% names(valid_results)) {
      ci_lower <- valid_results$estimate - 1.96 * valid_results$se
      ci_upper <- valid_results$estimate + 1.96 * valid_results$se

      coverage <- mean(valid_results$truth >= ci_lower &
                         valid_results$truth <= ci_upper, na.rm = TRUE)
      metrics$coverage_95 <- coverage

      # Average CI width
      metrics$avg_ci_width <- mean(ci_upper - ci_lower, na.rm = TRUE)
    }
  } else {
    # Without truth, just report distribution of estimates
    metrics$mean_estimate <- mean(valid_results$estimate, na.rm = TRUE)
    metrics$sd_estimate <- sd(valid_results$estimate, na.rm = TRUE)
  }

  # Sample size retention
  if (all(c("n_original", "n_effective") %in% names(valid_results))) {
    metrics$avg_retention <- mean(valid_results$n_effective / valid_results$n_original)
  }

  # Computation time
  if ("computation_time" %in% names(valid_results)) {
    metrics$avg_computation_time <- mean(valid_results$computation_time)
  }

  metrics
}

# Print and Summary Methods ----------------------------------------------------

#' Print method for Monte Carlo results
#'
#' @param x A margot_mc_results object
#' @param digits Integer. Number of digits to display
#' @param ... Additional arguments (currently unused)
#'
#' @return Invisibly returns the original object
#' @export
print.margot_mc_results <- function(x, digits = 4, ...) {
  cat("Monte Carlo Simulation Results\n")
  cat("==============================\n\n")

  # Simulation parameters
  cat("Simulation Setup:\n")
  cat(sprintf("  Replications: %d\n", x$params$n_reps))
  cat(sprintf("  Sample size per rep: %d\n", x$params$n_per_rep))

  if (length(x$params$shadows) > 0) {
    cat(sprintf("  Shadows applied: %d\n", length(x$params$shadows)))
    shadow_names <- sapply(x$params$shadows, function(m) m$name)
    cat(sprintf("    - %s\n", shadow_names))
  }

  cat(sprintf("  Total computation time: %.1f seconds\n", x$params$computation_time))

  # Performance metrics
  cat("\nPerformance Metrics:\n")
  perf <- x$performance

  cat(sprintf("  Convergence rate: %.1f%%\n", perf$convergence_rate * 100))

  if (!is.null(perf$bias)) {
    cat(sprintf("  Bias: %.*f\n", digits, perf$bias))
    cat(sprintf("  Variance: %.*f\n", digits, perf$variance))
    cat(sprintf("  RMSE: %.*f\n", digits, perf$rmse))
    cat(sprintf("  Relative bias: %.1f%%\n", perf$relative_bias * 100))
  }

  if (!is.null(perf$coverage_95)) {
    cat(sprintf("  95%% CI coverage: %.1f%%\n", perf$coverage_95 * 100))
    cat(sprintf("  Average CI width: %.*f\n", digits, perf$avg_ci_width))
  }

  if (!is.null(perf$avg_retention)) {
    cat(sprintf("  Average sample retention: %.1f%%\n", perf$avg_retention * 100))
  }

  invisible(x)
}

#' Summary method for Monte Carlo results
#'
#' @param object A margot_mc_results object
#' @param ... Additional arguments (currently unused)
#'
#' @return A summary object with class "summary.margot_mc_results"
#' @export
summary.margot_mc_results <- function(object, ...) {
  structure(
    list(
      performance = object$performance,
      params = object$params,
      results_summary = summary(object$results[, c("estimate", "truth", "n_effective")])
    ),
    class = "summary.margot_mc_results"
  )
}

# Diagnostic Functions ---------------------------------------------------------

#' Plot Monte Carlo results
#'
#' @param x A margot_mc_results object
#' @param type Character string specifying plot type: "histogram", "qq", "trace", or "bias"
#' @param ... Additional arguments passed to ggplot2 functions
#'
#' @return A ggplot2 plot object
#' @export
plot.margot_mc_results <- function(x, type = c("histogram", "qq", "trace", "bias"), ...) {
  type <- match.arg(type)

  # Requires ggplot2
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package required for plotting")
  }

  valid_results <- x$results[!is.na(x$results$estimate), ]

  if (type == "histogram") {
    p <- ggplot2::ggplot(valid_results, ggplot2::aes(x = estimate)) +
      ggplot2::geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Distribution of Estimates",
        x = "Estimate",
        y = "Frequency"
      )

    if ("truth" %in% names(valid_results) && !all(is.na(valid_results$truth))) {
      true_val <- mean(valid_results$truth, na.rm = TRUE)
      p <- p + ggplot2::geom_vline(xintercept = true_val,
                                   color = "red", linetype = "dashed", size = 1)
    }

  } else if (type == "qq") {
    if (!"truth" %in% names(valid_results) || all(is.na(valid_results$truth))) {
      stop("QQ plot requires truth values")
    }

    # Standardize estimates
    standardized <- (valid_results$estimate - valid_results$truth) /
      sd(valid_results$estimate)

    p <- ggplot2::ggplot(data.frame(sample = standardized), ggplot2::aes(sample = sample)) +
      ggplot2::stat_qq() +
      ggplot2::stat_qq_line() +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "QQ Plot of Standardized Estimates",
        x = "Theoretical Quantiles",
        y = "Sample Quantiles"
      )

  } else if (type == "trace") {
    p <- ggplot2::ggplot(valid_results, ggplot2::aes(x = rep_id, y = estimate)) +
      ggplot2::geom_line(alpha = 0.5) +
      ggplot2::geom_smooth(method = "loess", se = FALSE, color = "blue") +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Trace Plot of Estimates",
        x = "Replication",
        y = "Estimate"
      )

  } else if (type == "bias") {
    if (!"truth" %in% names(valid_results) || all(is.na(valid_results$truth))) {
      stop("Bias plot requires truth values")
    }

    valid_results$bias <- valid_results$estimate - valid_results$truth

    p <- ggplot2::ggplot(valid_results, ggplot2::aes(x = n_effective, y = bias)) +
      ggplot2::geom_point(alpha = 0.5) +
      ggplot2::geom_smooth(method = "loess", se = TRUE) +
      ggplot2::geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Bias vs Effective Sample Size",
        x = "Effective Sample Size",
        y = "Bias"
      )
  }

  p
}

# Comparison Functions ---------------------------------------------------------

#' Compare multiple Monte Carlo results
#'
#' @param ... Multiple margot_mc_results objects
#' @param names Optional names for each result set
#'
#' @return Comparison object
#' @export
compare_mc_results <- function(..., names = NULL) {
  results_list <- list(...)

  if (is.null(names)) {
    names <- paste0("Method", seq_along(results_list))
  }

  # Extract performance metrics
  comparison <- do.call(rbind, lapply(seq_along(results_list), function(i) {
    perf <- results_list[[i]]$performance
    data.frame(
      method = names[i],
      bias = perf$bias %||% NA,
      variance = perf$variance %||% NA,
      rmse = perf$rmse %||% NA,
      coverage = perf$coverage_95 %||% NA,
      stringsAsFactors = FALSE
    )
  }))

  class(comparison) <- c("mc_comparison", "data.frame")
  comparison
}

#' Print method for MC comparison
#'
#' @param x An mc_comparison object from compare_mc_results()
#' @param digits Integer. Number of digits to display
#' @param ... Additional arguments (currently unused)
#'
#' @return Invisibly returns the original object
#' @export
print.mc_comparison <- function(x, digits = 4, ...) {
  cat("Monte Carlo Method Comparison\n")
  cat("=============================\n\n")

  # Format for printing
  x_print <- x
  numeric_cols <- c("bias", "variance", "rmse", "coverage")

  for (col in numeric_cols) {
    if (col %in% names(x_print)) {
      if (col == "coverage") {
        x_print[[col]] <- sprintf("%.1f%%", x_print[[col]] * 100)
      } else {
        x_print[[col]] <- sprintf("%.*f", digits, x_print[[col]])
      }
    }
  }

  print.data.frame(x_print, row.names = FALSE)

  invisible(x)
}

# Example Usage ----------------------------------------------------------------

#' Example: Evaluate estimator under measurement error
#' @export
example_mc_measurement_error <- function() {
  cat("Example: Evaluating OLS under measurement error\n")
  cat("==============================================\n\n")

  # Define shadows with increasing measurement error
  sigma_values <- c(0, 0.5, 1.0)

  results_list <- list()

  for (sigma in sigma_values) {
    cat(sprintf("Running simulation with sigma = %.1f...\n", sigma))

    # Create measurement error shadow
    if (sigma > 0) {
      shadow <- create_shadow(
        "measurement_error",
        params = list(
          variables = c("t1_l", "t2_l"),
          error_type = "classical",
          sigma = sigma
        )
      )
    } else {
      shadow <- NULL
    }

    # Define OLS estimator
    ols_estimator <- function(data) {
      fit <- lm(t3_y ~ t2_a + t1_a + t2_l + t1_l + b1, data = data)

      list(
        estimate = coef(fit)["t2_a"],
        se = sqrt(diag(vcov(fit)))["t2_a"],
        converged = !any(is.na(coef(fit)))
      )
    }

    # Run Monte Carlo
    mc_result <- margot_monte_carlo(
      n_reps = 200,
      n_per_rep = 500,
      dgp_params = list(
        waves = 3,
        treatments = "a",
        interventions = list(
          natural = function(data, time, trt) data[[trt]]
        ),
        common_params = list(
          params = list(
            a_lag_y_coef = 0.3,  # True effect
            l_y_coef = 0.2       # L affects Y
          )
        )
      ),
      shadows = if (!is.null(shadow)) list(shadow) else list(),
      estimator_fn = ols_estimator,
      truth_fn = function(data) 0.3,  # True parameter value
      verbose = FALSE
    )

    results_list[[paste0("sigma_", sigma)]] <- mc_result
  }

  # Compare results
  comparison <- compare_mc_results(
    results_list[[1]],
    results_list[[2]],
    results_list[[3]],
    names = paste0("sigma = ", sigma_values)
  )

  print(comparison)

  invisible(results_list)
}
