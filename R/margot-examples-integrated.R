#' Integrated Examples for margot Extensions
#'
#' @description
#' This file demonstrates complete workflows using the shadowing framework,
#' Monte Carlo simulations, and flexible distributions together.

# Complete Workflow Example ----------------------------------------------------

#' Complete workflow: Evaluating TMLE under complex shadowing
#'
#' @description
#' This example demonstrates:
#' 1. Setting up a realistic DGP with non-normal distributions
#' 2. Applying multiple types of shadows (censoring + measurement error)
#' 3. Evaluating an estimator (TMLE) via Monte Carlo
#' 4. Comparing performance under different shadowing scenarios
#'
#' @export
example_complete_workflow <- function(n_reps = 200) {

  cat("\n=========================================\n")
  cat("COMPLETE WORKFLOW: TMLE UNDER SHADOWING\n")
  cat("=========================================\n\n")

  # Step 1: Define the data generating process
  cat("Step 1: Setting up data generating process\n")
  cat("------------------------------------------\n")

  dgp_params <- list(
    waves = 4,
    treatments = "a",
    interventions = list(
      always = function(data, time, trt) {
        if (time == 0) return(data[[trt]])
        rep(1, nrow(data))
      },
      never = function(data, time, trt) {
        if (time == 0) return(data[[trt]])
        rep(0, nrow(data))
      }
    ),
    common_params = list(
      params = list(
        a_lag_y_coef = 0.3,      # True causal effect
        l_y_coef = 0.2,          # Confounder effect
        y_autoreg = 0.25,        # Outcome autocorrelation
        cens_a_coef = 0.4,       # Censoring depends on treatment
        cens_y_coef = 0.3        # Censoring depends on outcome
      ),
      censoring_params = list(
        rate = 0.15,
        exposure_dependence = TRUE,
        y_dependence = TRUE,
        latent_dependence = TRUE
      )
    )
  )

  cat("  - True causal effect: 0.3\n")
  cat("  - 4 waves of follow-up\n")
  cat("  - Censoring depends on treatment and outcome\n\n")

  # Step 2: Define shadowing scenarios
  cat("Step 2: Defining shadowing scenarios\n")
  cat("----------------------------------\n")

  shadowing_scenarios <- list(
    # Scenario 1: No additional shadowing (just censoring)
    none = list(),

    # Scenario 2: Classical measurement error in confounders
    classical_error = list(
      create_shadow(
        "measurement_error",
        params = list(
          variables = c("t1_l", "t2_l", "t3_l"),
          error_type = "classical",
          sigma = 0.5
        ),
        name = "classical_error_l"
      )
    ),

    # Scenario 3: Differential measurement error
    differential_error = list(
      create_shadow(
        "measurement_error",
        params = list(
          variables = c("t1_l", "t2_l", "t3_l"),
          error_type = "differential",
          differential_var = "t0_a",
          differential_fn = function(a) 0.3 + 0.4 * a  # More error if treated
        ),
        name = "differential_error_l"
      )
    ),

    # Scenario 4: Outcome dichotomization
    dichotomized = list(
      create_shadow(
        "measurement_error",
        params = list(
          variables = "t5_y",
          error_type = "dichotomise",
          threshold = 0  # Dichotomize at median
        ),
        name = "dichotomize_outcome"
      )
    ),

    # Scenario 5: Complex - multiple shadows
    complex = list(
      create_shadow(
        "measurement_error",
        params = list(
          variables = c("t1_l", "t2_l", "t3_l"),
          error_type = "classical",
          sigma = 0.3
        )
      ),
      create_shadow(
        "measurement_error",
        params = list(
          variables = "t5_y",
          error_type = "differential",
          differential_var = "t4_a",
          differential_fn = function(a) 0.2 + 0.3 * a
        )
      )
    )
  )

  cat("  Defined 5 shadowing scenarios:\n")
  for (name in names(shadowing_scenarios)) {
    cat(sprintf("  - %s: %d shadows\n", name, length(shadowing_scenarios[[name]])))
  }
  cat("\n")

  # Step 3: Define the estimator
  cat("Step 3: Defining the estimator (pseudo-TMLE)\n")
  cat("--------------------------------------------\n")

  # Pseudo-TMLE implementation (simplified for example)
  tmle_estimator <- function(data) {
    # In practice, would use lmtp or tmle package
    # This is a simplified version for demonstration

    # Check if we have enough data
    n_complete <- sum(!is.na(data$t5_y))
    if (n_complete < 50) {
      return(list(
        estimate = NA,
        se = NA,
        converged = FALSE,
        n_used = n_complete
      ))
    }

    # Step 1: Fit outcome model (simplified)
    outcome_formula <- t5_y ~ t4_a + t3_a + t2_a + t1_a +
      t4_l + t3_l + t2_l + t1_l +
      b1 + b2 + b3

    outcome_fit <- glm(outcome_formula,
                       data = data,
                       family = gaussian())

    # Step 2: Fit propensity scores (simplified)
    # In reality, would fit separate models for each time point
    prop_fit <- glm(t4_a ~ t3_l + t3_a + b1 + b2 + b3,
                    data = data,
                    family = binomial())

    # Step 3: Compute targeted estimate (very simplified)
    # Real TMLE would involve targeting step
    data$prop_score <- fitted(prop_fit)
    data$weight <- ifelse(data$t4_a == 1,
                          1/data$prop_score,
                          1/(1-data$prop_score))

    # Truncate weights
    data$weight <- pmin(data$weight, 10)

    # Weighted regression (simplified targeting)
    weighted_fit <- glm(outcome_formula,
                        data = data,
                        weights = weight,
                        family = gaussian())

    # Extract effect estimate
    coef_names <- names(coef(weighted_fit))
    a_coefs <- grep("^t[0-9]+_a$", coef_names, value = TRUE)

    if (length(a_coefs) > 0) {
      # Average treatment effect across time points
      effect <- mean(coef(weighted_fit)[a_coefs], na.rm = TRUE)

      # Bootstrap SE would be better, but for speed:
      se <- sqrt(mean(diag(vcov(weighted_fit))[a_coefs], na.rm = TRUE))

      return(list(
        estimate = effect,
        se = se,
        converged = TRUE,
        n_used = n_complete,
        avg_weight = mean(data$weight, na.rm = TRUE)
      ))
    } else {
      return(list(
        estimate = NA,
        se = NA,
        converged = FALSE,
        n_used = n_complete
      ))
    }
  }

  cat("  Using simplified TMLE estimator\n")
  cat("  (In practice, would use lmtp or tmle package)\n\n")

  # Step 4: Run Monte Carlo simulations
  cat("Step 4: Running Monte Carlo simulations\n")
  cat("---------------------------------------\n")

  results <- list()

  for (scenario_name in names(shadowing_scenarios)) {
    cat(sprintf("\nScenario: %s\n", scenario_name))
    cat("Running", n_reps, "replications...\n")

    mc_result <- margot_monte_carlo(
      n_reps = n_reps,
      n_per_rep = 1000,
      dgp_params = dgp_params,
      shadows = shadowing_scenarios[[scenario_name]],
      estimator_fn = tmle_estimator,
      truth_fn = function(data) 0.3,  # True effect
      parallel = FALSE,  # Set to TRUE for speed
      verbose = FALSE,
      seed = 12345 + which(names(shadowing_scenarios) == scenario_name)
    )

    results[[scenario_name]] <- mc_result

    # Print summary
    perf <- mc_result$performance
    cat(sprintf("  Bias: %.3f\n", perf$bias))
    cat(sprintf("  RMSE: %.3f\n", perf$rmse))
    cat(sprintf("  Coverage: %.1f%%\n", perf$coverage_95 * 100))
    cat(sprintf("  Avg retention: %.1f%%\n", perf$avg_retention * 100))
  }

  # Step 5: Compare results
  cat("\n\nStep 5: Comparing results across scenarios\n")
  cat("------------------------------------------\n")

  comparison <- compare_mc_results(
    results$none,
    results$classical_error,
    results$differential_error,
    results$dichotomized,
    results$complex,
    names = names(shadowing_scenarios)
  )

  print(comparison)

  # Step 6: Visualize results
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    cat("\nStep 6: Creating visualizations\n")
    cat("-------------------------------\n")

    # Combine results for plotting
    plot_data <- do.call(rbind, lapply(names(results), function(scenario) {
      df <- results[[scenario]]$results
      df$scenario <- scenario
      df
    }))

    # Bias plot
    p_bias <- ggplot2::ggplot(plot_data,
                              ggplot2::aes(x = scenario, y = estimate - truth)) +
      ggplot2::geom_boxplot(fill = "lightblue") +
      ggplot2::geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Bias Distribution by Scenario",
        x = "Shadowing Scenario",
        y = "Bias"
      ) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

    print(p_bias)

    # Sample size retention
    p_retention <- ggplot2::ggplot(plot_data,
                                   ggplot2::aes(x = scenario, y = n_effective/n_original)) +
      ggplot2::geom_boxplot(fill = "lightgreen") +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Sample Size Retention by Scenario",
        x = "Shadowing Scenario",
        y = "Proportion Retained"
      ) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

    print(p_retention)
  }

  # Return results
  invisible(list(
    results = results,
    comparison = comparison,
    scenarios = shadowing_scenarios
  ))
}

# Practical Example: Measurement Error Study -----------------------------------

#' Practical example: Impact of measurement error on different estimators
#'
#' @description
#' Compares how different estimators (OLS, IPW, G-computation) perform
#' under increasing levels of measurement error
#'
#' @export
example_measurement_error_comparison <- function() {

  cat("\n================================================\n")
  cat("MEASUREMENT ERROR IMPACT ON DIFFERENT ESTIMATORS\n")
  cat("================================================\n\n")

  # Define different levels of measurement error
  sigma_levels <- c(0, 0.25, 0.5, 0.75, 1.0)

  # Define three different estimators
  estimators <- list(
    # 1. Naive OLS
    ols = function(data) {
      fit <- lm(t3_y ~ t2_a + t1_a + t0_a + t2_l + t1_l + b1, data = data)
      list(
        estimate = coef(fit)["t2_a"],
        se = sqrt(diag(vcov(fit)))["t2_a"],
        method = "OLS"
      )
    },

    # 2. IPW (simplified)
    ipw = function(data) {
      # Fit propensity model
      ps_fit <- glm(t2_a ~ t2_l + t1_a + b1,
                    data = data,
                    family = binomial())

      data$ps <- fitted(ps_fit)
      data$ipw <- ifelse(data$t2_a == 1, 1/data$ps, 1/(1-data$ps))
      data$ipw <- pmin(data$ipw, 10)  # Truncate

      # Weighted outcome model
      weighted_fit <- lm(t3_y ~ t2_a, data = data, weights = ipw)

      list(
        estimate = coef(weighted_fit)["t2_a"],
        se = sqrt(diag(vcov(weighted_fit)))["t2_a"],
        method = "IPW"
      )
    },

    # 3. G-computation (parametric)
    gcomp = function(data) {
      # Fit outcome model
      outcome_fit <- lm(t3_y ~ t2_a + t1_a + t0_a + t2_l + t1_l + b1,
                        data = data)

      # Predict under interventions
      data1 <- data0 <- data
      data1$t2_a <- 1
      data0$t2_a <- 0

      Y1 <- predict(outcome_fit, newdata = data1)
      Y0 <- predict(outcome_fit, newdata = data0)

      effect <- mean(Y1 - Y0, na.rm = TRUE)

      # Bootstrap would be better for SE
      se <- sd(Y1 - Y0, na.rm = TRUE) / sqrt(sum(!is.na(Y1)))

      list(
        estimate = effect,
        se = se,
        method = "G-comp"
      )
    }
  )

  # Run simulations
  cat("Running simulations for each sigma level and estimator...\n\n")

  all_results <- list()

  for (sigma in sigma_levels) {
    cat(sprintf("Sigma = %.2f\n", sigma))

    # Create shadow
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

    for (est_name in names(estimators)) {
      cat(sprintf("  Running %s...", est_name))

      mc_result <- margot_monte_carlo(
        n_reps = 100,
        n_per_rep = 500,
        dgp_params = list(
          waves = 3,
          treatments = "a",
          interventions = list(
            natural = function(data, time, trt) data[[trt]]
          ),
          common_params = list(
            params = list(
              a_lag_y_coef = 0.3,
              l_a_coef = 0.3,      # L affects A
              l_y_coef = 0.3       # L affects Y (confounding)
            )
          )
        ),
        shadows = if (!is.null(shadow)) list(shadow) else list(),
        estimator_fn = estimators[[est_name]],
        truth_fn = function(data) 0.3,
        verbose = FALSE,
        seed = 2025
      )

      all_results[[paste0(est_name, "_", sigma)]] <- mc_result
      cat(" done\n")
    }
  }

  # Summarize results
  cat("\n\nRESULTS SUMMARY\n")
  cat("===============\n\n")

  # Create summary table
  summary_data <- do.call(rbind, lapply(names(all_results), function(name) {
    parts <- strsplit(name, "_")[[1]]
    method <- parts[1]
    sigma <- as.numeric(parts[2])

    perf <- all_results[[name]]$performance

    data.frame(
      method = method,
      sigma = sigma,
      bias = perf$bias,
      rmse = perf$rmse,
      coverage = perf$coverage_95,
      stringsAsFactors = FALSE
    )
  }))

  # Print by method
  for (method in names(estimators)) {
    cat(sprintf("\n%s Performance:\n", toupper(method)))
    method_data <- summary_data[summary_data$method == method, ]
    print(method_data[, -1], digits = 3, row.names = FALSE)
  }

  # Plot results
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    p <- ggplot2::ggplot(summary_data,
                         ggplot2::aes(x = sigma, y = abs(bias),
                                      color = method, group = method)) +
      ggplot2::geom_line(size = 1) +
      ggplot2::geom_point(size = 3) +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Absolute Bias vs Measurement Error",
        x = "Measurement Error SD (σ)",
        y = "Absolute Bias",
        color = "Method"
      ) +
      ggplot2::scale_color_brewer(palette = "Set1")

    print(p)
  }

  cat("\n\nKEY FINDINGS:\n")
  cat("- All estimators show increasing bias with measurement error\n")
  cat("- IPW may be more sensitive to measurement error in propensity score\n")
  cat("- G-computation relies on correct outcome model specification\n")

  invisible(list(
    results = all_results,
    summary = summary_data
  ))
}

# Helper Functions -------------------------------------------------------------

#' Quick diagnostic for a shadowed dataset
#'
#' @param original Original data
#' @param shadowed Shadowed data
#' @param key_vars Variables to focus on
#'
#' @export
diagnose_shadowing <- function(original, shadowed, key_vars = NULL) {

  if (is.null(key_vars)) {
    # Auto-detect key variables
    key_vars <- grep("^(t[0-9]+_[aly]|b[0-9]+)$",
                     names(original),
                     value = TRUE)
  }

  cat("SHADOWING DIAGNOSTIC REPORT\n")
  cat("=========================\n\n")

  # Overall sample size
  cat(sprintf("Original N: %d\n", nrow(original)))
  cat(sprintf("Shadowed N: %d (%.1f%% retained)\n",
              nrow(shadowed),
              100 * nrow(shadowed) / nrow(original)))

  # Missing data patterns
  cat("\nMissing Data Patterns:\n")
  for (var in key_vars) {
    if (var %in% names(shadowed)) {
      n_miss_orig <- sum(is.na(original[[var]]))
      n_miss_shadow <- sum(is.na(shadowed[[var]]))

      cat(sprintf("  %s: %d -> %d missing (%.1f%% -> %.1f%%)\n",
                  var,
                  n_miss_orig, n_miss_shadow,
                  100 * n_miss_orig / nrow(original),
                  100 * n_miss_shadow / nrow(shadowed)))
    }
  }

  # Distribution changes
  cat("\nDistribution Changes:\n")
  for (var in key_vars) {
    if (var %in% names(original) && var %in% names(shadowed)) {
      orig_vals <- original[[var]][!is.na(original[[var]])]
      shadow_vals <- shadowed[[var]][!is.na(shadowed[[var]])]

      if (length(orig_vals) > 0 && length(shadow_vals) > 0) {
        if (is.numeric(orig_vals)) {
          cat(sprintf("  %s:\n", var))
          cat(sprintf("    Mean: %.3f -> %.3f (shift: %.3f)\n",
                      mean(orig_vals), mean(shadow_vals),
                      mean(shadow_vals) - mean(orig_vals)))
          cat(sprintf("    SD: %.3f -> %.3f (ratio: %.3f)\n",
                      sd(orig_vals), sd(shadow_vals),
                      sd(shadow_vals) / sd(orig_vals)))
        }
      }
    }
  }

  # Check for added variables (_true versions)
  true_vars <- grep("_true$", names(shadowed), value = TRUE)
  if (length(true_vars) > 0) {
    cat("\nMeasurement Error Applied To:\n")
    for (var in true_vars) {
      orig_var <- sub("_true$", "", var)
      cat(sprintf("  %s\n", orig_var))
    }
  }
}

# Final Message ----------------------------------------------------------------

cat("\n===========================================\n")
cat("MARGOT EXTENSION COMPONENTS LOADED\n")
cat("===========================================\n\n")
cat("Available functions:\n")
cat("- Shadowing: create_shadow(), apply_shadow(), apply_shadows()\n")
cat("- Monte Carlo: margot_monte_carlo(), compare_mc_results()\n")
cat("- Distributions: create_distribution(), create_distribution_set()\n")
cat("- Examples: example_complete_workflow(), example_measurement_error_comparison()\n")
cat("\nRun example_complete_workflow() for a full demonstration.\n\n")
