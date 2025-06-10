#' Example: Heterogeneous Treatment Effects by Baseline Characteristics
#'
#' Demonstrates how to simulate and analyze treatment effect heterogeneity
#' by baseline covariates, baseline outcome, and baseline exposure.
#'
#' @importFrom dplyr %>%
#' @param n Sample size (default 2000)
#' @param waves Number of waves (default 2)
#' @param seed Random seed (default 2025)
#' @param plot Logical, whether to create diagnostic plots (default TRUE)
#' @param verbose Logical, whether to print results (default TRUE)
#'
#' @return List containing simulated data and heterogeneity analysis
#' @export
#'
#' @examples
#' \dontrun{
#' # Run heterogeneity analysis
#' het_results <- example_heterogeneous_effects()
#' 
#' # Access the data
#' dat <- het_results$data
#' 
#' # See heterogeneity by b1
#' het_results$het_by_b1
#' }
example_heterogeneous_effects <- function(n = 2000, 
                                         waves = 2, 
                                         seed = 2025,
                                         plot = TRUE,
                                         verbose = TRUE) {
  
  set.seed(seed)
  
  # Custom parameters emphasizing heterogeneity
  het_params <- list(
    # Main effect
    a_lag_y_coef = 0.3,
    
    # Heterogeneous effects - stronger than defaults
    a_b1_y_het = 0.25,  # Strong modification by b1
    a_b2_y_het = 0.15,  # Moderate by b2
    a_b3_y_het = 0.10,  # Weaker by b3
    a_y0_y_het = 0.30,  # Strong modification by baseline outcome
    a_a0_y_het = 0.20   # Modification by baseline exposure
  )
  
  # Generate data with heterogeneous effects
  dat <- margot_simulate(
    n = n,
    waves = waves,
    params = het_params,
    y_feedback = "full",  # Include baseline outcome
    seed = seed
  )
  
  results <- list(data = dat)
  
  # Analyze heterogeneity by continuous baseline covariates
  if (verbose) {
    cat("\n=== Heterogeneous Treatment Effects Analysis ===\n\n")
  }
  
  # 1. By baseline covariate b1
  dat$b1_tertile <- cut(dat$b1, 
                        breaks = quantile(dat$b1, c(0, 1/3, 2/3, 1)),
                        labels = c("Low", "Middle", "High"),
                        include.lowest = TRUE)
  
  # Determine outcome column based on waves
  outcome_col <- paste0("t", waves + 1, "_y")
  treatment_col <- paste0("t", waves, "_a")
  
  het_b1 <- dat %>%
    dplyr::group_by(.data$b1_tertile) %>%
    dplyr::summarise(
      n = dplyr::n(),
      mean_effect = mean(.data[[outcome_col]][.data[[treatment_col]] == 1], na.rm = TRUE) - 
                   mean(.data[[outcome_col]][.data[[treatment_col]] == 0], na.rm = TRUE),
      .groups = "drop"
    )
  
  results$het_by_b1 <- het_b1
  
  if (verbose) {
    cat("Treatment effect by baseline covariate b1 tertiles:\n")
    print(het_b1)
    cat("\n")
  }
  
  # 2. By baseline outcome
  dat$y0_median_split <- ifelse(dat$t0_y > median(dat$t0_y), "High Y0", "Low Y0")
  
  het_y0 <- dat %>%
    dplyr::group_by(.data$y0_median_split) %>%
    dplyr::summarise(
      n = dplyr::n(),
      mean_effect = mean(.data[[outcome_col]][.data[[treatment_col]] == 1], na.rm = TRUE) - 
                   mean(.data[[outcome_col]][.data[[treatment_col]] == 0], na.rm = TRUE),
      .groups = "drop"
    )
  
  results$het_by_y0 <- het_y0
  
  if (verbose) {
    cat("Treatment effect by baseline outcome:\n")
    print(het_y0)
    cat("\n")
  }
  
  # 3. By baseline exposure
  het_a0 <- dat %>%
    dplyr::group_by(.data$t0_a) %>%
    dplyr::summarise(
      n = dplyr::n(),
      mean_effect = mean(.data[[outcome_col]][.data[[treatment_col]] == 1], na.rm = TRUE) - 
                   mean(.data[[outcome_col]][.data[[treatment_col]] == 0], na.rm = TRUE),
      .groups = "drop"
    )
  
  results$het_by_a0 <- het_a0
  
  if (verbose) {
    cat("Treatment effect by baseline exposure:\n")
    print(het_a0)
    cat("\n")
  }
  
  # 4. Regression-based heterogeneity analysis
  # Fit model with interactions
  formula_str <- paste0(outcome_col, " ~ ", treatment_col, " * (b1 + b2 + b3 + t0_y + t0_a)")
  het_model <- lm(as.formula(formula_str), data = dat)
  results$het_model <- het_model
  
  if (verbose) {
    cat("Regression model with interactions:\n")
    cat(sprintf("(Outcome: %s, Treatment: %s)\n\n", outcome_col, treatment_col))
    
    # Extract interaction terms
    coef_summary <- summary(het_model)$coefficients
    interaction_rows <- grep(paste0(treatment_col, ":"), rownames(coef_summary))
    
    if (length(interaction_rows) > 0) {
      cat("Treatment interaction effects:\n")
      print(round(coef_summary[interaction_rows, , drop = FALSE], 4))
    }
  }
  
  # 5. Create plots if requested
  if (plot && requireNamespace("ggplot2", quietly = TRUE)) {
    
    # Plot 1: Effect by b1
    p1 <- ggplot2::ggplot(dat, ggplot2::aes(x = .data$b1, y = .data[[outcome_col]], color = factor(.data[[treatment_col]]))) +
      ggplot2::geom_smooth(method = "loess", se = TRUE) +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Treatment Effect Heterogeneity by Baseline Covariate b1",
        x = "Baseline Covariate b1",
        y = paste0("Outcome (", outcome_col, ")"),
        color = "Treatment"
      ) +
      ggplot2::scale_color_manual(values = c("0" = "blue", "1" = "red"),
                                  labels = c("0" = "Control", "1" = "Treated"))
    
    results$plot_b1 <- p1
    print(p1)
    
    # Plot 2: Effect by baseline outcome
    p2 <- ggplot2::ggplot(dat, ggplot2::aes(x = .data$t0_y, y = .data[[outcome_col]], color = factor(.data[[treatment_col]]))) +
      ggplot2::geom_smooth(method = "loess", se = TRUE) +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Treatment Effect Heterogeneity by Baseline Outcome",
        x = "Baseline Outcome (t0_y)",
        y = paste0("Final Outcome (", outcome_col, ")"),
        color = "Treatment"
      ) +
      ggplot2::scale_color_manual(values = c("0" = "blue", "1" = "red"),
                                  labels = c("0" = "Control", "1" = "Treated"))
    
    results$plot_y0 <- p2
    print(p2)
  }
  
  # 6. Calculate subgroup-specific ATEs
  if (verbose) {
    cat("\n=== Subgroup-Specific Average Treatment Effects ===\n\n")
    
    # Define subgroups by multiple characteristics
    dat$subgroup <- paste0(
      "b1_", dat$b1_tertile,
      "_y0_", dat$y0_median_split
    )
    
    subgroup_ates <- dat %>%
      dplyr::group_by(.data$subgroup) %>%
      dplyr::summarise(
        n = dplyr::n(),
        n_treated = sum(.data[[treatment_col]] == 1),
        n_control = sum(.data[[treatment_col]] == 0),
        ate = mean(.data[[outcome_col]][.data[[treatment_col]] == 1], na.rm = TRUE) - 
              mean(.data[[outcome_col]][.data[[treatment_col]] == 0], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(.data$ate))
    
    results$subgroup_ates <- subgroup_ates
    print(subgroup_ates)
  }
  
  invisible(results)
}

#' Analyze heterogeneous effects from simulation output
#'
#' Helper function to analyze treatment effect heterogeneity from
#' margot_simulate output with custom heterogeneity parameters.
#'
#' @param data Output from margot_simulate
#' @param treatment_wave Wave of treatment to analyze (default 2)
#' @param outcome_wave Wave of outcome to analyze (default 3)
#' @param effect_modifiers Character vector of baseline variables to test
#'
#' @return Data frame with heterogeneity statistics
#' @export
analyze_heterogeneity <- function(data,
                                  treatment_wave = 2,
                                  outcome_wave = 3,
                                  effect_modifiers = c("b1", "b2", "b3", "t0_y", "t0_a")) {
  
  trt_var <- paste0("t", treatment_wave, "_a")
  out_var <- paste0("t", outcome_wave, "_y")
  
  # Check variables exist
  required_vars <- c(trt_var, out_var)
  if (!all(required_vars %in% names(data))) {
    stop("Required variables not found: ", 
         paste(setdiff(required_vars, names(data)), collapse = ", "))
  }
  
  # Check which modifiers exist
  available_modifiers <- intersect(effect_modifiers, names(data))
  
  results <- list()
  
  for (mod in available_modifiers) {
    # For continuous modifiers, calculate correlation with treatment effect
    if (is.numeric(data[[mod]])) {
      # Individual treatment effects (using potential outcomes framework approximation)
      ite_approx <- data[[out_var]] * data[[trt_var]] - 
                   data[[out_var]] * (1 - data[[trt_var]])
      
      cor_test <- cor.test(data[[mod]], ite_approx)
      
      results[[mod]] <- data.frame(
        modifier = mod,
        correlation = cor_test$estimate,
        p_value = cor_test$p.value,
        ci_lower = cor_test$conf.int[1],
        ci_upper = cor_test$conf.int[2]
      )
    }
  }
  
  dplyr::bind_rows(results)
}