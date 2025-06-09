#' Test simple censoring
#'
#' @description
#' Example function demonstrating basic censoring functionality.
#' Generates data with high censoring rate to show the effect.
#'
#' @return Invisibly returns the generated data
#' @export
test_censoring_simple <- function() {
  cli::cli_h1("Testing Simple Censoring")
  
  # generate with high censoring
  dat <- margot_simulate(
    n = 500,
    waves = 3,
    censoring = list(rate = 0.3, exposure_dependence = TRUE),
    seed = 2025
  )
  
  # apply censoring
  dat_censored <- apply_censoring_post_hoc(dat)
  
  # check retention
  not_lost_cols <- grep("not_lost_following_wave", names(dat_censored), value = TRUE)
  
  cli::cli_h2("Retention by Wave")
  for (col in not_lost_cols) {
    retained <- mean(dat_censored[[col]] == 1, na.rm = TRUE)
    wave <- gsub("_not_lost_following_wave", "", col)
    cli::cli_alert_info("{wave}: {round(retained * 100, 1)}% retained")
  }
  
  # check final outcome availability
  final_y <- paste0("t", attr(dat, "margot_meta")$waves + 1, "_y")
  n_complete <- sum(!is.na(dat_censored[[final_y]]))
  
  cli::cli_alert_success(
    "Final outcome available for {n_complete}/{nrow(dat)} ({round(100*n_complete/nrow(dat), 1)}%)"
  )
  
  invisible(dat_censored)
}

#' Example: Test censoring indicators
#'
#' @description
#' Demonstrates how censoring indicators work and validates the censoring logic
#' by showing that future data is NA when not_lost indicator is 0.
#'
#' @return List with original and censored data
#' @export
example_test_censoring <- function() {
  cli::cli_h1("Testing Censoring Indicators")
  
  # small dataset for clarity
  dat <- margot_simulate(
    n = 100,
    waves = 2,
    censoring = list(rate = 0.2),
    seed = 123
  )
  
  dat_censored <- apply_censoring_post_hoc(dat)
  
  # examine specific cases
  cli::cli_h2("Example Cases")
  
  # find someone censored after t0
  censored_t0 <- which(dat_censored$t0_not_lost_following_wave == 0)[1]
  if (!is.na(censored_t0)) {
    cli::cli_alert_info("Subject {censored_t0} censored after t0:")
    cli::cli_bullets(c(
      " " = "t0_not_lost = {dat_censored$t0_not_lost_following_wave[censored_t0]}",
      " " = "t1_a is {ifelse(is.na(dat_censored$t1_a[censored_t0]), 'NA', dat_censored$t1_a[censored_t0])}",
      " " = "t2_y is {ifelse(is.na(dat_censored$t2_y[censored_t0]), 'NA', dat_censored$t2_y[censored_t0])}"
    ))
  }
  
  # find someone who survives to end
  survivor <- which(
    dat_censored$t0_not_lost_following_wave == 1 &
    dat_censored$t1_not_lost_following_wave == 1
  )[1]
  
  if (!is.na(survivor)) {
    cli::cli_alert_info("Subject {survivor} observed throughout:")
    cli::cli_bullets(c(
      " " = "t0_not_lost = {dat_censored$t0_not_lost_following_wave[survivor]}",
      " " = "t1_not_lost = {dat_censored$t1_not_lost_following_wave[survivor]}",
      " " = "t2_y = {round(dat_censored$t2_y[survivor], 2)}"
    ))
  }
  
  invisible(list(original = dat, censored = dat_censored))
}

#' Example: Complete workflow with weights and censoring
#'
#' @description
#' Comprehensive example showing:
#' - Sampling weights to create target population
#' - Multiple interventions for causal contrasts
#' - Post-hoc censoring creating observed data
#' - Bias analysis from censoring
#'
#' @return margot_causal_sim object with results
#' @export
example_complete_workflow <- function() {
  cli::cli_h1("Complete margot Workflow Example")
  
  # define sampling weight function
  # upweight those with high baseline risk
  weight_fn <- function(baseline_data) {
    risk_score <- baseline_data$b1 + baseline_data$b2
    weights <- 1 + 0.5 * (risk_score > 0)  # higher weight for high risk
    weights / mean(weights)  # normalize
  }
  
  # define interventions
  interventions <- list(
    natural = function(data, time, trt) {
      data[[trt]]  # observed treatment
    },
    
    never = function(data, time, trt) {
      rep(0, nrow(data))
    },
    
    always = function(data, time, trt) {
      rep(1, nrow(data))
    },
    
    threshold = function(data, time, trt) {
      if (time == 0) {
        return(data[[trt]])  # natural at baseline
      }
      # treat if confounder above median
      l_var <- paste0("t", time, "_l")
      as.numeric(data[[l_var]] > 0)
    }
  )
  
  # run simulation
  results <- margot_simulate_causal(
    n = 1000,
    waves = 3,
    treatments = "a",
    interventions = interventions,
    sampling_weights = weight_fn,
    apply_censoring = TRUE,
    common_params = list(
      params = list(a_lag_y_coef = 0.3),  # true effect
      censoring_params = list(
        rate = 0.15,
        exposure_dependence = TRUE,
        y_dependence = TRUE
      ),
      verbose = TRUE
    ),
    seed = 2025
  )
  
  cli::cli_h2("Results Summary")
  print(results)
  
  invisible(results)
}

#' Run all margot examples
#'
#' @description
#' Runs all example functions to demonstrate package capabilities.
#'
#' @param pause Logical. Pause between examples?
#'
#' @export
run_all_examples <- function(pause = TRUE) {
  examples <- list(
    "Basic simulation" = example_basic_simulation,
    "Censoring mechanisms" = example_censoring_mechanisms,
    "Causal censoring bias" = example_causal_censoring_bias,
    "Sampling weights" = example_sampling_weights,
    "Multiple outcomes" = example_multiple_outcomes,
    "Dynamic interventions" = example_dynamic_interventions,
    "Long format data" = example_long_format,
    "Complete workflow" = example_complete_workflow,
    "Measurement error comparison" = example_measurement_error_comparison,
    "Monte Carlo evaluation" = example_mc_measurement_error
  )
  
  for (name in names(examples)) {
    cli::cli_rule(name)
    
    if (exists(examples[[name]], mode = "function")) {
      examples[[name]]()
    } else {
      cli::cli_alert_warning("Example function {examples[[name]]} not found")
    }
    
    if (pause && name != names(examples)[length(examples)]) {
      readline(prompt = "Press [enter] to continue to next example...")
    }
  }
  
  cli::cli_alert_success("All examples completed!")
}

# Additional example functions that might be referenced elsewhere
# These provide specific demonstrations of features

#' Example: Basic simulation
#' @keywords internal
example_basic_simulation <- function() {
  cli::cli_h2("Basic margot Simulation")
  
  dat <- margot_simulate(
    n = 500,
    waves = 3,
    n_outcomes = 1,
    n_baselines = 5,
    seed = 123
  )
  
  cli::cli_alert_info("Generated {nrow(dat)} subjects over {attr(dat, 'margot_meta')$waves} waves")
  cli::cli_alert_info("Variables: {length(names(dat))} total")
  
  margot_report_sim(dat)
  
  invisible(dat)
}

#' Example: Different censoring mechanisms
#' @keywords internal
example_censoring_mechanisms <- function() {
  cli::cli_h2("Censoring Mechanism Comparison")
  
  # base parameters
  base_params <- list(n = 500, waves = 3, seed = 456)
  
  # different censoring setups
  censoring_types <- list(
    "No censoring" = list(rate = 0),
    "Random censoring" = list(rate = 0.1),
    "Exposure-dependent" = list(rate = 0.1, exposure_dependence = TRUE),
    "Outcome-dependent" = list(rate = 0.1, y_dependence = TRUE),
    "Informative (all)" = list(
      rate = 0.1,
      exposure_dependence = TRUE,
      l_dependence = TRUE,
      y_dependence = TRUE
    )
  )
  
  for (name in names(censoring_types)) {
    cli::cli_h3(name)
    
    dat <- do.call(margot_simulate, c(
      base_params,
      list(censoring = censoring_types[[name]])
    ))
    
    dat_cens <- apply_censoring_post_hoc(dat)
    
    # final retention
    final_y <- paste0("t", base_params$waves + 1, "_y")
    retention <- mean(!is.na(dat_cens[[final_y]]))
    
    cli::cli_alert_info("Final retention: {round(retention * 100, 1)}%")
  }
}

#' Example: Causal effects under censoring
#' @keywords internal  
example_causal_censoring_bias <- function() {
  cli::cli_h2("Censoring Bias in Causal Effects")
  
  # simple always vs never comparison
  results <- margot_simulate_causal(
    n = 1000,
    waves = 2,
    treatments = "a",
    interventions = list(
      never = function(data, time, trt) rep(0, nrow(data)),
      always = function(data, time, trt) rep(1, nrow(data))
    ),
    apply_censoring = TRUE,
    common_params = list(
      params = list(a_lag_y_coef = 0.4),  # true effect = 0.4
      censoring_params = list(
        rate = 0.2,
        exposure_dependence = TRUE  # censoring depends on treatment
      )
    ),
    seed = 789
  )
  
  if (!is.null(results$effects)) {
    true_effect <- results$effects$estimate[1]
    
    if (!is.null(results$censoring_bias)) {
      observed_effect <- results$censoring_bias$estimate_observed[1]
      bias <- results$censoring_bias$bias[1]
      
      cli::cli_alert_info("True effect: {round(true_effect, 3)}")
      cli::cli_alert_info("Observed effect: {round(observed_effect, 3)}")
      cli::cli_alert_warning("Bias from censoring: {round(bias, 3)} ({round(bias/true_effect * 100, 1)}%)")
    }
  }
  
  invisible(results)
}

#' Example: Sampling weights
#' @keywords internal
example_sampling_weights <- function() {
  cli::cli_h2("Target Population via Sampling Weights")
  
  # weight function: oversample high-risk individuals
  weight_fn <- function(baseline_data) {
    risk <- baseline_data$b1 + baseline_data$b2
    ifelse(risk > median(risk), 2, 0.5)  # 4:1 ratio
  }
  
  # with and without weights
  dat_unweighted <- margot_simulate(n = 500, waves = 2, seed = 111)
  dat_weighted <- margot_simulate(
    n = 500, 
    waves = 2, 
    sampling_weights = weight_fn,
    seed = 111
  )
  
  cli::cli_h3("Baseline distributions")
  cli::cli_text("Unweighted mean(b1): {round(mean(dat_unweighted$b1), 3)}")
  cli::cli_text("Weighted mean(b1): {round(mean(dat_weighted$b1), 3)}")
  
  # weights create different population
  cli::cli_alert_info(
    "Sampling weights shifted baseline by {round(mean(dat_weighted$b1) - mean(dat_unweighted$b1), 3)}"
  )
}

#' Example: Multiple correlated outcomes
#' @keywords internal
example_multiple_outcomes <- function() {
  cli::cli_h2("Multiple Correlated Outcomes")
  
  dat <- margot_simulate(
    n = 500,
    waves = 2,
    n_outcomes = 3,
    params = list(
      y_cor = 0.5,  # correlation between outcomes
      y2_shrink = 0.8,  # attenuate effects for y2
      y3_shrink = 0.6   # attenuate effects for y3
    ),
    seed = 222
  )
  
  # check correlations at final time
  cor_mat <- cor(dat[, c("t3_y1", "t3_y2", "t3_y3")], use = "complete.obs")
  
  cli::cli_h3("Outcome correlations at final wave")
  cli::cli_text("Y1-Y2: {round(cor_mat[1,2], 3)}")
  cli::cli_text("Y1-Y3: {round(cor_mat[1,3], 3)}")
  cli::cli_text("Y2-Y3: {round(cor_mat[2,3], 3)}")
  
  # treatment effects differ by outcome
  effects <- sapply(1:3, function(j) {
    y_var <- paste0("t3_y", j)
    fit <- lm(as.formula(paste(y_var, "~ t2_a + t1_a + b1")), data = dat)
    coef(fit)["t2_a"]
  })
  
  cli::cli_h3("Treatment effects by outcome")
  cli::cli_bullets(setNames(
    paste0("Y", 1:3, ": ", round(effects, 3)),
    rep("*", 3)
  ))
}

#' Example: Dynamic treatment strategies
#' @keywords internal
example_dynamic_interventions <- function() {
  cli::cli_h2("Dynamic Treatment Strategies")
  
  interventions <- list(
    # static strategies
    never = function(data, time, trt) rep(0, nrow(data)),
    always = function(data, time, trt) rep(1, nrow(data)),
    
    # dynamic: respond to time-varying confounder
    responsive = function(data, time, trt) {
      if (time == 0) return(rep(0, nrow(data)))
      l_var <- paste0("t", time, "_l")
      as.numeric(data[[l_var]] > quantile(data[[l_var]], 0.7, na.rm = TRUE))
    },
    
    # threshold based on cumulative exposure
    limited = function(data, time, trt) {
      if (time == 0) return(rep(0, nrow(data)))
      # count previous treatments
      prev_treats <- 0
      for (s in 0:(time-1)) {
        a_var <- paste0("t", s, "_a")
        if (a_var %in% names(data)) {
          prev_treats <- prev_treats + data[[a_var]]
        }
      }
      # treat only if < 2 previous treatments
      as.numeric(prev_treats < 2 & data[[paste0("t", time, "_l")]] > 0)
    }
  )
  
  results <- margot_simulate_causal(
    n = 500,
    waves = 4,
    treatments = "a",
    interventions = interventions,
    common_params = list(
      params = list(a_lag_y_coef = 0.3),
      verbose = FALSE
    ),
    seed = 333
  )
  
  # summarise strategies
  for (name in names(interventions)) {
    if (name %in% names(results$data)) {
      dat <- results$data[[name]]
      total_treat <- rowSums(dat[, grep("^t[0-9]+_a$", names(dat))])
      avg_treat <- mean(total_treat, na.rm = TRUE)
      
      cli::cli_alert_info("{name}: average {round(avg_treat, 2)} treatments per person")
    }
  }
  
  if (!is.null(results$effects)) {
    cli::cli_h3("Causal effects")
    print(results$effects[, c("contrast", "estimate", "se")])
  }
}

#' Example: Long format data
#' @keywords internal
example_long_format <- function() {
  cli::cli_h2("Long Format Data")
  
  # generate in long format
  dat_long <- margot_simulate(
    n = 200,
    waves = 3,
    wide = FALSE,
    seed = 444
  )
  
  cli::cli_alert_info("Long format: {nrow(dat_long)} rows ({length(unique(dat_long$id))} subjects × {length(unique(dat_long$time))} times)")
  
  cli::cli_h3("Data structure")
  print(head(dat_long[, c("id", "time", "a", "l", "y")], 10))
  
  # can analyse with mixed models
  if (requireNamespace("lme4", quietly = TRUE)) {
    cli::cli_h3("Mixed model example")
    # would fit model here
    cli::cli_text("Mixed model would account for within-person correlation")
  }
}