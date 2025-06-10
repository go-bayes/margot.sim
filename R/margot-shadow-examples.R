#' Example: Shadow Bias Analysis Workflow
#'
#' This example demonstrates the complete workflow for analyzing how
#' observational shadows affect causal effect estimation.
#'
#' @param n Sample size (default 1000)
#' @param seed Random seed for reproducibility
#' @param verbose Print progress messages
#'
#' @return A list containing the simulation results and bias analysis
#'
#' @examples
#' \dontrun{
#' # Run the example
#' results <- example_shadow_bias_analysis()
#' 
#' # View the bias comparison
#' print(results$comparison)
#' }
#'
#' @export
example_shadow_bias_analysis <- function(n = 1000, seed = 2024, verbose = TRUE) {
  set.seed(seed)
  
  if (verbose) {
    cat("Shadow Bias Analysis Example\n")
    cat("============================\n\n")
  }
  
  # step 1: simulate data with known causal effect
  if (verbose) cat("Step 1: Simulating data with true ATE = 0.5...\n")
  
  data <- margot_simulate(
    n = n,
    waves = 3,
    params = list(
      a_y_coef = 0.5,         # true causal effect
      l_y_coef = 0.3,         # confounder effect
      a_l_coef = 0.2          # treatment affects confounder
    )
  )
  
  # step 2: define multiple shadows
  if (verbose) cat("\nStep 2: Defining shadows...\n")
  
  shadows <- list(
    # measurement error in treatment
    create_shadow(
      type = "measurement_error",
      params = list(
        variables = c("t1_a", "t2_a"),
        error_type = "classical",
        sigma = 0.3
      ),
      name = "treatment_measurement_error"
    ),
    
    # measurement error in outcome
    create_shadow(
      type = "measurement_error",
      params = list(
        variables = "t3_y",
        error_type = "classical",
        sigma = 0.4
      ),
      name = "outcome_measurement_error"
    ),
    
    # item missingness in confounder
    create_shadow(
      type = "item_missingness",
      params = list(
        variables = c("t1_l", "t2_l"),
        mechanism = "MAR",
        rate = 0.15,
        dependent_vars = "t1_a"  # correct parameter name
      ),
      name = "confounder_missingness"
    )
  )
  
  # step 3: apply shadows with truth preservation
  if (verbose) cat("\nStep 3: Applying shadows...\n")
  
  shadow_result <- apply_shadows_with_truth(
    data = data,
    shadows = shadows,
    preserve_complete = TRUE,
    verbose = verbose
  )
  
  # step 4: compare effects
  if (verbose) cat("\nStep 4: Computing and comparing causal effects...\n")
  
  comparison <- compare_shadow_effects(
    shadow_result,
    wave = 1,
    outcome_wave = 3,
    treatment_name = "a",
    outcome_name = "y",
    estimands = c("ate", "att", "atu")
  )
  
  # step 5: create summary
  if (verbose) {
    cat("\n")
    print(comparison)
  }
  
  # return everything for further analysis
  list(
    data = shadow_result,
    shadows = shadows,
    comparison = comparison,
    metadata = list(
      n = n,
      seed = seed,
      true_ate = 0.5
    )
  )
}

#' Example: Multiple Shadow Scenarios
#'
#' Demonstrates how different shadow combinations affect bias.
#'
#' @param n Sample size
#' @param scenarios List of shadow scenarios to compare
#' @param verbose Print progress
#'
#' @return Data frame comparing bias across scenarios
#'
#' @export
example_shadow_scenarios <- function(n = 500, 
                                     scenarios = NULL,
                                     verbose = TRUE) {
  
  if (is.null(scenarios)) {
    # default scenarios
    scenarios <- list(
      none = list(),
      
      measurement_only = list(
        create_shadow(
          type = "measurement_error",
          params = list(
            variables = c("t1_a", "t2_a", "t3_y"),
            error_type = "classical",
            sigma = 0.3
          )
        )
      ),
      
      missingness_only = list(
        create_shadow(
          type = "item_missingness",
          params = list(
            variables = c("t1_l", "t2_l", "t3_y"),
            mechanism = "MCAR",
            rate = 0.2
          )
        )
      ),
      
      combined = list(
        create_shadow(
          type = "measurement_error",
          params = list(
            variables = c("t1_a", "t2_a"),
            error_type = "classical",
            sigma = 0.3
          )
        ),
        create_shadow(
          type = "item_missingness",
          params = list(
            variables = c("t3_y"),
            mechanism = "MAR",
            rate = 0.15,
            dependent_vars = "t1_a"
          )
        )
      )
    )
  }
  
  # simulate base data
  set.seed(42)
  data <- margot_simulate(
    n = n,
    waves = 3,
    params = list(a_y_coef = 0.6)  # true effect
  )
  
  # run each scenario
  results <- list()
  
  for (scenario_name in names(scenarios)) {
    if (verbose) {
      cat(sprintf("\nScenario: %s\n", scenario_name))
    }
    
    # apply shadows
    if (length(scenarios[[scenario_name]]) > 0) {
      shadow_data <- apply_shadows_with_truth(
        data = data,
        shadows = scenarios[[scenario_name]],
        preserve_complete = TRUE,
        verbose = FALSE
      )
    } else {
      # no shadows - create compatible structure
      shadow_data <- list(
        data_true = data,
        data_observed = data,
        shadows_applied = NULL
      )
      class(shadow_data) <- c("margot_shadow_result", "list")
    }
    
    # compare effects
    comparison <- compare_shadow_effects(
      shadow_data,
      wave = 1,
      outcome_wave = 3,
      treatment_name = "a",
      outcome_name = "y"
    )
    
    # store results
    results[[scenario_name]] <- comparison$comparison
    results[[scenario_name]]$scenario <- scenario_name
  }
  
  # combine results
  combined <- do.call(rbind, results)
  row.names(combined) <- NULL
  
  if (verbose) {
    cat("\n\nSummary Across Scenarios:\n")
    print(combined)
  }
  
  return(combined)
}

#' Example: Shadow Bias Analysis with Sampling Weights
#'
#' Demonstrates how sampling weights interact with shadow bias analysis,
#' showing effects in both the source population and weighted target population.
#'
#' @param n Sample size
#' @param seed Random seed
#' @param verbose Print progress
#'
#' @return List with weighted and unweighted comparisons
#'
#' @examples
#' \dontrun{
#' # Run weighted analysis
#' results <- example_weighted_shadow_analysis()
#' 
#' # Compare weighted vs unweighted bias
#' print(results$comparison_weighted)
#' print(results$comparison_unweighted)
#' }
#'
#' @export
example_weighted_shadow_analysis <- function(n = 800, seed = 2025, verbose = TRUE) {
  set.seed(seed)
  
  if (verbose) {
    cat("Weighted Shadow Bias Analysis\n")
    cat("=============================\n\n")
  }
  
  # create weight function to target older population
  # this mimics reweighting a general population sample to match
  # an older clinical trial population
  weight_fn <- function(baseline_data) {
    # b1 represents age, b2 represents health status
    age_score <- baseline_data$b1
    health_score <- baseline_data$b2
    
    # upweight older (b1 > 0) and sicker (b2 > 0) individuals
    weights <- exp(0.5 * age_score + 0.3 * health_score)
    weights / mean(weights)  # normalize to mean 1
  }
  
  # simulate data where treatment effect varies by age
  if (verbose) cat("Simulating data with heterogeneous treatment effects...\n")
  
  data <- margot_simulate(
    n = n,
    waves = 3,
    params = list(
      # base treatment effect
      a_y_coef = 0.4,
      # age modifies treatment effect (older benefit more)
      a_b1_y_het = 0.3,
      # baseline effects
      b1_y_coef = 0.2,
      b2_y_coef = 0.15
    ),
    sampling_weights = weight_fn
  )
  
  if (verbose) {
    cat(sprintf("  Effective sample size (1/sum(w^2)): %.1f\n", 
                1/sum(data$sampling_weight^2)))
    cat(sprintf("  Weight range: [%.2f, %.2f]\n", 
                min(data$sampling_weight), 
                max(data$sampling_weight)))
  }
  
  # create realistic shadows
  shadows <- list(
    create_shadow(
      type = "measurement_error",
      params = list(
        variables = c("t1_a", "t2_a"),
        error_type = "classical",
        sigma = 0.25
      ),
      name = "treatment_error"
    ),
    create_shadow(
      type = "item_missingness",
      params = list(
        variables = "t3_y",
        mechanism = "MAR",
        rate = 0.1,
        dependent_vars = c("b1", "b2")  # missingness depends on age/health
      ),
      name = "outcome_missingness"
    )
  )
  
  # apply shadows
  if (verbose) cat("\nApplying observational shadows...\n")
  
  shadow_result <- apply_shadows_with_truth(
    data = data,
    shadows = shadows,
    preserve_complete = TRUE,
    verbose = FALSE
  )
  
  # compare effects WITH weights (target population)
  if (verbose) cat("\nComputing effects in TARGET population (weighted)...\n")
  
  comparison_weighted <- compare_shadow_effects(
    shadow_result,
    wave = 1,
    outcome_wave = 3,
    treatment_name = "a",
    outcome_name = "y",
    weights = data$sampling_weight
  )
  
  # compare effects WITHOUT weights (source population)
  if (verbose) cat("Computing effects in SOURCE population (unweighted)...\n")
  
  comparison_unweighted <- compare_shadow_effects(
    shadow_result,
    wave = 1,
    outcome_wave = 3,
    treatment_name = "a",
    outcome_name = "y"
    # no weights argument
  )
  
  if (verbose) {
    cat("\n--- Target Population (Weighted) ---\n")
    print(comparison_weighted$comparison)
    
    cat("\n--- Source Population (Unweighted) ---\n")
    print(comparison_unweighted$comparison)
    
    # show difference
    weighted_bias <- comparison_weighted$comparison$bias[
      comparison_weighted$comparison$estimand == "ate"
    ]
    unweighted_bias <- comparison_unweighted$comparison$bias[
      comparison_unweighted$comparison$estimand == "ate"
    ]
    
    cat(sprintf("\nBias difference (weighted - unweighted): %.3f\n", 
                weighted_bias - unweighted_bias))
  }
  
  # return both for comparison
  list(
    data = shadow_result,
    comparison_weighted = comparison_weighted,
    comparison_unweighted = comparison_unweighted,
    weights_summary = summary(data$sampling_weight)
  )
}