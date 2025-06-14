#' Simple Pre-built Scenario Library for margot
#'
#' @description
#' This file contains simple pre-built scenarios that work with 
#' standard margot_simulate() output without requiring specific variable names.

# Oracle Scenario (unchanged) -------------------------------------------------

#' @rdname scenario_oracle
#' @export
scenario_oracle <- function(name = "Oracle") {
  create_scenario(
    name = name,
    shadows = list(),
    description = "Perfect measurement with no observational distortions",
    justification = "Theoretical benchmark assuming perfect information",
    references = c(
      "Pearl J. Causality: Models, Reasoning, and Inference. 2009."
    )
  )
}

# Simple RCT Scenario ---------------------------------------------------------

#' Create simple RCT scenario
#'
#' @description
#' A simplified RCT scenario that works with any margot_simulate() data
#'
#' @param measurement_error_sd Numeric, SD of measurement error (default 0.1)
#' @param dropout_rate Numeric, dropout rate (default 0.15)
#' @param name Character, name for the scenario
#'
#' @return A margot_scenario object
#' @export
scenario_rct_simple <- function(measurement_error_sd = 0.1,
                               dropout_rate = 0.15,
                               name = "Simple RCT") {
  
  shadows <- list()
  
  # measurement error for continuous variables
  if (measurement_error_sd > 0) {
    shadows$measurement <- create_shadow(
      type = "measurement_error",
      params = list(
        variables = c("t1_l", "t2_l"),  # time-varying confounders
        error_type = "classical",
        sigma = measurement_error_sd
      ),
      name = "Measurement error"
    )
  }
  
  # dropout/censoring
  if (dropout_rate > 0) {
    shadows$dropout <- create_shadow(
      type = "censoring",
      params = list(
        rate = dropout_rate,
        b_scale = 0.1,
        a_scale = 0.1,
        y_scale = 0.2
      ),
      name = "Study dropout"
    )
  }
  
  create_scenario(
    name = name,
    shadows = shadows,
    description = "Simple RCT with measurement error and dropout",
    justification = "Based on typical patterns from clinical trials",
    references = c(
      "Altman DG, Bland JM. Measurement in medicine. The Statistician. 1983;32:307-317."
    )
  )
}

# Simple Observational Scenario -----------------------------------------------

#' Create simple observational study scenario
#'
#' @description
#' A simplified observational scenario with common biases
#'
#' @param name Character, name for the scenario
#'
#' @return A margot_scenario object
#' @export
scenario_observational_simple <- function(name = "Simple Observational") {
  
  shadows <- list(
    # treatment misclassification
    treatment_misclass = create_shadow(
      type = "measurement_error",
      params = list(
        variables = "t1_a",
        error_type = "misclassification",
        sensitivity = 0.85,
        specificity = 0.90
      ),
      name = "Treatment misclassification"
    ),
    
    # outcome measurement error
    outcome_error = create_shadow(
      type = "measurement_error",
      params = list(
        variables = "t2_y",
        error_type = "classical",
        sigma = 0.2
      ),
      name = "Outcome measurement error"
    ),
    
    # missing data
    missing = create_item_missingness_shadow(
      variables = c("t2_y"),
      missing_mechanism = "MAR",
      missing_rate = 0.20,
      dependent_vars = c("b1", "b2"),
      name = "Missing outcomes"
    )
  )
  
  create_scenario(
    name = name,
    shadows = shadows,
    description = "Observational study with measurement error and missingness",
    justification = "Common issues in observational research",
    references = c(
      "Hernan MA, Robins JM. Causal Inference: What If. 2020."
    )
  )
}

# Pessimistic Scenario --------------------------------------------------------

#' @rdname scenario_pessimistic
#' @export
scenario_pessimistic <- function(name = "Pessimistic") {
  
  shadows <- list(
    # substantial measurement error
    measurement = create_shadow(
      type = "measurement_error",
      params = list(
        variables = c("t1_l", "t2_l"),
        error_type = "classical",
        sigma = 0.4
      )
    ),
    
    # poor classification
    misclass = create_shadow(
      type = "measurement_error",
      params = list(
        variables = "t1_a",
        error_type = "misclassification",
        sensitivity = 0.70,
        specificity = 0.75
      )
    ),
    
    # substantial missingness
    missing = create_item_missingness_shadow(
      variables = c("t2_y"),
      missing_mechanism = "MNAR",
      missing_rate = 0.40,
      dependent_vars = c("t1_a", "b1")
    )
  )
  
  create_scenario(
    name = name,
    shadows = shadows,
    description = "Worst-case plausible scenario with multiple biases",
    justification = paste(
      "Represents challenging but realistic data conditions.",
      "Multiple biases can co-occur in observational studies.",
      "Useful for stress-testing causal conclusions."
    ),
    references = c(
      "Lash TL et al. Good practices for quantitative bias analysis. Int J Epidemiol. 2014;43:1969-1985."
    )
  )
}

# Simple Scenario Collection --------------------------------------------------

#' Get simple scenario collection
#'
#' @description
#' Returns a simple collection of scenarios that work with standard
#' margot_simulate() output
#'
#' @return Named list of scenarios
#' @export
scenario_collection_simple <- function() {
  list(
    oracle = scenario_oracle(),
    rct = scenario_rct_simple(),
    observational = scenario_observational_simple(),
    pessimistic = scenario_pessimistic()
  )
}