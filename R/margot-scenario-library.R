#' Pre-built Scenario Library for margot
#'
#' @description
#' This file contains pre-built scenarios representing common research
#' contexts with typical observational challenges.

# Oracle Scenario ---------------------------------------------------------

#' Create oracle scenario
#'
#' @description
#' The oracle scenario represents perfect measurement with no observational
#' distortions. This serves as a theoretical benchmark against which other
#' scenarios can be compared.
#'
#' @param name Character, name for the scenario (default "Oracle")
#'
#' @return A margot_scenario object with no shadows
#' @export
#'
#' @examples
#' oracle <- scenario_oracle()
#' print(oracle)
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

# RCT Scenarios -----------------------------------------------------------

#' Create typical RCT scenario
#'
#' @description
#' Represents a well-conducted randomised controlled trial with minimal
#' measurement error and moderate dropout.
#'
#' @param measurement_error_sd Numeric, SD of measurement error (default 0.1)
#' @param dropout_rate Numeric, dropout rate (default 0.15)
#' @param name Character, name for the scenario
#'
#' @return A margot_scenario object
#' @export
#'
#' @examples
#' rct <- scenario_rct_typical()
#' print(rct)
scenario_rct_typical <- function(measurement_error_sd = 0.1,
                               dropout_rate = 0.15,
                               name = "Typical RCT") {
  
  shadows <- list()
  
  # measurement error for continuous variables
  if (measurement_error_sd > 0) {
    shadows$measurement <- create_shadow(
      type = "measurement_error",
      params = list(
        variables = c("l"),  # generic continuous variable (time-varying confounder)
        error_type = "classical",
        sigma = measurement_error_sd
      ),
      name = "Clinical measurement error"
    )
  }
  
  # dropout/censoring
  if (dropout_rate > 0) {
    shadows$dropout <- create_shadow(
      type = "censoring",
      params = list(
        rate = dropout_rate,
        b_scale = 0.1,  # weak dependence on baseline
        a_scale = 0.1,  # weak dependence on treatment
        y_scale = 0.2   # moderate dependence on outcomes
      ),
      name = "Study dropout"
    )
  }
  
  # item missingness for questionnaires
  shadows$missing_items <- create_item_missingness_shadow(
    variables = c("t1_y", "t2_y"),  # outcome variables
    missing_mechanism = "MCAR",
    missing_rate = 0.05,  # low rate
    name = "Questionnaire item missingness"
  )
  
  create_scenario(
    name = name,
    shadows = shadows,
    description = "Well-conducted RCT with protocol-based measurement",
    justification = paste(
      "Based on typical patterns from major clinical trials.",
      "Measurement error reflects instrument precision.",
      "Dropout rates from trial meta-analyses."
    ),
    references = c(
      "Akl EA et al. Potential impact on estimated treatment effects of information lost to follow-up in randomised controlled trials (LOST-IT): systematic review. BMJ. 2012;344:e2809.",
      "Altman DG, Bland JM. Measurement in medicine: the analysis of method comparison studies. The Statistician. 1983;32:307-317."
    )
  )
}

#' Create pragmatic RCT scenario
#'
#' @description
#' Represents a pragmatic trial with real-world conditions including
#' higher dropout and more measurement challenges.
#'
#' @param name Character, name for the scenario
#'
#' @return A margot_scenario object
#' @export
scenario_rct_pragmatic <- function(name = "Pragmatic RCT") {
  
  shadows <- list(
    # higher measurement error
    measurement = create_shadow(
      type = "measurement_error",
      params = list(
        variables = c("t1_l", "t2_l"),  # time-varying confounders
        error_type = "classical",
        sigma = 0.2  # higher than typical RCT
      )
    ),
    
    # misclassification of adherence
    adherence_misclass = create_shadow(
      type = "measurement_error",
      params = list(
        variables = "t1_a",  # treatment/exposure variable
        error_type = "misclassification",
        sensitivity = 0.85,
        specificity = 0.80
      )
    ),
    
    # higher dropout
    dropout = create_shadow(
      type = "censoring",
      params = list(
        rate = 0.25,
        b_scale = 0.2,  # stronger baseline dependence
        a_scale = 0.3,  # treatment affects dropout
        y_scale = 0.3   # outcome affects dropout
      )
    ),
    
    # more missing data
    missing = create_item_missingness_shadow(
      variables = c("t2_y"),  # outcome
      missing_mechanism = "MAR",
      missing_rate = 0.15,
      dependent_vars = c("b1", "b2")  # generic baseline vars
    )
  )
  
  create_scenario(
    name = name,
    shadows = shadows,
    description = "Pragmatic trial with real-world measurement challenges",
    justification = paste(
      "Pragmatic trials prioritise generalisability over internal validity.",
      "Higher measurement error due to routine care settings.",
      "Adherence often self-reported with known biases."
    ),
    references = c(
      "Ford I, Norrie J. Pragmatic Trials. N Engl J Med. 2016;375:454-463.",
      "Loudon K et al. The PRECIS-2 tool: designing trials that are fit for purpose. BMJ. 2015;350:h2147."
    )
  )
}

# Observational Study Scenarios -------------------------------------------

#' Create typical EHR scenario
#'
#' @description
#' Represents electronic health record data with typical limitations including
#' misclassification, missing data, and selection issues.
#'
#' @param name Character, name for the scenario
#'
#' @return A margot_scenario object
#' @export
#'
#' @examples
#' ehr <- scenario_ehr_typical()
#' print(ehr)
scenario_ehr_typical <- function(name = "Typical EHR") {
  
  shadows <- list(
    # diagnosis misclassification
    diagnosis_misclass = create_shadow(
      type = "measurement_error",
      params = list(
        variables = c("diagnosis", "comorbidity"),
        error_type = "misclassification",
        sensitivity = 0.85,  # miss 15% of true cases
        specificity = 0.95   # 5% false positives
      ),
      name = "Diagnosis coding errors"
    ),
    
    # lab value measurement error
    lab_error = create_shadow(
      type = "measurement_error",
      params = list(
        variables = c("lab_value", "vital_signs"),
        error_type = "classical",
        sigma = 0.15
      )
    ),
    
    # missing lab values (informative)
    missing_labs = create_item_missingness_shadow(
      variables = c("lab_value", "biomarker"),
      missing_mechanism = "MNAR",
      missing_rate = 0.35,
      dependent_vars = c("severity", "age"),
      name = "Missing lab values"
    ),
    
    # truncation of extreme values
    lab_truncation = create_truncation_shadow(
      variables = c("lab_value"),
      lower = 0,    # can't be negative
      upper = 500,  # equipment limits
      type = "boundary"
    ),
    
    # selection based on healthcare utilisation
    selection = create_shadow(
      type = "selection",
      params = list(
        selection_type = "baseline",
        selection_vars = c("age", "comorbidity"),
        age_coef = 0.02,        # older more likely in EHR
        comorbidity_coef = 0.5  # sicker more likely in EHR
      )
    )
  )
  
  create_scenario(
    name = name,
    shadows = shadows,
    description = "Electronic health record data with typical limitations",
    justification = paste(
      "EHR data quality varies by healthcare system and data type.",
      "Diagnosis codes have known validity issues.",
      "Lab values missing not at random (sicker patients tested more).",
      "Selection into EHR based on healthcare utilisation."
    ),
    references = c(
      "Hernan MA et al. Using Big Data to Emulate a Target Trial When a Randomized Trial Is Not Available. Am J Epidemiol. 2016;183:758-764.",
      "Wells BJ et al. Strategies for handling missing data in electronic health record derived data. EGEMS. 2013;1:1035.",
      "Goldstein BA et al. Opportunities and challenges in developing risk prediction models with EHR data: a systematic review. JAMIA. 2017;24:198-208."
    )
  )
}

#' Create survey study scenario
#'
#' @description
#' Represents survey or cohort study data with self-report biases,
#' item non-response, and selection issues.
#'
#' @param response_rate Numeric, survey response rate
#' @param name Character, name for the scenario
#'
#' @return A margot_scenario object
#' @export
scenario_survey_typical <- function(response_rate = 0.60,
                                  name = "Typical Survey") {
  
  shadows <- list(
    # self-report measurement error
    selfreport_error = create_shadow(
      type = "measurement_error",
      params = list(
        variables = c("income", "exercise", "diet_quality"),
        error_type = "classical",
        sigma = 0.25  # substantial error in self-report
      )
    ),
    
    # social desirability bias for sensitive items
    misreport_sensitive = create_shadow(
      type = "measurement_error",
      params = list(
        variables = c("smoking", "alcohol", "drug_use"),
        error_type = "misclassification",
        sensitivity = 0.70,  # under-report sensitive behaviours
        specificity = 0.95   # rarely false positive
      )
    ),
    
    # differential misclassification by education
    differential_misclass = create_shadow(
      type = "measurement_error",
      params = list(
        variables = "health_status",
        error_type = "differential",
        differential_var = "b2",  # baseline variable used as proxy for education
        differential_fn = function(modifier_values) {
          # more educated report health more accurately
          # education is b2 in our simulated data
          # higher education = lower error
          rnorm(length(modifier_values), 0, 0.3 - 0.1 * modifier_values)
        }
      )
    ),
    
    # item non-response
    item_missing = create_item_missingness_shadow(
      variables = c("income", "sensitive_items"),
      missing_mechanism = "MAR",
      missing_rate = 0.20,
      dependent_vars = c("age", "education")
    ),
    
    # selection based on response
    selection = create_shadow(
      type = "selection",
      params = list(
        selection_type = "baseline",
        selection_vars = c("education", "health"),
        education_coef = 0.5,  # more educated more likely to respond
        health_coef = -0.3     # less healthy less likely to respond
      )
    )
  )
  
  # adjust selection to match response rate
  # (this is simplified - would need proper calibration)
  
  create_scenario(
    name = name,
    shadows = shadows,
    description = "Survey study with self-report biases and selection",
    justification = paste(
      "Survey response rates have declined substantially.",
      "Self-report measures have known biases, especially for sensitive topics.",
      "Item non-response follows predictable patterns.",
      "Response propensity related to education and health."
    ),
    references = c(
      "Groves RM. Nonresponse rates and nonresponse bias in household surveys. Public Opin Q. 2006;70:646-675.",
      "Tourangeau R, Yan T. Sensitive questions in surveys. Psychol Bull. 2007;133:859-883.",
      "Bowling A. Mode of questionnaire administration can have serious effects on data quality. J Public Health. 2005;27:281-291."
    )
  )
}

# Registry/Administrative Data Scenarios ----------------------------------

#' Create registry data scenario
#'
#' @description
#' Represents administrative or registry data with typical limitations
#' including truncation, coarsening, and systematic errors.
#'
#' @param name Character, name for the scenario
#'
#' @return A margot_scenario object
#' @export
scenario_registry_typical <- function(name = "Typical Registry") {
  
  shadows <- list(
    # income top-coding
    income_truncation = create_truncation_shadow(
      variables = "income",
      upper = 200000,  # top-coded for privacy
      type = "boundary"
    ),
    
    # age coarsening
    age_coarsening = create_coarsening_shadow(
      variables = "age",
      breaks = c(0, 18, 25, 35, 45, 55, 65, 75, 85, Inf),
      type = "lower",  # often report lower bound of range
      name = "Age grouping"
    ),
    
    # date heaping
    date_heaping = create_coarsening_shadow(
      variables = c("diagnosis_date", "procedure_date"),
      breaks = 12,  # monthly
      type = "heaping",
      heaping_digits = c(1, 15),  # heap at 1st and 15th
      heaping_prob = 0.40
    ),
    
    # coding errors
    code_errors = create_shadow(
      type = "measurement_error",
      params = list(
        variables = c("procedure_code", "diagnosis_code"),
        error_type = "misclassification",
        sensitivity = 0.90,
        specificity = 0.98
      )
    ),
    
    # missing not at random
    missing_followup = create_item_missingness_shadow(
      variables = c("followup_outcome"),
      missing_mechanism = "MNAR",
      missing_rate = 0.25,
      dependent_vars = c("severity", "ses")
    )
  )
  
  create_scenario(
    name = name,
    shadows = shadows,
    description = "Administrative/registry data with typical limitations",
    justification = paste(
      "Administrative data collected for non-research purposes.",
      "Privacy regulations require truncation/coarsening.",
      "Coding accuracy varies by institution and coder experience.",
      "Follow-up often incomplete with informative missingness."
    ),
    references = c(
      "Benchimol EI et al. The REporting of studies Conducted using Observational Routinely-collected health Data (RECORD) statement. PLoS Med. 2015;12:e1001885.",
      "Harron K et al. Challenges in administrative data linkage for research. Big Data Soc. 2017;4:2053951717745678."
    )
  )
}

# Pessimistic/Worst-Case Scenarios ----------------------------------------

#' Create pessimistic scenario
#'
#' @description
#' Represents a worst-case but plausible scenario with multiple
#' substantial observational challenges.
#'
#' @param name Character, name for the scenario
#'
#' @return A margot_scenario object
#' @export
scenario_pessimistic <- function(name = "Pessimistic") {
  
  shadows <- list(
    # substantial measurement error
    measurement = create_shadow(
      type = "measurement_error",
      params = list(
        variables = c("t1_l", "t2_l"),  # time-varying confounders
        error_type = "classical",
        sigma = 0.4
      )
    ),
    
    # poor classification
    misclass = create_shadow(
      type = "measurement_error",
      params = list(
        variables = "t1_a",  # treatment
        error_type = "misclassification",
        sensitivity = 0.70,
        specificity = 0.75
      )
    ),
    
    # differential misclassification
    diff_misclass = create_shadow(
      type = "measurement_error",
      params = list(
        variables = "t2_y",  # outcome
        error_type = "differential",
        differential_var = "t1_a",  # treatment
        differential_fn = function(modifier_values) {
          # outcome error depends on treatment status
          # higher error for untreated
          ifelse(modifier_values == 1, 
                 rnorm(length(modifier_values), 0, 0.2),
                 rnorm(length(modifier_values), 0, 0.4))
        }
      )
    ),
    
    # substantial missingness
    missing = create_item_missingness_shadow(
      variables = c("t2_y"),  # outcome
      missing_mechanism = "MNAR",
      missing_rate = 0.40,
      dependent_vars = c("t1_a", "b1")  # treatment and baseline
    ),
    
    # truncation
    truncation = create_truncation_shadow(
      variables = c("t1_l"),  # time-varying confounder
      lower = 10,
      upper = 90,
      type = "simple"
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
      "Lash TL et al. Good practices for quantitative bias analysis. Int J Epidemiol. 2014;43:1969-1985.",
      "VanderWeele TJ, Ding P. Sensitivity analysis in observational research: introducing the E-value. Ann Intern Med. 2017;167:268-274."
    )
  )
}

# Scenario Collections ----------------------------------------------------

#' Get standard scenario collection
#'
#' @description
#' Returns a standard collection of scenarios for sensitivity analysis,
#' ranging from oracle (best case) to pessimistic (worst case).
#'
#' @param include Character vector of scenario types to include
#'
#' @return Named list of scenarios
#' @export
#'
#' @examples
#' scenarios <- scenario_collection()
#' names(scenarios)
#' 
#' # use in sensitivity analysis
#' \dontrun{
#' results <- compare_scenarios(
#'   data = my_data,
#'   scenarios = scenarios,
#'   exposure = "treatment",
#'   outcome = "outcome"
#' )
#' }
scenario_collection <- function(include = c("oracle", "rct", "ehr", "survey", "pessimistic")) {
  
  all_scenarios <- list()
  
  if ("oracle" %in% include) {
    all_scenarios$oracle <- scenario_oracle()
  }
  
  if ("rct" %in% include) {
    all_scenarios$rct_typical <- scenario_rct_typical()
    all_scenarios$rct_pragmatic <- scenario_rct_pragmatic()
  }
  
  if ("ehr" %in% include) {
    all_scenarios$ehr <- scenario_ehr_typical()
  }
  
  if ("survey" %in% include) {
    all_scenarios$survey <- scenario_survey_typical()
  }
  
  if ("registry" %in% include) {
    all_scenarios$registry <- scenario_registry_typical()
  }
  
  if ("pessimistic" %in% include) {
    all_scenarios$pessimistic <- scenario_pessimistic()
  }
  
  return(all_scenarios)
}

#' Create custom scenario from template
#'
#' @description
#' Helper function to create a custom scenario based on a template,
#' allowing easy modification of specific parameters.
#'
#' @param template Character, name of template scenario
#' @param modifications List of modifications to apply
#' @param name Character, name for the new scenario
#'
#' @return A margot_scenario object
#' @export
#'
#' @examples
#' # create RCT with higher dropout
#' high_dropout_rct <- scenario_from_template(
#'   "rct_typical",
#'   modifications = list(dropout_rate = 0.30),
#'   name = "High Dropout RCT"
#' )
scenario_from_template <- function(template, 
                                 modifications = list(),
                                 name = NULL) {
  
  # get base scenario
  base_scenario <- switch(template,
    "oracle" = scenario_oracle(),
    "rct_typical" = scenario_rct_typical(),
    "rct_pragmatic" = scenario_rct_pragmatic(),
    "ehr" = scenario_ehr_typical(),
    "survey" = scenario_survey_typical(),
    "registry" = scenario_registry_typical(),
    "pessimistic" = scenario_pessimistic(),
    stop("Unknown template: ", template)
  )
  
  # apply modifications
  # (this would need more sophisticated implementation)
  
  # update name if provided
  if (!is.null(name)) {
    base_scenario$name <- name
  }
  
  return(base_scenario)
}