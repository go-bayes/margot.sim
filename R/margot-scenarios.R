#' Scenario Framework for margot
#'
#' @description
#' This file implements the scenario abstraction layer that bundles shadows
#' with justifications for systematic sensitivity analyses.

# Scenario Creation -------------------------------------------------------

#' Create a scenario specification
#'
#' @description
#' Creates a scenario that bundles observational shadows with documentation
#' about assumptions and justifications. Scenarios represent different sets
#' of assumptions about how data are generated, measured, and selected.
#'
#' @param name Character string naming the scenario
#' @param shadows List of shadow objects to apply
#' @param population Optional population specification
#' @param description Character string describing the scenario
#' @param justification Character string explaining why these assumptions are plausible
#' @param references Optional character vector of supporting references
#'
#' @return A scenario object of class "margot_scenario"
#' @export
#'
#' @examples
#' # create a scenario for typical RCT conditions
#' rct_scenario <- create_scenario(
#'   name = "Typical RCT",
#'   shadows = list(
#'     measurement = create_shadow(
#'       type = "measurement_error",
#'       params = list(
#'         variables = c("bp", "cholesterol"),
#'         error_type = "classical",
#'         sigma = 0.1
#'       )
#'     ),
#'     dropout = create_shadow(
#'       type = "censoring",
#'       params = list(
#'         rate = 0.15,
#'         mechanism = "MAR"
#'       )
#'     )
#'   ),
#'   description = "Typical randomised controlled trial with good measurement",
#'   justification = "Based on protocols from major cardiovascular trials",
#'   references = c("NEJM 2019;380:1509-1524", "Lancet 2020;395:795-808")
#' )
#'
#' # create a scenario for electronic health records
#' ehr_scenario <- create_scenario(
#'   name = "EHR Data",
#'   shadows = list(
#'     misclass = create_shadow(
#'       type = "measurement_error",
#'       params = list(
#'         variables = "diagnosis",
#'         error_type = "misclassification",
#'         sensitivity = 0.85,
#'         specificity = 0.95
#'       )
#'     ),
#'     missingness = create_item_missingness_shadow(
#'       variables = c("lab_values", "vitals"),
#'       mechanism = "MAR",
#'       rate = 0.30,
#'       dependent_vars = c("age", "severity")
#'     )
#'   ),
#'   description = "Electronic health record data with typical limitations",
#'   justification = "Validation studies show 85% sensitivity for diagnoses"
#' )
create_scenario <- function(name,
                          shadows = list(),
                          population = NULL,
                          description = "",
                          justification = "",
                          references = NULL) {
  
  # validate inputs
  if (!is.character(name) || length(name) != 1) {
    stop("Name must be a single character string")
  }
  
  if (!is.list(shadows)) {
    stop("Shadows must be a list")
  }
  
  # check that all shadows are valid shadow objects
  shadow_classes <- sapply(shadows, function(x) inherits(x, "margot_shadow"))
  if (length(shadows) > 0 && !all(shadow_classes)) {
    stop("All elements in shadows list must be margot_shadow objects")
  }
  
  # create scenario object
  scenario <- structure(
    list(
      name = name,
      shadows = shadows,
      population = population,
      description = description,
      justification = justification,
      references = references,
      created = Sys.time(),
      n_shadows = length(shadows)
    ),
    class = "margot_scenario"
  )
  
  return(scenario)
}

#' Print method for scenario objects
#'
#' @param x A margot_scenario object
#' @param ... Additional arguments (ignored)
#'
#' @export
print.margot_scenario <- function(x, ...) {
  cat("Margot Scenario: ", x$name, "\n", sep = "")
  cat(rep("=", nchar(x$name) + 17), "\n", sep = "")
  
  if (nchar(x$description) > 0) {
    cat("Description: ", x$description, "\n", sep = "")
  }
  
  if (nchar(x$justification) > 0) {
    cat("\nJustification: ", x$justification, "\n", sep = "")
  }
  
  if (!is.null(x$references) && length(x$references) > 0) {
    cat("\nReferences:\n")
    for (i in seq_along(x$references)) {
      cat("  [", i, "] ", x$references[i], "\n", sep = "")
    }
  }
  
  cat("\nShadows (", x$n_shadows, "):\n", sep = "")
  if (x$n_shadows > 0) {
    shadow_names <- names(x$shadows)
    if (is.null(shadow_names)) {
      shadow_names <- paste0("Shadow ", seq_along(x$shadows))
    }
    
    for (i in seq_along(x$shadows)) {
      shadow <- x$shadows[[i]]
      cat("  - ", shadow_names[i], ": ", shadow$type, " shadow", sep = "")
      
      # add key details based on shadow type
      if (shadow$type == "measurement_error") {
        cat(" (", shadow$params$error_type, ")", sep = "")
      } else if (shadow$type == "censoring") {
        cat(" (rate: ", shadow$params$rate, ")", sep = "")
      } else if (shadow$type == "item_missingness") {
        cat(" (", shadow$params$mechanism, ", rate: ", shadow$params$rate, ")", sep = "")
      }
      cat("\n")
    }
  } else {
    cat("  (No shadows - represents perfect measurement)\n")
  }
  
  if (!is.null(x$population)) {
    cat("\nPopulation: ", x$population$name, "\n", sep = "")
  }
  
  invisible(x)
}

# Scenario Application ----------------------------------------------------

#' Apply a scenario to data
#'
#' @description
#' Applies all shadows in a scenario to a dataset, maintaining the
#' dual data architecture (true and observed).
#'
#' @param data Data frame to apply scenario to
#' @param scenario A margot_scenario object
#' @param verbose Logical, whether to print progress messages
#'
#' @return A list with components:
#'   \item{data_true}{Original data before shadows}
#'   \item{data_observed}{Data after applying all shadows}
#'   \item{scenario}{Name of the applied scenario}
#'   \item{shadows_applied}{List of applied shadow names}
#'
#' @export
#'
#' @examples
#' # simulate some data
#' sim_data <- margot_simulate(n = 1000, waves = 2)
#' 
#' # create and apply a scenario
#' my_scenario <- create_scenario(
#'   name = "Realistic conditions",
#'   shadows = list(
#'     measurement = create_shadow(
#'       type = "measurement_error",
#'       params = list(
#'         variables = "t1_l",
#'         error_type = "classical",
#'         sigma = 0.5
#'       )
#'     )
#'   )
#' )
#' 
#' result <- apply_scenario(sim_data, my_scenario)
#' 
#' # compare true vs observed data
#' compare_shadow_effects(result)
apply_scenario <- function(data, scenario, verbose = FALSE) {
  
  if (!inherits(scenario, "margot_scenario")) {
    stop("scenario must be a margot_scenario object")
  }
  
  if (verbose) {
    cat("Applying scenario: ", scenario$name, "\n", sep = "")
  }
  
  # use the existing shadow application with truth preservation
  result <- apply_shadows_with_truth(
    data = data,
    shadows = scenario$shadows,
    preserve_complete = TRUE,
    verbose = verbose
  )
  
  # add scenario information
  result$scenario <- scenario$name
  result$scenario_object <- scenario
  
  # update class
  class(result) <- c("margot_scenario_result", class(result))
  
  return(result)
}

#' Print method for scenario results
#'
#' @param x A margot_scenario_result object
#' @param ... Additional arguments (ignored)
#'
#' @export
print.margot_scenario_result <- function(x, ...) {
  cat("Margot Scenario Application Result\n")
  cat("==================================\n")
  cat("Scenario: ", x$scenario, "\n", sep = "")
  
  # call parent print method
  NextMethod()
}

# Scenario Comparison -----------------------------------------------------

#' Compare causal effects across scenarios
#'
#' @description
#' Compares the same causal effect estimate across different scenarios to
#' assess sensitivity to assumptions.
#'
#' @param data Data frame with the true data
#' @param scenarios List of scenario objects to compare
#' @param exposure Character, name of exposure variable
#' @param outcome Character, name of outcome variable
#' @param estimands Character vector of estimands to compute
#' @param verbose Logical, whether to print progress
#'
#' @return A scenario comparison object containing effects under each scenario
#' @export
#'
#' @examples
#' # create data
#' sim_data <- margot_simulate(n = 1000, waves = 2)
#' 
#' # define scenarios
#' scenarios <- list(
#'   oracle = create_scenario("Oracle", shadows = list()),
#'   realistic = create_scenario(
#'     "Realistic",
#'     shadows = list(
#'       create_shadow(
#'         type = "measurement_error",
#'         params = list(
#'           variables = "t1_a",
#'           error_type = "misclassification",
#'           sensitivity = 0.85,
#'           specificity = 0.90
#'         )
#'       )
#'     )
#'   )
#' )
#' 
#' # compare scenarios
#' comparison <- compare_scenarios(
#'   sim_data,
#'   scenarios,
#'   exposure = "t1_a",
#'   outcome = "t2_y"
#' )
#' 
#' print(comparison)
compare_scenarios <- function(data,
                            scenarios,
                            exposure,
                            outcome,
                            estimands = c("ate", "att", "atu"),
                            verbose = FALSE) {
  
  if (!is.list(scenarios)) {
    stop("scenarios must be a list of scenario objects")
  }
  
  # check all are scenarios
  scenario_checks <- sapply(scenarios, function(x) inherits(x, "margot_scenario"))
  if (!all(scenario_checks)) {
    stop("All elements in scenarios list must be margot_scenario objects")
  }
  
  # extract scenario names
  scenario_names <- names(scenarios)
  if (is.null(scenario_names)) {
    scenario_names <- sapply(scenarios, function(x) x$name)
  }
  
  # compute effects under each scenario
  results <- list()
  
  for (i in seq_along(scenarios)) {
    scenario <- scenarios[[i]]
    name <- scenario_names[i]
    
    if (verbose) {
      cat("\nProcessing scenario: ", name, "\n", sep = "")
    }
    
    # apply scenario
    scenario_result <- apply_scenario(data, scenario, verbose = FALSE)
    
    # compute effects
    effects <- compute_causal_effects(
      data = scenario_result$data_observed,
      exposure = exposure,
      outcome = outcome,
      estimands = estimands
    )
    
    # store results
    results[[name]] <- list(
      scenario = scenario,
      effects = effects,
      n_shadows = scenario$n_shadows
    )
  }
  
  # create comparison object
  comparison <- structure(
    list(
      scenarios = scenarios,
      scenario_names = scenario_names,
      results = results,
      exposure = exposure,
      outcome = outcome,
      estimands = estimands,
      n_scenarios = length(scenarios)
    ),
    class = "margot_scenario_comparison"
  )
  
  return(comparison)
}

#' Print method for scenario comparisons
#'
#' @param x A margot_scenario_comparison object
#' @param digits Number of digits for rounding
#' @param ... Additional arguments (ignored)
#'
#' @export
print.margot_scenario_comparison <- function(x, digits = 3, ...) {
  cat("Scenario Comparison Results\n")
  cat("===========================\n")
  cat("Exposure: ", x$exposure, "\n", sep = "")
  cat("Outcome: ", x$outcome, "\n", sep = "")
  cat("Number of scenarios: ", x$n_scenarios, "\n\n", sep = "")
  
  # create summary table
  summary_data <- data.frame(
    Scenario = character(),
    N_Shadows = integer(),
    stringsAsFactors = FALSE
  )
  
  # add columns for each estimand
  for (estimand in x$estimands) {
    summary_data[[toupper(estimand)]] <- numeric()
  }
  
  # fill in data
  for (i in seq_along(x$scenario_names)) {
    name <- x$scenario_names[i]
    result <- x$results[[name]]
    
    row_data <- data.frame(
      Scenario = name,
      N_Shadows = result$n_shadows,
      stringsAsFactors = FALSE
    )
    
    for (estimand in x$estimands) {
      row_data[[toupper(estimand)]] <- result$effects[[estimand]]
    }
    
    summary_data <- rbind(summary_data, row_data)
  }
  
  # round numeric columns
  numeric_cols <- sapply(summary_data, is.numeric)
  summary_data[numeric_cols] <- round(summary_data[numeric_cols], digits)
  
  print(summary_data, row.names = FALSE)
  
  # calculate range of effects
  cat("\nSensitivity Analysis:\n")
  for (estimand in x$estimands) {
    effects <- sapply(x$results, function(r) r$effects[[estimand]])
    cat("  ", toupper(estimand), " range: [", 
        round(min(effects), digits), ", ", 
        round(max(effects), digits), "]\n", sep = "")
    
    # if oracle scenario exists, show bias
    if ("oracle" %in% tolower(x$scenario_names) || "Oracle" %in% x$scenario_names) {
      oracle_idx <- which(tolower(x$scenario_names) == "oracle")[1]
      oracle_effect <- effects[oracle_idx]
      
      cat("    Bias from oracle:\n")
      for (i in seq_along(x$scenario_names)) {
        if (i != oracle_idx) {
          bias <- effects[i] - oracle_effect
          rel_bias <- (bias / oracle_effect) * 100
          cat("      ", x$scenario_names[i], ": ", 
              round(bias, digits), " (", 
              round(rel_bias, 1), "%)\n", sep = "")
        }
      }
    }
  }
  
  invisible(x)
}

#' Plot scenario comparison
#'
#' @param x A margot_scenario_comparison object
#' @param estimand Which estimand to plot (default "ate")
#' @param ... Additional arguments passed to plotting functions
#'
#' @return A ggplot object
#' @export
plot.margot_scenario_comparison <- function(x, estimand = "ate", ...) {
  # check if ggplot2 is available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting")
  }
  
  # extract data for plotting
  plot_data <- data.frame(
    Scenario = x$scenario_names,
    Effect = sapply(x$results, function(r) r$effects[[estimand]]),
    N_Shadows = sapply(x$results, function(r) r$n_shadows),
    stringsAsFactors = FALSE
  )
  
  # order by effect size
  plot_data <- plot_data[order(plot_data$Effect), ]
  plot_data$Scenario <- factor(plot_data$Scenario, levels = plot_data$Scenario)
  
  # create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Effect, y = Scenario)) +
    ggplot2::geom_point(ggplot2::aes(size = N_Shadows), alpha = 0.7) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
    ggplot2::labs(
      title = paste("Scenario Comparison:", toupper(estimand)),
      x = paste(toupper(estimand), "Estimate"),
      y = "Scenario",
      size = "Number of\nShadows"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank()
    )
  
  # add oracle line if present
  if ("oracle" %in% tolower(plot_data$Scenario) || "Oracle" %in% plot_data$Scenario) {
    oracle_idx <- which(tolower(plot_data$Scenario) == "oracle")[1]
    oracle_effect <- plot_data$Effect[oracle_idx]
    
    p <- p + ggplot2::geom_vline(
      xintercept = oracle_effect,
      linetype = "dotted",
      color = "red",
      alpha = 0.7
    ) +
    ggplot2::annotate(
      "text",
      x = oracle_effect,
      y = nrow(plot_data) + 0.5,
      label = "Oracle",
      color = "red",
      size = 3,
      hjust = -0.1
    )
  }
  
  return(p)
}

# Scenario Sensitivity Analysis -------------------------------------------

#' Perform sensitivity analysis across scenarios
#'
#' @description
#' Systematically evaluates how causal conclusions change across a set of
#' scenarios representing different assumptions.
#'
#' @param data_generator Function that generates data (or fixed dataset)
#' @param scenarios List of scenarios to evaluate
#' @param intervention Intervention specification
#' @param outcome_var Character, name of outcome variable
#' @param n_sim Number of simulations per scenario
#' @param parallel Logical, whether to use parallel processing
#' @param verbose Logical, whether to show progress
#'
#' @return A sensitivity analysis object
#' @export
sensitivity_analysis <- function(data_generator,
                               scenarios,
                               intervention,
                               outcome_var,
                               n_sim = 100,
                               parallel = FALSE,
                               verbose = TRUE) {
  
  # determine if data_generator is a function or fixed data
  if (is.function(data_generator)) {
    use_fixed_data <- FALSE
  } else if (is.data.frame(data_generator)) {
    use_fixed_data <- TRUE
    fixed_data <- data_generator
  } else {
    stop("data_generator must be either a function or a data frame")
  }
  
  # set up parallel processing if requested
  if (parallel && requireNamespace("future.apply", quietly = TRUE)) {
    if (verbose) cat("Using parallel processing\n")
    future::plan(future::multisession)
    apply_fun <- future.apply::future_lapply
  } else {
    apply_fun <- lapply
  }
  
  # run simulations for each scenario
  all_results <- list()
  
  for (i in seq_along(scenarios)) {
    scenario <- scenarios[[i]]
    scenario_name <- names(scenarios)[i]
    
    if (is.null(scenario_name)) {
      scenario_name <- scenario$name
    }
    
    if (verbose) {
      cat("\nProcessing scenario: ", scenario_name, " (", i, "/", 
          length(scenarios), ")\n", sep = "")
    }
    
    # run simulations
    scenario_results <- apply_fun(1:n_sim, function(sim_i) {
      # generate or use data
      if (use_fixed_data) {
        sim_data <- fixed_data
      } else {
        sim_data <- data_generator()
      }
      
      # apply scenario
      scenario_data <- apply_scenario(sim_data, scenario, verbose = FALSE)
      
      # apply intervention and compute effects
      # (this is simplified - would need actual intervention application)
      effects <- compute_causal_effects(
        data = scenario_data$data_observed,
        exposure = names(intervention),
        outcome = outcome_var
      )
      
      return(effects)
    })
    
    all_results[[scenario_name]] <- scenario_results
  }
  
  # clean up parallel processing
  if (parallel && requireNamespace("future.apply", quietly = TRUE)) {
    future::plan(future::sequential)
  }
  
  # create sensitivity analysis object
  sensitivity_result <- structure(
    list(
      scenarios = scenarios,
      results = all_results,
      n_sim = n_sim,
      outcome_var = outcome_var,
      timestamp = Sys.time()
    ),
    class = "margot_sensitivity_analysis"
  )
  
  return(sensitivity_result)
}

#' Summarise sensitivity analysis results
#'
#' @param object A margot_sensitivity_analysis object
#' @param ... Additional arguments (ignored)
#'
#' @return Summary data frame
#' @export
summary.margot_sensitivity_analysis <- function(object, ...) {
  # extract results for each scenario
  summary_list <- list()
  
  for (scenario_name in names(object$results)) {
    scenario_results <- object$results[[scenario_name]]
    
    # extract ATEs across simulations
    ates <- sapply(scenario_results, function(x) x$ate)
    
    summary_list[[scenario_name]] <- data.frame(
      Scenario = scenario_name,
      Mean_ATE = mean(ates),
      SD_ATE = sd(ates),
      Min_ATE = min(ates),
      Max_ATE = max(ates),
      N_Sims = length(ates),
      stringsAsFactors = FALSE
    )
  }
  
  summary_df <- do.call(rbind, summary_list)
  rownames(summary_df) <- NULL
  
  return(summary_df)
}