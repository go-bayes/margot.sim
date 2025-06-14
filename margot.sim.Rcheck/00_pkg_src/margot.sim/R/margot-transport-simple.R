#' Direct Replacement for simulate_ate_data_with_weights
#'
#' This is a simplified direct replacement that exactly mimics the old function's
#' behavior without using the full margot.sim machinery.
#'
#' @param n_sample Sample size
#' @param n_population Population size
#' @param p_z_sample Probability of effect modifier Z=1 in sample
#' @param p_z_population Probability of effect modifier Z=1 in population
#' @param beta_a Treatment effect
#' @param beta_z Effect modifier coefficient
#' @param beta_az Treatment-by-modifier interaction
#' @param noise_sd Standard deviation of outcome noise
#' @param seed Random seed for reproducibility
#'
#' @return List containing sample_data and population_data with appropriate weights
#'
#' @examples
#' # Direct replacement usage
#' data <- simulate_ate_data_with_weights(
#'   n_sample = 1000,
#'   n_population = 10000,
#'   p_z_sample = 0.1,
#'   p_z_population = 0.5
#' )
#'
#' # Check weighted vs unweighted ATE
#' with(data$sample_data, {
#'   cat("Unweighted ATE:", mean(y_sample[a_sample==1]) - mean(y_sample[a_sample==0]), "\n")
#'   cat("Weighted ATE:", 
#'       weighted.mean(y_sample[a_sample==1], weights[a_sample==1]) - 
#'       weighted.mean(y_sample[a_sample==0], weights[a_sample==0]), "\n")
#' })
#'
#' @export
simulate_ate_data_with_weights <- function(n_sample = 10000, 
                                           n_population = 100000,
                                           p_z_sample = 0.1, 
                                           p_z_population = 0.5,
                                           beta_a = 1, 
                                           beta_z = 2.5, 
                                           beta_az = 0.5,
                                           noise_sd = 0.5, 
                                           seed = NULL) {
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # create sample data
  z_sample <- rbinom(n_sample, 1, p_z_sample)
  a_sample <- rbinom(n_sample, 1, 0.5)
  
  # simulate outcome
  y_sample <- beta_a * a_sample + beta_z * z_sample + beta_az * (a_sample * z_sample) +
    rnorm(n_sample, mean = 0, sd = noise_sd)
  
  # put sample data into data frame
  sample_data <- data.frame(y_sample, a_sample, z_sample)
  
  # simulate population data
  z_population <- rbinom(n_population, 1, p_z_population)
  a_population <- rbinom(n_population, 1, 0.5)
  y_population <- beta_a * a_population + beta_z * z_population +
    beta_az * (a_population * z_population) + rnorm(n_population, mean = 0, sd = noise_sd)
  
  # put population data in dataframe
  population_data <- data.frame(y_population, a_population, z_population)
  
  # calculate weights based on z distribution difference
  weight_z_1 <- p_z_population / p_z_sample  # weight for Z=1
  weight_z_0 <- (1 - p_z_population) / (1 - p_z_sample)  # weight for Z=0
  weights <- ifelse(z_sample == 1, weight_z_1, weight_z_0)
  
  # add weights to sample_data
  sample_data$weights <- weights
  
  # return list of data frames
  list(sample_data = sample_data, population_data = population_data)
}

#' Enhanced Version Using margot.sim Shadow Framework
#'
#' This demonstrates how to extend the basic transport weights example
#' to include realistic observational shadows using margot.sim.
#'
#' @param n_sample Sample size
#' @param p_z_sample Probability of effect modifier in sample
#' @param p_z_population Probability of effect modifier in population
#' @param beta_a Treatment effect
#' @param beta_z Effect modifier coefficient
#' @param beta_az Treatment-by-modifier interaction
#' @param apply_shadows Logical, whether to apply observational shadows
#' @param shadow_config List specifying which shadows to apply
#' @param seed Random seed
#'
#' @return A list with data, weights, and shadow bias analysis
#'
#' @examples
#' # Basic usage without shadows
#' result1 <- margot_transport_analysis(
#'   n_sample = 1000,
#'   apply_shadows = FALSE
#' )
#'
#' # With measurement error and missingness
#' result2 <- margot_transport_analysis(
#'   n_sample = 1000,
#'   apply_shadows = TRUE,
#'   shadow_config = list(
#'     measurement_error = TRUE,
#'     missingness = TRUE
#'   )
#' )
#'
#' print(result2$bias_comparison)
#'
#' @export
margot_transport_analysis <- function(n_sample = 1000,
                                      p_z_sample = 0.1,
                                      p_z_population = 0.5,
                                      beta_a = 1,
                                      beta_z = 2.5,
                                      beta_az = 0.5,
                                      apply_shadows = FALSE,
                                      shadow_config = NULL,
                                      seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  # first generate using the simple function
  base_data <- simulate_ate_data_with_weights(
    n_sample = n_sample,
    n_population = n_sample * 10,
    p_z_sample = p_z_sample,
    p_z_population = p_z_population,
    beta_a = beta_a,
    beta_z = beta_z,
    beta_az = beta_az,
    seed = seed
  )
  
  # convert to margot format (waves structure)
  # create a data frame that looks like margot output
  margot_data <- data.frame(
    id = seq_len(n_sample),
    b1 = base_data$sample_data$z_sample,  # effect modifier as baseline covariate
    t0_a = base_data$sample_data$a_sample,  # treatment at wave 0
    t2_y = base_data$sample_data$y_sample,  # outcome at wave 2
    sampling_weight = base_data$sample_data$weights
  )
  
  # add margot metadata
  attr(margot_data, "n_waves") <- 3
  attr(margot_data, "time_points") <- c("t0", "t1", "t2")
  class(margot_data) <- c("tbl_df", "tbl", "data.frame")
  
  # if no shadows requested, just compute effects
  if (!apply_shadows) {
    # compute unweighted effects
    effects_unweighted <- compute_effects_from_sim(
      margot_data,
      wave = 0,
      outcome_wave = 2,
      treatment_name = "a",
      outcome_name = "y"
    )
    
    # compute weighted effects (transport to population)
    effects_weighted <- compute_effects_from_sim(
      margot_data,
      wave = 0,
      outcome_wave = 2,
      treatment_name = "a",
      outcome_name = "y",
      weights = margot_data$sampling_weight
    )
    
    return(list(
      data = margot_data,
      effects_sample = effects_unweighted,
      effects_population = effects_weighted,
      weights_summary = summary(margot_data$sampling_weight)
    ))
  }
  
  # apply shadows based on config
  if (is.null(shadow_config)) {
    shadow_config <- list(
      measurement_error = TRUE,
      missingness = TRUE
    )
  }
  
  shadows <- list()
  
  if (shadow_config$measurement_error) {
    shadows <- append(shadows, list(
      create_shadow(
        type = "measurement_error",
        params = list(
          variables = "t0_a",
          error_type = "classical",
          sigma = 0.2
        ),
        name = "treatment_measurement"
      )
    ))
  }
  
  if (shadow_config$missingness) {
    shadows <- append(shadows, list(
      create_shadow(
        type = "item_missingness",
        params = list(
          variables = "t2_y",
          mechanism = "MAR",
          rate = 0.1,
          dependent_vars = "b1"  # depends on effect modifier
        ),
        name = "differential_missingness"
      )
    ))
  }
  
  # apply shadows
  shadow_result <- apply_shadows_with_truth(
    data = margot_data,
    shadows = shadows,
    preserve_complete = TRUE
  )
  
  # compare effects in sample (unweighted)
  comparison_sample <- compare_shadow_effects(
    shadow_result,
    wave = 0,
    outcome_wave = 2,
    treatment_name = "a",
    outcome_name = "y"
  )
  
  # compare effects in population (weighted)
  comparison_population <- compare_shadow_effects(
    shadow_result,
    wave = 0,
    outcome_wave = 2,
    treatment_name = "a",
    outcome_name = "y",
    weights = margot_data$sampling_weight
  )
  
  # create summary comparison
  bias_comparison <- data.frame(
    Population = c("Sample (unweighted)", "Target (weighted)"),
    True_ATE = c(
      comparison_sample$effects_true$ate,
      comparison_population$effects_true$ate
    ),
    Observed_ATE = c(
      comparison_sample$effects_observed$ate,
      comparison_population$effects_observed$ate
    ),
    Bias = c(
      comparison_sample$comparison$bias[1],
      comparison_population$comparison$bias[1]
    ),
    Relative_Bias = c(
      comparison_sample$comparison$relative_bias[1],
      comparison_population$comparison$relative_bias[1]
    )
  )
  
  return(list(
    data = shadow_result,
    comparison_sample = comparison_sample,
    comparison_population = comparison_population,
    bias_comparison = bias_comparison,
    weights_summary = summary(margot_data$sampling_weight),
    sample_z_prop = mean(margot_data$b1),
    target_z_prop = p_z_population
  ))
}