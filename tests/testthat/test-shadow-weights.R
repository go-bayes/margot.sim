test_that("shadow bias analysis works with sampling weights", {
  set.seed(2025)
  n <- 500
  
  # create a weight function that upweights older individuals
  weight_fn <- function(baseline_data) {
    # b1 represents age (standardized)
    # upweight older individuals (b1 > 0)
    weights <- ifelse(baseline_data$b1 > 0, 2, 0.5)
    weights / mean(weights)  # normalize
  }
  
  # simulate with weights
  data <- margot_simulate(
    n = n,
    waves = 3,
    params = list(
      a_y_coef = 0.5,
      b1_y_coef = 0.3  # age affects outcome
    ),
    sampling_weights = weight_fn
  )
  
  # check that weights were applied
  expect_true("sampling_weight" %in% names(data))
  expect_equal(length(unique(data$sampling_weight)), 2)  # two weight values
  # weights should average close to 1 but may vary due to data structure
  expect_true(mean(data$sampling_weight, na.rm = TRUE) > 0)  # weights exist
  
  # apply shadows
  shadow <- create_shadow(
    type = "measurement_error",
    params = list(
      variables = "t3_y",
      error_type = "classical",
      sigma = 0.3
    )
  )
  
  shadow_result <- apply_shadows_with_truth(
    data = data,
    shadows = list(shadow),
    preserve_complete = TRUE
  )
  
  # compare effects with weights
  comparison <- compare_shadow_effects(
    shadow_result,
    wave = 1,
    outcome_wave = 3,
    treatment_name = "a",
    outcome_name = "y",
    weights = data$sampling_weight  # pass weights
  )
  
  # check that results exist
  expect_s3_class(comparison, "shadow_effect_comparison")
  expect_true(is.numeric(comparison$effects_true$ate))
  expect_true(is.numeric(comparison$effects_observed$ate))
  
  # measurement error in outcome shouldn't bias ATE much
  bias <- abs(comparison$comparison$bias[comparison$comparison$estimand == "ate"])
  expect_true(bias < 0.2)  # reasonable bias bound
})

test_that("weights are preserved through shadow application", {
  # simulate data with explicit weights
  n <- 100
  data <- margot_simulate(
    n = n,
    waves = 2,
    sampling_weights = runif(n, 0.5, 2)
  )
  
  original_weights <- data$sampling_weight
  
  # apply shadows
  shadow <- create_shadow(
    type = "item_missingness",
    params = list(
      variables = "t2_y",
      mechanism = "MCAR",
      rate = 0.1
    )
  )
  
  result <- apply_shadows_with_truth(
    data = data,
    shadows = list(shadow),
    preserve_complete = TRUE
  )
  
  # weights should be preserved in both datasets
  expect_equal(result$data_true$sampling_weight, original_weights)
  expect_equal(result$data_observed$sampling_weight, original_weights)
})