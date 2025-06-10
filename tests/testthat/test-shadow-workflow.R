test_that("apply_shadows_with_truth preserves complete data", {
  # simulate data
  set.seed(789)
  data <- margot_simulate(
    n = 100,
    waves = 3,
    params = list(a_y_coef = 0.5)
  )
  
  # create shadows
  shadows <- list(
    create_shadow(
      type = "measurement_error",
      params = list(
        variables = c("t1_a", "t2_a"),
        error_type = "classical",
        sigma = 0.2
      )
    ),
    create_shadow(
      type = "item_missingness",
      params = list(
        variables = c("t3_y"),
        mechanism = "MCAR",
        rate = 0.1
      )
    )
  )
  
  # apply shadows with truth preservation
  result <- apply_shadows_with_truth(
    data = data,
    shadows = shadows,
    preserve_complete = TRUE,
    verbose = FALSE
  )
  
  # check structure
  expect_s3_class(result, "margot_shadow_result")
  expect_true("data_true" %in% names(result))
  expect_true("data_observed" %in% names(result))
  expect_equal(nrow(result$data_true), nrow(result$data_observed))
  
  # check that true data is unchanged
  expect_identical(result$data_true, data)
  
  # check that shadows were applied
  expect_true("t1_a_true" %in% names(result$data_observed))
  expect_true("t2_a_true" %in% names(result$data_observed))
  
  # check metadata
  expect_equal(length(result$shadows_applied), 2)
})

test_that("compare_shadow_effects computes bias correctly", {
  # simulate data with known effect
  set.seed(101)
  n <- 500
  data <- data.frame(
    id = 1:n,
    t1_a = rbinom(n, 1, 0.5),
    t3_y = rnorm(n, mean = 0)
  )
  # add true treatment effect
  data$t3_y[data$t1_a == 1] <- data$t3_y[data$t1_a == 1] + 0.8
  
  # create measurement error shadow
  shadow <- create_shadow(
    type = "measurement_error",
    params = list(
      variables = "t3_y",
      error_type = "classical",
      sigma = 0.5  # substantial error
    )
  )
  
  # apply shadow
  result <- apply_shadows_with_truth(
    data = data,
    shadows = list(shadow),
    preserve_complete = TRUE
  )
  
  # compare effects
  comparison <- compare_shadow_effects(
    result,
    wave = 1,
    outcome_wave = 3,
    treatment_name = "a",
    outcome_name = "y"
  )
  
  # check structure
  expect_s3_class(comparison, "shadow_effect_comparison")
  expect_true("effects_true" %in% names(comparison))
  expect_true("effects_observed" %in% names(comparison))
  expect_true("comparison" %in% names(comparison))
  
  # true effect should be close to 0.8
  expect_true(abs(comparison$effects_true$ate - 0.8) < 0.2)
  
  # observed effect should be attenuated (closer to 0) due to measurement error
  # adding noise to outcome should not bias the estimate, just increase variance
  expect_true(abs(comparison$effects_observed$ate - 0.8) < 0.3)
})

test_that("apply_shadows_with_truth works without preservation", {
  # simulate data
  data <- margot_simulate(n = 50, waves = 2)
  
  # create shadow
  shadow <- create_shadow(
    type = "measurement_error",
    params = list(
      variables = "t1_a",
      error_type = "classical",
      sigma = 0.1
    )
  )
  
  # apply without preserving complete data
  result <- apply_shadows_with_truth(
    data = data,
    shadows = list(shadow),
    preserve_complete = FALSE
  )
  
  # should return data frame, not list
  expect_true(is.data.frame(result))
  expect_false(inherits(result, "margot_shadow_result"))
  
  # should still have truth columns
  expect_true("t1_a_true" %in% names(result))
})