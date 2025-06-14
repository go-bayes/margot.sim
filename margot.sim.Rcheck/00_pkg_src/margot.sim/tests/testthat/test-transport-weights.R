test_that("simulate_ate_data_with_weights works as expected", {
  set.seed(2025)
  
  # test basic functionality
  data <- simulate_ate_data_with_weights(
    n_sample = 1000,
    n_population = 10000,
    p_z_sample = 0.1,
    p_z_population = 0.5,
    beta_a = 1,
    beta_z = 2,
    beta_az = 0.5
  )
  
  # check structure
  expect_equal(names(data), c("sample_data", "population_data"))
  expect_equal(nrow(data$sample_data), 1000)
  expect_equal(nrow(data$population_data), 10000)
  expect_true("weights" %in% names(data$sample_data))
  
  # check z distribution
  expect_true(abs(mean(data$sample_data$z_sample) - 0.1) < 0.05)
  expect_true(abs(mean(data$population_data$z_population) - 0.5) < 0.05)
  
  # check weights calculation
  # weight for Z=1: 0.5/0.1 = 5
  # weight for Z=0: 0.5/0.9 = 0.556
  z1_weights <- data$sample_data$weights[data$sample_data$z_sample == 1]
  z0_weights <- data$sample_data$weights[data$sample_data$z_sample == 0]
  expect_true(all(abs(z1_weights - 5) < 0.001))
  expect_true(all(abs(z0_weights - 0.556) < 0.01))
})

test_that("margot_transport_analysis integrates with shadow framework", {
  set.seed(2025)
  
  # without shadows
  result_clean <- margot_transport_analysis(
    n_sample = 500,
    apply_shadows = FALSE,
    p_z_sample = 0.2,
    p_z_population = 0.6
  )
  
  # check structure
  expect_true("data" %in% names(result_clean))
  expect_true("effects_sample" %in% names(result_clean))
  expect_true("effects_population" %in% names(result_clean))
  
  # sample and population effects should differ due to effect modification
  expect_false(
    abs(result_clean$effects_sample$ate - 
        result_clean$effects_population$ate) < 0.01
  )
  
  # with shadows
  result_shadow <- margot_transport_analysis(
    n_sample = 500,
    apply_shadows = TRUE,
    p_z_sample = 0.2,
    p_z_population = 0.6,
    shadow_config = list(
      measurement_error = TRUE,
      missingness = FALSE
    )
  )
  
  # check enhanced structure
  expect_true("bias_comparison" %in% names(result_shadow))
  expect_true("comparison_sample" %in% names(result_shadow))
  expect_true("comparison_population" %in% names(result_shadow))
  
  # bias should exist
  expect_true(all(abs(result_shadow$bias_comparison$Bias) > 0))
})

test_that("transport weights preserve population ATE", {
  set.seed(123)
  
  # parameters
  params <- list(
    n = 2000,
    p_z_sample = 0.1,
    p_z_population = 0.5,
    beta_a = 1,
    beta_z = 2,
    beta_az = 1  # strong interaction
  )
  
  # generate data
  data <- simulate_ate_data_with_weights(
    n_sample = params$n,
    n_population = params$n * 10,
    p_z_sample = params$p_z_sample,
    p_z_population = params$p_z_population,
    beta_a = params$beta_a,
    beta_z = params$beta_z,
    beta_az = params$beta_az
  )
  
  # calculate true population ATE
  # E[Y(1) - Y(0)] = beta_a + beta_az * P(Z=1)
  true_pop_ate <- params$beta_a + params$beta_az * params$p_z_population
  
  # calculate weighted ATE in sample
  sample_df <- data$sample_data
  weighted_ate <- with(sample_df, {
    y1 <- weighted.mean(y_sample[a_sample == 1], weights[a_sample == 1])
    y0 <- weighted.mean(y_sample[a_sample == 0], weights[a_sample == 0])
    y1 - y0
  })
  
  # should be close to true population ATE
  expect_true(abs(weighted_ate - true_pop_ate) < 0.2)
  
  # unweighted ATE should be different (biased for population)
  unweighted_ate <- with(sample_df, {
    mean(y_sample[a_sample == 1]) - mean(y_sample[a_sample == 0])
  })
  
  # unweighted should approximate sample ATE
  true_sample_ate <- params$beta_a + params$beta_az * params$p_z_sample
  expect_true(abs(unweighted_ate - true_sample_ate) < 0.2)
  
  # weighted and unweighted should differ substantially
  expect_true(abs(weighted_ate - unweighted_ate) > 0.2)
})