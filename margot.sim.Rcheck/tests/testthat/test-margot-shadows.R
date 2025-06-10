test_that("measurement error shadows work correctly", {
  # generate data
  dat <- margot_simulate(n = 100, waves = 2, seed = 123)
  
  # create classical measurement error shadow
  shadow <- create_shadow(
    "measurement_error",
    params = list(
      variables = c("t1_l", "t2_l"),
      error_type = "classical",
      sigma = 0.5
    )
  )
  
  # apply shadow
  shadowed_dat <- apply_shadow(dat, shadow)
  
  # check that original values are stored
  expect_true("t1_l_true" %in% names(shadowed_dat))
  expect_true("t2_l_true" %in% names(shadowed_dat))
  
  # check that variance increased
  expect_gt(var(shadowed_dat$t1_l, na.rm = TRUE), var(dat$t1_l, na.rm = TRUE))
})

test_that("item missingness shadows work correctly", {
  dat <- margot_simulate(n = 100, waves = 2, seed = 123)
  
  # MCAR missingness
  shadow_mcar <- create_item_missingness_shadow(
    variables = c("t1_l", "t2_l"),
    missing_rate = 0.2,
    missing_mechanism = "MCAR"
  )
  
  shadowed_dat <- apply_shadow(dat, shadow_mcar)
  
  # check that some values are now missing
  expect_true(any(is.na(shadowed_dat$t1_l)))
  expect_true(any(is.na(shadowed_dat$t2_l)))
  
  # rate should be approximately 0.2
  actual_rate <- mean(is.na(shadowed_dat$t1_l))
  expect_true(actual_rate > 0.1 && actual_rate < 0.3)
})

test_that("MAR missingness depends on other variables", {
  dat <- margot_simulate(n = 500, waves = 2, seed = 123)
  
  # MAR missingness depending on baseline
  shadow_mar <- create_item_missingness_shadow(
    variables = "t1_l",
    missing_rate = 0.2,
    missing_mechanism = "MAR",
    dependent_vars = "b1"
  )
  
  shadowed_dat <- apply_shadow(dat, shadow_mar)
  
  # missingness should be related to b1
  # split by b1 median and check missingness rates
  b1_high <- shadowed_dat$b1 > median(shadowed_dat$b1, na.rm = TRUE)
  miss_rate_high <- mean(is.na(shadowed_dat$t1_l[b1_high]))
  miss_rate_low <- mean(is.na(shadowed_dat$t1_l[!b1_high]))
  
  # rates should differ (not a perfect test but reasonable)
  expect_true(abs(miss_rate_high - miss_rate_low) > 0.01)
})

test_that("positivity shadows filter correctly", {
  dat <- margot_simulate(n = 200, waves = 2, seed = 123)
  
  # create positivity shadow: treatment impossible if b1 + b2 > 2
  pos_shadow <- create_positivity_shadow(
    exposure_var = "t1_a",
    filter_fn = function(data) {
      risk_score <- data$b1 + data$b2
      risk_score <= 2  # can only treat if risk score <= 2
    }
  )
  
  filtered_dat <- apply_shadow(dat, pos_shadow)
  
  # check that rows were filtered
  expect_lt(nrow(filtered_dat), nrow(dat))
  
  # check that all remaining rows satisfy constraint
  remaining_risk <- filtered_dat$b1 + filtered_dat$b2
  expect_true(all(remaining_risk <= 2))
  
  # check attribute
  filter_info <- attr(filtered_dat, "positivity_filter")
  expect_equal(filter_info$n_original, 200)
  expect_equal(filter_info$n_filtered, nrow(filtered_dat))
})

test_that("multiple shadows can be applied sequentially", {
  dat <- margot_simulate(n = 100, waves = 2, seed = 123)
  
  # create multiple shadows
  shadows <- list(
    create_shadow(
      "measurement_error",
      params = list(
        variables = "t1_l",
        error_type = "classical",
        sigma = 0.3
      )
    ),
    create_item_missingness_shadow(
      variables = "t2_l",
      missing_rate = 0.1,
      missing_mechanism = "MCAR"
    )
  )
  
  # apply shadows
  shadowed_dat <- apply_shadows(dat, shadows)
  
  # check both shadows were applied
  expect_true("t1_l_true" %in% names(shadowed_dat))
  expect_true(any(is.na(shadowed_dat$t2_l)))
  
  # check metadata
  expect_equal(attr(shadowed_dat, "applied_shadows"), 
               list("measurement_error_shadow", "item_missingness"))
})

test_that("shadow analysis functions work", {
  dat <- margot_simulate(n = 100, waves = 2, seed = 123)
  
  shadow <- create_shadow(
    "measurement_error",
    params = list(
      variables = "t1_l",
      error_type = "classical",
      sigma = 1
    )
  )
  
  shadowed_dat <- apply_shadow(dat, shadow)
  
  # analyse effects
  effects <- analyse_shadow_effects(dat, shadowed_dat, variables = "t1_l")
  
  expect_s3_class(effects, "shadow_effects")
  expect_true("t1_l" %in% names(effects))
  expect_gt(effects$t1_l$sd_ratio, 1)  # sd should increase
})

test_that("misclassification shadow works correctly for binary variables", {
  dat <- margot_simulate(n = 500, waves = 2, seed = 123)
  
  # create misclassification shadow
  shadow <- create_shadow(
    type = "measurement_error",
    params = list(
      variables = "t1_a",
      error_type = "misclassification",
      sensitivity = 0.8,  # 80% of true 1s correctly classified
      specificity = 0.9   # 90% of true 0s correctly classified
    )
  )
  
  shadowed_dat <- apply_shadow(dat, shadow)
  
  # check that original values are stored
  expect_true("t1_a_true" %in% names(shadowed_dat))
  
  # calculate observed sensitivity and specificity
  true_1s <- which(shadowed_dat$t1_a_true == 1)
  true_0s <- which(shadowed_dat$t1_a_true == 0)
  
  obs_sensitivity <- mean(shadowed_dat$t1_a[true_1s] == 1, na.rm = TRUE)
  obs_specificity <- mean(shadowed_dat$t1_a[true_0s] == 0, na.rm = TRUE)
  
  # should be close to specified values (within sampling variability)
  expect_true(abs(obs_sensitivity - 0.8) < 0.1)
  expect_true(abs(obs_specificity - 0.9) < 0.1)
})