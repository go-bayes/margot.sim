library(margot.sim)

test_that("heterogeneous effects are correctly implemented", {
  set.seed(123)
  
  # Create data with strong heterogeneity
  het_params <- list(
    a_lag_y_coef = 0.3,      # main effect
    a_b1_y_het = 0.5,        # strong heterogeneity by b1
    a_y0_y_het = 0.4,        # strong heterogeneity by baseline outcome
    a_a0_y_het = 0.3         # heterogeneity by baseline exposure
  )
  
  dat <- margot_simulate(
    n = 1000,
    waves = 2,
    params = het_params,
    y_feedback = "full"
  )
  
  # Test 1: Data structure includes baseline outcome
  expect_true("t0_y" %in% names(dat))
  
  # Test 2: Heterogeneity by b1 - split into high/low groups
  dat$b1_high <- dat$b1 > median(dat$b1)
  
  # Calculate treatment effects in each group
  ate_low_b1 <- mean(dat$t3_y[dat$t2_a == 1 & !dat$b1_high]) - 
                mean(dat$t3_y[dat$t2_a == 0 & !dat$b1_high])
  
  ate_high_b1 <- mean(dat$t3_y[dat$t2_a == 1 & dat$b1_high]) - 
                 mean(dat$t3_y[dat$t2_a == 0 & dat$b1_high])
  
  # With positive heterogeneity coefficient, effect should be larger in high b1 group
  expect_true(ate_high_b1 > ate_low_b1)
  
  # Test 3: Check regression coefficients align with parameters
  # Simplified model focusing on key interactions
  fit <- lm(t3_y ~ t2_a * b1 + t2_a * t0_y, data = dat)
  coefs <- coef(fit)
  
  # The interaction terms should be positive given our parameters
  expect_true(coefs["t2_a:b1"] > 0)
  expect_true(coefs["t2_a:t0_y"] > 0)
})

test_that("heterogeneous effects work with multiple outcomes", {
  set.seed(456)
  
  het_params <- list(
    a_b1_y_het = 0.3,
    y2_shrink = 0.8  # Second outcome has shrunken effects
  )
  
  dat <- margot_simulate(
    n = 500,
    waves = 1,
    n_outcomes = 2,
    params = het_params
  )
  
  # Both outcomes should exist
  expect_true(all(c("t2_y1", "t2_y2") %in% names(dat)))
  
  # Fit models for both outcomes
  fit1 <- lm(t2_y1 ~ t1_a * b1, data = dat)
  fit2 <- lm(t2_y2 ~ t1_a * b1, data = dat)
  
  # Heterogeneity should be weaker for y2 due to shrinkage
  het_effect_y1 <- abs(coef(fit1)["t1_a:b1"])
  het_effect_y2 <- abs(coef(fit2)["t1_a:b1"])
  
  # y2 heterogeneity should be approximately 80% of y1
  ratio <- het_effect_y2 / het_effect_y1
  expect_true(ratio < 1)  # Should be shrunken
})

test_that("example_heterogeneous_effects runs without error", {
  expect_silent({
    results <- example_heterogeneous_effects(
      n = 200, 
      waves = 1, 
      seed = 789,
      plot = FALSE,
      verbose = FALSE
    )
  })
  
  expect_type(results, "list")
  expect_s3_class(results$data, "data.frame")
  expect_true("het_by_b1" %in% names(results))
  expect_true("het_by_y0" %in% names(results))
  expect_true("het_model" %in% names(results))
})

test_that("analyze_heterogeneity works correctly", {
  set.seed(999)
  
  dat <- margot_simulate(
    n = 500,
    waves = 2,
    params = list(a_b1_y_het = 0.4)
  )
  
  het_analysis <- analyze_heterogeneity(
    dat,
    treatment_wave = 2,
    outcome_wave = 3,
    effect_modifiers = c("b1", "b2")
  )
  
  expect_s3_class(het_analysis, "data.frame")
  expect_true(all(c("modifier", "correlation", "p_value") %in% names(het_analysis)))
  expect_equal(nrow(het_analysis), 2)  # Should have results for b1 and b2
})

test_that("heterogeneous effects respect parameter bounds", {
  set.seed(111)
  
  # Test with extreme heterogeneity parameters
  extreme_params <- list(
    a_lag_y_coef = 0.1,
    a_b1_y_het = 2.0,  # Very large
    a_b2_y_het = -1.5  # Negative heterogeneity
  )
  
  # Should run without error despite extreme values
  expect_silent({
    dat <- margot_simulate(
      n = 200,
      waves = 1,
      params = extreme_params,
      verbose = FALSE
    )
  })
  
  # Check data integrity
  expect_false(any(is.infinite(dat$t2_y)))
  expect_true(sum(!is.na(dat$t2_y)) > 0)
})