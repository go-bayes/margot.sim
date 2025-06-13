# Test positivity and diagnostic functions

test_that("Positivity check detects violations", {
  set.seed(123)
  n <- 1000
  
  # create data with positivity violation
  # treatment strongly depends on x1
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  data <- data.frame(
    x1 = x1,
    x2 = x2,
    a = rbinom(n, 1, plogis(3 * x1)),  # strong dependence creates violations
    y = rnorm(n)
  )
  
  # check positivity
  pos_check <- margot_check_positivity(
    data = data,
    treatment = "a",
    covariates = c("x1", "x2"),
    threshold = 0.1,
    method = "both"
  )
  
  # structure checks
  expect_s3_class(pos_check, "margot_positivity_diagnostic")
  expect_true("empirical" %in% names(pos_check))
  expect_true("model_based" %in% names(pos_check))
  expect_true("summary" %in% names(pos_check))
  
  # should find violations
  expect_true(pos_check$empirical$n_violations > 0)
  expect_true(pos_check$model_based$n_violations > 0)
})

test_that("Positivity check handles edge cases", {
  data <- data.frame(
    x = rnorm(100),
    a = rbinom(100, 1, 0.5),
    y = rnorm(100)
  )
  
  # missing treatment
  expect_error(
    margot_check_positivity(data, "missing_var", "x"),
    "Treatment variable not found"
  )
  
  # missing covariate
  expect_error(
    margot_check_positivity(data, "a", c("x", "missing")),
    "Covariates not found"
  )
  
  # single treatment level
  data_single <- data
  data_single$a <- 1
  expect_error(
    margot_check_positivity(data_single, "a", "x"),
    "at least 2 levels"
  )
})

test_that("Empirical positivity check works correctly", {
  set.seed(456)
  n <- 500
  
  # create data with clear violation in one region
  x <- runif(n, -3, 3)
  data <- data.frame(
    x = x,
    a = ifelse(x > 0, 
              rbinom(sum(x > 0), 1, 0.8),  # high prob when x > 0
              rbinom(sum(x <= 0), 1, 0.2)), # low prob when x <= 0
    y = rnorm(n)
  )
  
  result <- check_positivity_empirical(
    data, "a", "x", c(0, 1), threshold = 0.3
  )
  
  # should find violations
  expect_true(result$n_violations > 0)
  expect_true(length(result$violations) > 0)
  
  # check violation structure
  if (length(result$violations) > 0) {
    v <- result$violations[[1]]
    expect_true("stratum" %in% names(v))
    expect_true("probability" %in% names(v))
    expect_true(v$probability < 0.3)
  }
})

test_that("Model-based positivity check works", {
  set.seed(789)
  n <- 1000
  
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  data <- data.frame(
    x1 = x1,
    x2 = x2,
    a = rbinom(n, 1, plogis(2 * x1 - 1 * x2))
  )
  
  result <- check_positivity_model(
    data, "a", c("x1", "x2"), c(0, 1), threshold = 0.05
  )
  
  # check structure
  expect_true("model" %in% names(result))
  expect_true("propensity_scores" %in% names(result))
  expect_true("violations" %in% names(result))
  expect_equal(length(result$propensity_scores), n)
  
  # propensity scores in [0,1]
  expect_true(all(result$propensity_scores >= 0))
  expect_true(all(result$propensity_scores <= 1))
})

test_that("Print method works for positivity diagnostic", {
  set.seed(111)
  x <- rnorm(100)
  data <- data.frame(
    x = x,
    a = rbinom(100, 1, plogis(2 * x)),
    y = rnorm(100)
  )
  
  pos_check <- margot_check_positivity(data, "a", "x", threshold = 0.1)
  
  # capture print output
  output <- capture.output(print(pos_check))
  
  expect_true(any(grepl("Margot Positivity Diagnostic", output)))
  expect_true(any(grepl("Sample size:", output)))
  expect_true(any(grepl("Treatment:", output)))
})

test_that("create_positivity_shadow_from_diagnostic works", {
  set.seed(222)
  n <- 500
  x <- rnorm(n)
  data <- data.frame(
    x = x,
    a = rbinom(n, 1, plogis(3 * x)),
    y = rnorm(n)
  )
  
  # get diagnostic
  diag <- margot_check_positivity(data, "a", "x", method = "model-based")
  
  # create shadow if violations exist
  if (diag$model_based$n_violations > 0) {
    shadow <- create_positivity_shadow_from_diagnostic(diag, method = "trim")
    
    expect_s3_class(shadow, "margot_shadow")
    expect_equal(shadow$type, "positivity")
    expect_equal(shadow$params$method, "trim")
  }
})

test_that("diagnose_all_shadows provides comprehensive diagnostics", {
  set.seed(333)
  n <- 200
  
  # use margot_simulate to get proper data structure
  data <- margot_simulate(
    n = n,
    waves = 2,
    params = list(a_var = 0.2)
  )
  
  # apply some shadows that work with margot data
  shadows <- list(
    create_shadow("measurement_error", 
                 params = list(variables = "t1_l", error_type = "classical", sigma = 0.5))
  )
  
  shadowed_data <- data
  for (shadow in shadows) {
    result <- apply_shadow(shadowed_data, shadow)
    # handle both direct data and result list
    if (is.list(result) && "data" %in% names(result)) {
      shadowed_data <- result$data
    } else {
      shadowed_data <- result
    }
  }
  
  # diagnose
  diag <- diagnose_all_shadows(data, shadowed_data, shadows)
  
  # check structure
  expect_s3_class(diag, "margot_shadow_diagnostics")
  expect_true("data_loss" %in% names(diag))
  expect_true("shadow_diagnostics" %in% names(diag))
  expect_true("variable_changes" %in% names(diag))
  
  # check data loss metrics
  if (!is.null(diag$data_loss$prop_retained)) {
    expect_true(diag$data_loss$prop_retained <= 1)
  }
  expect_equal(diag$data_loss$n_original, nrow(data))
})