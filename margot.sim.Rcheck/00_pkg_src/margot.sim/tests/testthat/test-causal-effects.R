test_that("compute_causal_effects works with simple data", {
  # create simple test data with known effect
  set.seed(123)
  n <- 1000
  data <- data.frame(
    a = rbinom(n, 1, 0.5),
    y = rnorm(n, mean = 0)
  )
  # add treatment effect of 0.5
  data$y[data$a == 1] <- data$y[data$a == 1] + 0.5
  
  # compute effects
  effects <- compute_causal_effects(data, exposure = "a", outcome = "y")
  
  # check structure
  expect_type(effects, "list")
  expect_s3_class(effects, "margot_effects")
  expect_true("ate" %in% names(effects))
  expect_true("att" %in% names(effects))
  expect_true("atu" %in% names(effects))
  
  # check values are reasonable (should be close to 0.5)
  expect_true(abs(effects$ate - 0.5) < 0.1)
  
  # check sample sizes
  expect_equal(effects$n_total, n)
  expect_true(effects$n_treated > 0)
  expect_true(effects$n_control > 0)
  expect_equal(effects$n_treated + effects$n_control, n)
})

test_that("compute_causal_effects handles missing data", {
  # create data with missing values
  data <- data.frame(
    a = c(1, 0, 1, NA, 0),
    y = c(2, 1, 3, 2, NA)
  )
  
  effects <- compute_causal_effects(data, exposure = "a", outcome = "y")
  
  # should have 3 complete cases
  expect_equal(effects$n_total, 3)
  expect_equal(effects$n_treated, 2)
  expect_equal(effects$n_control, 1)
})

test_that("compute_causal_effects validates inputs", {
  data <- data.frame(a = c(1, 0), y = c(2, 1))
  
  # missing exposure
  expect_error(
    compute_causal_effects(data, exposure = "missing", outcome = "y"),
    "not found in data"
  )
  
  # missing outcome
  expect_error(
    compute_causal_effects(data, exposure = "a", outcome = "missing"),
    "not found in data"
  )
  
  # non-binary treatment
  data$a <- c(0.5, 0.7)
  expect_warning(
    compute_causal_effects(data, exposure = "a", outcome = "y"),
    "Non-binary treatment detected"
  )
})

test_that("compare_shadow_bias computes correct bias", {
  # create effects with known bias
  true_effects <- list(
    ate = 0.5,
    att = 0.6,
    atu = 0.4,
    n_treated = 500,
    n_control = 500
  )
  
  obs_effects <- list(
    ate = 0.3,  # bias = -0.2
    att = 0.35, # bias = -0.25
    atu = 0.25, # bias = -0.15
    n_treated = 450,
    n_control = 450
  )
  
  comparison <- compare_shadow_bias(true_effects, obs_effects)
  
  # check structure
  expect_s3_class(comparison, "shadow_bias_comparison")
  expect_s3_class(comparison, "data.frame")
  expect_equal(nrow(comparison), 3)
  
  # check values
  expect_equal(comparison$bias[comparison$estimand == "ate"], -0.2)
  expect_equal(comparison$bias[comparison$estimand == "att"], -0.25)
  expect_equal(comparison$bias[comparison$estimand == "atu"], -0.15)
  
  # check relative bias
  expect_true("relative_bias" %in% names(comparison))
  expect_equal(
    comparison$relative_bias[comparison$estimand == "ate"],
    -40  # -0.2 / 0.5 * 100
  )
})

test_that("print methods work correctly", {
  # test margot_effects print
  effects <- list(
    ate = 0.5,
    att = 0.6,
    atu = 0.4,
    n_treated = 500,
    n_control = 500,
    n_total = 1000
  )
  class(effects) <- c("margot_effects", "list")
  
  expect_output(print(effects), "Causal Effect Estimates")
  expect_output(print(effects), "ATE: 0.5")
  
  # test shadow_bias_comparison print
  comparison <- data.frame(
    estimand = c("ate", "att"),
    truth = c(0.5, 0.6),
    observed = c(0.3, 0.35),
    bias = c(-0.2, -0.25),
    relative_bias = c(-40, -41.7)
  )
  class(comparison) <- c("shadow_bias_comparison", "data.frame")
  
  expect_output(print(comparison), "Shadow Bias Comparison")
})