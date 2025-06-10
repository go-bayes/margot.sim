library(margot.sim)

test_that("margot_simulate_causal creates data under different interventions", {
  set.seed(123)
  
  # define simple interventions
  interventions <- list(
    never_treat = function(data, time, trt) rep(0, nrow(data)),
    always_treat = function(data, time, trt) rep(1, nrow(data))
  )
  
  # simulate causal data
  result <- margot_simulate_causal(
    n = 100,
    waves = 2,
    treatments = "a",
    interventions = interventions,
    apply_censoring = FALSE
  )
  
  # check structure
  expect_s3_class(result, "margot_causal_sim")
  expect_true(all(c("data", "effects", "metadata") %in% names(result)))
  
  # check that we have data for each intervention
  expect_equal(names(result$data), c("never_treat", "always_treat"))
  
  # check data dimensions
  expect_equal(nrow(result$data$never_treat), 100)
  expect_equal(nrow(result$data$always_treat), 100)
  
  # check that interventions were applied correctly
  # for never_treat, all treatment should be 0
  treat_cols <- grep("^t[0-9]+_a$", names(result$data$never_treat), value = TRUE)
  for (col in treat_cols) {
    expect_true(all(result$data$never_treat[[col]] == 0))
  }
  
  # for always_treat, all treatment should be 1
  for (col in treat_cols) {
    expect_true(all(result$data$always_treat[[col]] == 1))
  }
})

test_that("margot_simulate_causal computes causal effects", {
  set.seed(456)
  
  interventions <- list(
    control = function(data, time, trt) rep(0, nrow(data)),
    treat = function(data, time, trt) rep(1, nrow(data))
  )
  
  result <- margot_simulate_causal(
    n = 200,
    waves = 2,
    treatments = "a",
    interventions = interventions,
    apply_censoring = FALSE
  )
  
  # check that effects were computed
  expect_true("effects" %in% names(result))
  expect_true(is.data.frame(result$effects))
  
  # check effect structure
  expect_true("contrast" %in% names(result$effects))
  expect_true("estimand" %in% names(result$effects))
  expect_true("estimate" %in% names(result$effects))
  expect_true("se" %in% names(result$effects))
})

test_that("margot_simulate_causal handles censoring", {
  set.seed(789)
  
  interventions <- list(
    natural = function(data, time, trt) data[[trt]]
  )
  
  # simulate with censoring
  result <- margot_simulate_causal(
    n = 150,
    waves = 3,
    treatments = "a",
    interventions = interventions,
    apply_censoring = TRUE,
    common_params = list(
      censoring_params = list(
        pr_censor_baseline = 0.1,
        pr_censor_wave = 0.1
      )
    )
  )
  
  # check that censoring was applied
  expect_true("censoring_bias" %in% names(result))
  
  # when censoring is applied, data structure changes
  expect_true(is.list(result$data$natural))
  expect_true(all(c("complete", "observed") %in% names(result$data$natural)))
  
  # check for censoring indicators in observed data
  censoring_cols <- grep("not_lost_following_wave", names(result$data$natural$observed), value = TRUE)
  expect_true(length(censoring_cols) > 0)
  
  # some individuals should be censored
  for (col in censoring_cols) {
    expect_true(any(result$data$natural$observed[[col]] == 0))
  }
})

test_that("margot_simulate_causal handles sampling weights", {
  set.seed(111)
  
  # create simple weight function
  weight_fn <- function(baseline_data) {
    # weight based on first baseline covariate
    weights <- ifelse(baseline_data$b1 > 0, 2, 1)
    weights / mean(weights)  # normalize
  }
  
  interventions <- list(
    natural = function(data, time, trt) data[[trt]]
  )
  
  result <- margot_simulate_causal(
    n = 100,
    waves = 2,
    treatments = "a",
    interventions = interventions,
    sampling_weights = weight_fn,
    apply_censoring = FALSE
  )
  
  # check that simulation completed
  expect_s3_class(result, "margot_causal_sim")
  expect_equal(nrow(result$data$natural), 100)
})

test_that("margot_simulate_causal validates inputs", {
  # interventions must be provided
  expect_error(
    margot_simulate_causal(
      n = 50,
      waves = 1,
      treatments = "a",
      apply_censoring = FALSE
    ),
    "missing"
  )
  
  # interventions must be a list
  expect_error(
    margot_simulate_causal(
      n = 50,
      waves = 1,
      treatments = "a",
      interventions = "not a list",
      apply_censoring = FALSE
    ),
    "must be a named list"
  )
  
  # interventions must be named
  expect_error(
    margot_simulate_causal(
      n = 50,
      waves = 1,
      treatments = "a",
      interventions = list(function(data, time, trt) rep(0, nrow(data))),
      apply_censoring = FALSE
    ),
    "must be named"
  )
})

test_that("margot_simulate_causal print method works", {
  set.seed(222)
  
  interventions <- list(
    never = function(data, time, trt) rep(0, nrow(data)),
    always = function(data, time, trt) rep(1, nrow(data))
  )
  
  result <- margot_simulate_causal(
    n = 75,
    waves = 2,
    treatments = "a",
    interventions = interventions,
    apply_censoring = TRUE
  )
  
  # capture print output
  output <- capture.output(print(result))
  
  # check that output contains expected information
  expect_true(any(grepl("margot causal simulation results", output)))
  expect_true(any(grepl("sample size:", output)))
  expect_true(any(grepl("interventions:", output)))
  expect_true(any(grepl("never", output)))
  expect_true(any(grepl("always", output)))
})