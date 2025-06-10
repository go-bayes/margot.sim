test_that("margot_simulate_causal implements dual data architecture", {
  # define simple interventions
  interventions <- list(
    never_treat = function(data, time, trt) rep(0, nrow(data)),
    always_treat = function(data, time, trt) rep(1, nrow(data))
  )
  
  # test with censoring (should create dual architecture)
  set.seed(123)
  results_censored <- margot_simulate_causal(
    n = 100,
    waves = 3,
    treatments = "a",
    interventions = interventions,
    apply_censoring = TRUE,
    common_params = list(
      params = list(a_lag_y_coef = 0.3),
      verbose = FALSE
    )
  )
  
  # check dual data structure exists when censoring applied
  expect_true("data_true" %in% names(results_censored))
  expect_true("data_observed" %in% names(results_censored))
  expect_equal(length(results_censored$data_true), 2)  # two interventions
  expect_equal(names(results_censored$data_true), c("never_treat", "always_treat"))
  
  # test without censoring (should not create dual architecture)
  results_no_censor <- margot_simulate_causal(
    n = 100,
    waves = 3,
    treatments = "a",
    interventions = interventions,
    apply_censoring = FALSE,
    common_params = list(
      params = list(a_lag_y_coef = 0.3),
      verbose = FALSE
    )
  )
  
  # should not have dual data when no censoring
  expect_false("data_true" %in% names(results_no_censor))
  expect_false("data_observed" %in% names(results_no_censor))
})

test_that("compute_effects_from_sim works with simulation data", {
  # simulate simple data
  set.seed(456)
  sim_data <- margot_simulate(
    n = 200,
    waves = 3,
    params = list(a_y_coef = 0.5)
  )
  
  # compute effects
  effects <- compute_effects_from_sim(
    sim_data,
    wave = 1,
    outcome_wave = 3,
    treatment_name = "a",
    outcome_name = "y"
  )
  
  # check structure
  expect_s3_class(effects, "margot_effects")
  expect_true("ate" %in% names(effects))
  expect_true(is.numeric(effects$ate))
  expect_equal(effects$n_total, 200)
})

test_that("extract_treatment_outcome handles time indexing correctly", {
  # create mock data with time prefixes
  mock_data <- data.frame(
    id = 1:5,
    t1_a = c(0, 1, 0, 1, 1),
    t2_a = c(0, 0, 1, 1, 0),
    t3_y = c(1.5, 2.0, 1.0, 2.5, 2.2)
  )
  
  # extract wave 1 treatment and wave 3 outcome
  extracted <- extract_treatment_outcome(
    mock_data,
    wave = 1,
    outcome_wave = 3,
    treatment_name = "a",
    outcome_name = "y"
  )
  
  expect_equal(names(extracted), c("a", "y", "id"))
  expect_equal(extracted$a, mock_data$t1_a)
  expect_equal(extracted$y, mock_data$t3_y)
  expect_equal(extracted$id, mock_data$id)
  
  # test automatic outcome wave detection
  extracted_auto <- extract_treatment_outcome(
    mock_data,
    wave = 1,
    treatment_name = "a",
    outcome_name = "y"
  )
  
  # should use wave 3 (highest) for outcome
  expect_equal(extracted_auto$y, mock_data$t3_y)
})