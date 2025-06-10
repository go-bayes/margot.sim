library(margot.sim)

test_that("margot_process_longitudinal creates correct censoring indicators", {
  # create simple test data
  set.seed(123)
  n <- 100
  waves <- 3
  
  # simulate basic data
  sim_data <- margot_simulate(
    n = n,
    waves = waves,
    apply_process_function = FALSE  # Don't apply processing yet
  )
  
  # apply the processing function
  processed <- margot.sim:::margot_process_longitudinal(
    sim_data,
    outcome_vars = "y"
  )
  
  # check that censoring indicators were created
  expect_true("t0_not_lost_following_wave" %in% names(processed))
  expect_true("t1_not_lost_following_wave" %in% names(processed))
  expect_true("t2_not_lost_following_wave" %in% names(processed))
  
  # check that indicators are binary
  for (wave in 0:(waves-1)) {
    col_name <- paste0("t", wave, "_not_lost_following_wave")
    values <- processed[[col_name]]
    non_na_values <- values[!is.na(values)]
    expect_true(all(non_na_values %in% c(0, 1)))
  }
})

test_that("margot_process_longitudinal handles missing outcome data correctly", {
  set.seed(456)
  n <- 50
  
  # create data with some missing outcomes
  sim_data <- margot_simulate(
    n = n,
    waves = 2,
    apply_process_function = FALSE
  )
  
  # manually set ALL data at t1 to NA for some individuals to trigger censoring
  t1_cols <- grep("^t1_", names(sim_data), value = TRUE)
  for (col in t1_cols) {
    sim_data[[col]][1:10] <- NA
  }
  
  # process the data
  processed <- margot.sim:::margot_process_longitudinal(
    sim_data,
    outcome_vars = "y"
  )
  
  # check that censoring is properly applied
  # if ALL t1 data is missing, should be censored at t0
  expect_equal(processed$t0_not_lost_following_wave[1:10], rep(0, 10))
  
  # individuals with data at t1 should not be censored
  expect_equal(processed$t0_not_lost_following_wave[11:20], rep(1, 10))
})

test_that("margot_process_longitudinal handles exposure_vars parameter", {
  set.seed(789)
  n <- 30
  
  sim_data <- margot_simulate(
    n = n,
    waves = 2,
    apply_process_function = FALSE
  )
  
  # process with exposure variables specified
  processed <- margot.sim:::margot_process_longitudinal(
    sim_data,
    outcome_vars = "y",
    exposure_vars = "a"
  )
  
  # should still have censoring indicators
  expect_true("t0_not_lost_following_wave" %in% names(processed))
  expect_true("t1_not_lost_following_wave" %in% names(processed))
})

test_that("margot_process_longitudinal preserves data structure", {
  set.seed(111)
  n <- 50
  
  sim_data <- margot_simulate(
    n = n,
    waves = 2,
    apply_process_function = FALSE
  )
  
  # process the data
  processed <- margot.sim:::margot_process_longitudinal(
    sim_data,
    outcome_vars = "y"
  )
  
  # check dimensions
  expect_equal(nrow(processed), nrow(sim_data))
  
  # check that original columns are preserved
  original_cols <- names(sim_data)
  for (col in original_cols) {
    expect_true(col %in% names(processed))
  }
  
  # check that id column is preserved
  expect_equal(processed$id, sim_data$id)
})

test_that("margot_process_longitudinal creates appropriate indicators for minimal waves", {
  set.seed(222)
  n <- 25
  
  # minimal wave simulation (waves = 1 still creates t0, t1, t2)
  sim_data <- margot_simulate(
    n = n,
    waves = 1,
    apply_process_function = FALSE
  )
  
  # process the data
  processed <- margot.sim:::margot_process_longitudinal(
    sim_data,
    outcome_vars = "y"
  )
  
  # should have censoring indicators for t0 and t1
  expect_true("t0_not_lost_following_wave" %in% names(processed))
  expect_true("t1_not_lost_following_wave" %in% names(processed))
})

test_that("margot_process_longitudinal handles multiple outcomes", {
  set.seed(333)
  n <- 40
  
  # simulate with multiple outcomes
  sim_data <- margot_simulate(
    n = n,
    waves = 2,
    n_outcomes = 2,
    apply_process_function = FALSE
  )
  
  # process with multiple outcome variables
  processed <- margot.sim:::margot_process_longitudinal(
    sim_data,
    outcome_vars = c("y1", "y2")
  )
  
  # check censoring indicators exist
  expect_true("t0_not_lost_following_wave" %in% names(processed))
  expect_true("t1_not_lost_following_wave" %in% names(processed))
  
  # if ALL t1 data is missing, should be censored
  t1_cols <- grep("^t1_", names(sim_data), value = TRUE)
  for (col in t1_cols) {
    sim_data[[col]][1:5] <- NA
  }
  processed2 <- margot.sim:::margot_process_longitudinal(
    sim_data,
    outcome_vars = c("y1", "y2")
  )
  expect_equal(processed2$t0_not_lost_following_wave[1:5], rep(0, 5))
})